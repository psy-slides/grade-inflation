# Scenario assembly: from a scenario_config to the full `grade_scenario` object.
# This is the single entry point used by the Shiny layer (via eventReactive) and by
# tests/scripts; it contains no reactive code.

#' Build a complete simulated scenario.
#'
#' @param config scenario_config from make_scenario_config().
#' @param progress optional function(message, fraction) for UI progress feedback.
#' @return object of class grade_scenario: list(config, courses, exams, students,
#'   enrollments, grades, exam_statistics, percentile_scores, student_scores,
#'   recovery, metadata).
build_scenario <- function(config, app_config = load_app_config(), progress = NULL) {
  notify <- function(msg, frac) if (!is.null(progress)) progress(msg, frac)
  check <- validate_scenario_config(config, app_config)
  if (!check$valid) {
    stop("Configurazione non valida: ", paste(check$errors, collapse = " | "))
  }

  set.seed(config$seed)
  model <- app_config$model
  sigma_eps <- reliability_to_sigma(config$reliability_level, app_config)

  notify("Generazione di corsi ed esami", 0.1)
  courses <- generate_courses(config, app_config)
  exams <- generate_exams(config, courses, app_config)

  notify("Calibrazione della media", 0.25)
  calibration <- calibrate_base_location(exams, config, sigma_eps, app_config)
  exams$expected_grade <- expected_exam_means(
    calibration$base_location, exams$delta, sigma_eps, model$lambda,
    model$grade_min, model$grade_max, app_config$calibration$n_theta_grid
  )

  notify("Generazione degli studenti e scelta degli opzionali", 0.4)
  students <- generate_students(courses)
  enrollments <- assign_exam_paths(students, exams, config)

  notify("Simulazione dei voti", 0.6)
  grades <- simulate_grades(enrollments, students, exams,
                            calibration$base_location, sigma_eps,
                            config$max_attempts, app_config)
  n_failed <- sum(grades$failed)
  grades_ok <- grades[!grades$failed, ]

  notify("Percentili e aggregazione", 0.8)
  exam_statistics <- compute_exam_statistics(grades_ok, exams, app_config)
  percentile_scores <- compute_percentile_scores(grades_ok, config$percentile_method)
  student_scores <- aggregate_student_scores(percentile_scores, grades_ok)
  student_scores <- add_career_stability(student_scores, percentile_scores,
                                         exam_statistics)

  notify("Metriche di recovery", 0.95)
  recovery <- compute_recovery(student_scores, students, enrollments, exams)

  warnings <- check$warnings
  if (!calibration$converged) {
    warnings <- c(warnings, calibration$message %||% sprintf(
      "Calibrazione fuori tolleranza: media attesa %.2f contro target %.2f.",
      calibration$expected_mean, config$target_mean_grade))
  }
  if (n_failed > 0) {
    warnings <- c(warnings, sprintf(
      "%d prove hanno superato il limite tecnico di tentativi e sono escluse.", n_failed))
  }
  unobserved <- exam_statistics$exam_id[exam_statistics$n == 0]
  if (length(unobserved) > 0) {
    warnings <- c(warnings, sprintf(
      "Esami senza iscritti (nessun percentile calcolato): %s.",
      paste(unobserved, collapse = ", ")))
  }

  structure(list(
    config = config,
    courses = courses,
    exams = exams,
    students = students,
    enrollments = enrollments,
    grades = grades,
    exam_statistics = exam_statistics,
    percentile_scores = percentile_scores,
    student_scores = student_scores,
    recovery = recovery,
    metadata = list(
      seed = config$seed,
      created_at = Sys.time(),
      base_location = calibration$base_location,
      expected_mean = calibration$expected_mean,
      calibration_converged = calibration$converged,
      calibration_tolerance = app_config$calibration$tolerance,
      observed_mean = mean(grades_ok$grade),
      sigma_eps = sigma_eps,
      lambda = model$lambda,
      reliability_value = app_config$reliability_levels[[config$reliability_level]],
      n_failed_grades = n_failed,
      warnings = warnings
    )
  ), class = "grade_scenario")
}

#' Long table of percentile scores (one row per student x exam actually taken).
compute_percentile_scores <- function(grades_ok, method = "rango_medio") {
  scored <- grades_ok |>
    dplyr::group_by(exam_id) |>
    dplyr::group_modify(function(g, key) dplyr::bind_cols(g, percentile_table(g$grade))) |>
    dplyr::ungroup()
  raw <- select_percentile_method(scored, method)
  scored$p_used <- clip_percentile(raw, scored$n_ref)
  scored$z <- qnorm(scored$p_used)
  scored[, c("student_id", "exam_id", "grade", "p_mid", "p_lower", "p_upper",
             "p_conservative", "p_used", "z", "n_ties", "n_ref")]
}

#' Descriptive statistics and diagnostic flags for each exam (including exams with
#' zero takers, kept as "non osservato" instead of failing).
compute_exam_statistics <- function(grades_ok, exams, app_config = load_app_config()) {
  grade_max <- app_config$model$grade_max
  by_exam <- grades_ok |>
    dplyr::group_by(exam_id) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_grade = mean(grade),
      median_grade = stats::median(grade),
      sd_grade = stats::sd(grade),
      share_top = mean(grade == grade_max),
      n_distinct = dplyr::n_distinct(grade),
      modal_share = max(table(grade)) / dplyr::n(),
      mean_attempts = mean(n_attempts),
      .groups = "drop"
    )
  stats_tbl <- dplyr::left_join(exams, by_exam, by = "exam_id")
  stats_tbl$n[is.na(stats_tbl$n)] <- 0L
  stats_tbl$stability <- classify_stability(stats_tbl$n, app_config)
  stats_tbl$resolution <- classify_resolution(
    stats_tbl$n_distinct, stats_tbl$modal_share, stats_tbl$share_top, app_config
  )
  stats_tbl$resolution[stats_tbl$n == 0] <- NA
  stats_tbl
}

#' Stability of the percentile reference, driven by the effective N of the exam.
#' Levels are ordered: fragile < moderata < buona.
classify_stability <- function(n, app_config = load_app_config()) {
  th <- app_config$stability_thresholds
  out <- ifelse(n == 0, NA_character_,
         ifelse(n < th$fragile_below, "fragile",
         ifelse(n < th$good_at_least, "moderata", "buona")))
  factor(out, levels = c("fragile", "moderata", "buona"), ordered = TRUE)
}

#' Effective resolution of the observed distribution (discriminative capacity):
#' distinct from sampling stability. An exam can have large N and poor resolution
#' (almost everyone at 30) or small N and decent dispersion.
classify_resolution <- function(n_distinct, modal_share, share_top,
                                app_config = load_app_config()) {
  th <- app_config$resolution_thresholds
  poor <- (n_distinct <= th$distinct_poor_max) |
    (modal_share >= th$modal_share_poor) | (share_top >= th$top_share_poor)
  medium <- (n_distinct <= th$distinct_medium_max) |
    (modal_share >= th$modal_share_medium)
  out <- ifelse(poor, "scarsa", ifelse(medium, "media", "buona"))
  factor(out, levels = c("scarsa", "media", "buona"), ordered = TRUE)
}

#' Career-level synthetic stability flag: the WORST stability level among the
#' exams the student actually took (documented conservative rule).
add_career_stability <- function(student_scores, percentile_scores, exam_statistics) {
  levels_map <- exam_statistics[, c("exam_id", "stability")]
  per_student <- dplyr::inner_join(percentile_scores[, c("student_id", "exam_id")],
                                   levels_map, by = "exam_id") |>
    dplyr::group_by(student_id) |>
    dplyr::summarise(career_stability = min(stability), .groups = "drop")
  dplyr::left_join(student_scores, per_student, by = "student_id")
}
