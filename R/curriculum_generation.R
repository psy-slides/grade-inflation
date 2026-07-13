# Generation of the fictional degree programmes (CdS) and their exams.
# All names come from clearly fictional pools defined in config/defaults.yml.

#' Generate the courses (degree programmes) table.
#'
#' @param config scenario_config.
#' @param app_config full app configuration.
#' @return tibble: course_id, course_name, n_students.
generate_courses <- function(config, app_config = load_app_config()) {
  k <- config$n_courses
  custom_n <- parse_students_per_course(config$students_per_course_custom, k)
  n_students <- if (is.null(custom_n) || inherits(custom_n, "invalid_input")) {
    rep(config$n_students_per_course, k)
  } else {
    custom_n
  }
  tibble::tibble(
    course_id = paste0("C", seq_len(k)),
    course_name = app_config$course_name_pool[seq_len(k)],
    n_students = as.integer(n_students)
  )
}

#' Generate the exams table for all courses.
#'
#' Each course has `n_mandatory` mandatory exams plus `n_optional_groups` choice
#' groups of `n_alternatives_per_group` alternatives each (a student takes exactly
#' one exam per group). Exam generosity differs only through delta_j ~ N(0, tau^2),
#' centered so that mean(delta) ~ 0 across all exams; derived difficulty = -delta
#' (higher = harder). Discrimination (lambda) and residual sd are common to all
#' exams by design.
#'
#' @return tibble: exam_id, exam_name, course_id, mandatory, choice_group, delta,
#'   difficulty.
generate_exams <- function(config, courses, app_config = load_app_config()) {
  name_pool <- app_config$exam_name_pool
  suffixes <- app_config$optional_name_suffixes
  n_names_needed <- config$n_mandatory + config$n_optional_groups
  stopifnot(n_names_needed <= length(name_pool))

  rows <- lapply(seq_len(nrow(courses)), function(ci) {
    course_id <- courses$course_id[ci]
    base_names <- sample(name_pool, n_names_needed)
    mandatory <- tibble::tibble(
      exam_name = base_names[seq_len(config$n_mandatory)],
      course_id = course_id,
      mandatory = TRUE,
      choice_group = NA_character_
    )
    optional <- NULL
    if (config$n_optional_groups > 0) {
      optional <- do.call(rbind, lapply(seq_len(config$n_optional_groups), function(g) {
        group_name <- base_names[config$n_mandatory + g]
        tibble::tibble(
          exam_name = paste(group_name, suffixes[seq_len(config$n_alternatives_per_group)]),
          course_id = course_id,
          mandatory = FALSE,
          choice_group = paste0(course_id, "-G", g)
        )
      }))
    }
    rbind(mandatory, optional)
  })
  exams <- do.call(rbind, rows)
  exams$exam_id <- sprintf("E%02d", seq_len(nrow(exams)))

  # Exam-level generosity: delta_j ~ N(0, tau^2), centered overall so that the
  # target mean calibration acts on base_location only.
  tau <- config$exam_heterogeneity
  delta <- rnorm(nrow(exams), 0, tau)
  if (tau > 0) delta <- delta - mean(delta)
  exams$delta <- delta
  exams$difficulty <- -delta

  exams[, c("exam_id", "exam_name", "course_id", "mandatory", "choice_group",
            "delta", "difficulty")]
}
