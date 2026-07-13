# Validation of scenario configurations, independent from Shiny (usable in tests).
# The Shiny layer translates errors into validate()/need() messages in Italian.

#' Validate a scenario_config against the limits in config/defaults.yml.
#'
#' @return list(valid = logical, errors = character, warnings = character).
validate_scenario_config <- function(config, app_config = load_app_config()) {
  limits <- app_config$limits
  errors <- character(0)
  warnings <- character(0)

  in_range <- function(x, lim) is.numeric(x) && length(x) == 1 && !is.na(x) &&
    x >= lim[[1]] && x <= lim[[2]]
  check_range <- function(value, key, label) {
    if (!in_range(value, limits[[key]])) {
      errors <<- c(errors, sprintf("%s deve essere tra %s e %s.",
                                   label, limits[[key]][[1]], limits[[key]][[2]]))
    }
  }

  check_range(config$n_courses, "n_courses", "Il numero di corsi di studio")
  check_range(config$n_mandatory, "n_mandatory", "Il numero di esami obbligatori")
  check_range(config$n_optional_groups, "n_optional_groups",
              "Il numero di gruppi opzionali")
  if (config$n_optional_groups > 0) {
    check_range(config$n_alternatives_per_group, "n_alternatives_per_group",
                "Il numero di alternative per gruppo")
  }
  check_range(config$target_mean_grade, "target_mean_grade", "Il voto medio target")
  check_range(config$exam_heterogeneity, "exam_heterogeneity",
              "L'eterogeneita' tra esami")
  check_range(config$strategic_strength, "strategic_strength",
              "La forza della scelta strategica")
  check_range(config$hard_exam_preference, "hard_exam_preference",
              "La preferenza per gli esami difficili")
  check_range(config$softmax_temperature, "softmax_temperature",
              "La temperatura della softmax")

  if (config$n_mandatory < 1) {
    errors <- c(errors, "Serve almeno un esame obbligatorio.")
  }
  n_exams <- config$n_mandatory +
    config$n_optional_groups * config$n_alternatives_per_group
  if (!in_range(n_exams, limits$n_exams_per_course)) {
    errors <- c(errors, sprintf(
      "Il totale di esami per CdS (%d) deve restare tra %d e %d.",
      n_exams, limits$n_exams_per_course[[1]], limits$n_exams_per_course[[2]]))
  }

  if (!is.numeric(config$seed) || length(config$seed) != 1 || is.na(config$seed) ||
      config$seed != round(config$seed)) {
    errors <- c(errors, "Il seed deve essere un numero intero.")
  }
  if (!config$reliability_level %in% names(app_config$reliability_levels)) {
    errors <- c(errors, "Livello di affidabilita' non riconosciuto.")
  }
  if (!config$choice_mode %in% c("casuale", "strategica")) {
    errors <- c(errors, "Modalita' di scelta degli opzionali non riconosciuta.")
  }
  if (!config$percentile_method %in% c("rango_medio", "conservativo")) {
    errors <- c(errors, "Metodo percentile non riconosciuto.")
  }

  # Per-course N: global value or custom comma-separated vector.
  custom <- parse_students_per_course(config$students_per_course_custom,
                                      config$n_courses)
  if (inherits(custom, "invalid_input")) {
    errors <- c(errors, sprintf(
      "N per CdS personalizzati non validi: servono %d interi positivi separati da virgola.",
      config$n_courses))
  } else {
    n_vec <- if (is.null(custom)) rep(config$n_students_per_course, config$n_courses)
             else custom
    if (any(!vapply(n_vec, in_range, logical(1), lim = limits$n_students_per_course))) {
      errors <- c(errors, sprintf(
        "Ogni N per CdS deve essere tra %d e %d.",
        limits$n_students_per_course[[1]], limits$n_students_per_course[[2]]))
    } else if (!in_range(sum(n_vec), limits$n_students_total)) {
      errors <- c(errors, sprintf(
        "Il totale di studenti (%d) deve restare tra %d e %d.",
        sum(n_vec), limits$n_students_total[[1]], limits$n_students_total[[2]]))
    }
    if (length(errors) == 0 && config$n_optional_groups > 0) {
      min_n <- min(n_vec)
      expected_per_alt <- min_n / config$n_alternatives_per_group
      if (expected_per_alt < app_config$stability_thresholds$fragile_below) {
        warnings <- c(warnings, paste(
          "Con questi N gli esami opzionali avranno riferimenti percentili",
          "potenzialmente fragili (poche decine di studenti per alternativa)."))
      }
    }
  }

  if (!in_range(config$max_attempts, limits$max_attempts)) {
    errors <- c(errors, "Il numero massimo di tentativi non e' plausibile.")
  }

  list(valid = length(errors) == 0, errors = errors, warnings = warnings)
}
