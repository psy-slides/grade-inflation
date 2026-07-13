# Loading and handling of the centralized configuration (config/defaults.yml).
# The YAML file is the single source of truth for defaults, reliability bands,
# stability/resolution thresholds and validation limits.

#' Locate the configuration file robustly (app root, tests, deployed app).
find_config_file <- function() {
  candidates <- c(
    "config/defaults.yml",
    file.path("..", "..", "config", "defaults.yml"),  # from tests/testthat
    file.path("..", "config", "defaults.yml")
  )
  for (path in candidates) {
    if (file.exists(path)) return(normalizePath(path))
  }
  stop("File di configurazione 'config/defaults.yml' non trovato.")
}

#' Load the full app configuration (cached per session).
load_app_config <- local({
  cache <- NULL
  function(path = NULL) {
    if (is.null(path) && !is.null(cache)) return(cache)
    cfg <- yaml::read_yaml(if (is.null(path)) find_config_file() else path)
    stopifnot(
      is.list(cfg$defaults), is.list(cfg$reliability_levels),
      is.list(cfg$model), is.list(cfg$limits)
    )
    if (is.null(path)) cache <<- cfg
    cfg
  }
})

#' Scenario configuration: app defaults overridden by user parameters.
#' Returns a plain named list ("scenario_config") consumed by build_scenario().
make_scenario_config <- function(..., app_config = load_app_config()) {
  overrides <- list(...)
  config <- app_config$defaults
  unknown <- setdiff(names(overrides), names(config))
  if (length(unknown) > 0) {
    stop("Parametri di scenario sconosciuti: ", paste(unknown, collapse = ", "))
  }
  config[names(overrides)] <- overrides
  config$n_exams_per_course <- config$n_mandatory +
    config$n_optional_groups * config$n_alternatives_per_group
  class(config) <- c("scenario_config", "list")
  config
}

#' Map the reliability band (bassa/media/alta) to the residual sd sigma_eps.
#' reliability = lambda^2 / (lambda^2 + sigma^2)  =>  sigma = lambda * sqrt((1-r)/r).
reliability_to_sigma <- function(level, app_config = load_app_config()) {
  levels_map <- app_config$reliability_levels
  if (!level %in% names(levels_map)) {
    stop("Livello di affidabilita' sconosciuto: ", level)
  }
  rel <- levels_map[[level]]
  lambda <- app_config$model$lambda
  lambda * sqrt((1 - rel) / rel)
}

#' Parse the optional "N per CdS" free-text override (e.g. "200,150,300").
#' Returns an integer vector of length n_courses, or NULL if the field is empty.
parse_students_per_course <- function(text, n_courses) {
  text <- trimws(text %||% "")
  if (identical(text, "")) return(NULL)
  parts <- suppressWarnings(as.integer(trimws(strsplit(text, "[,;]")[[1]])))
  if (anyNA(parts) || length(parts) != n_courses || any(parts <= 0)) {
    return(structure(NA, class = "invalid_input"))
  }
  parts
}

`%||%` <- function(a, b) if (is.null(a)) b else a
