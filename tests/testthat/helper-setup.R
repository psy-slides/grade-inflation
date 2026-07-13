# Shared test setup: sources the pure statistical engine (no Shiny required).
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

engine_dir <- file.path("..", "..", "R")
for (f in list.files(engine_dir, pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# Small, fast scenario used across tests.
small_config <- function(...) {
  make_scenario_config(
    n_courses = 2, n_mandatory = 3, n_optional_groups = 1,
    n_alternatives_per_group = 2, n_students_per_course = 60,
    seed = 1234, ...
  )
}
