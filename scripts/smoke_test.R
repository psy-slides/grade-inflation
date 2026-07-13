# Smoke test end-to-end del motore + costruzione dell'oggetto Shiny.
# Esecuzione: Rscript scripts/smoke_test.R (dalla radice della repository).

message("== Smoke test: laboratorio voti e percentili ==")
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(DT)
})

stopifnot(file.exists("app.R"))
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
for (f in list.files(file.path("R", "modules"), pattern = "\\.R$",
                     full.names = TRUE)) source(f)

fail <- function(...) stop(sprintf(...), call. = FALSE)
check <- function(condition, description) {
  if (!isTRUE(condition)) fail("FALLITO: %s", description)
  message("  OK: ", description)
}

# 1. Scenario di default -------------------------------------------------------
config <- make_scenario_config()
t0 <- Sys.time()
scenario <- build_scenario(config)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
message(sprintf("Scenario di default costruito in %.2f s", elapsed))

check(inherits(scenario, "grade_scenario"), "classe grade_scenario")
check(config$reliability_level == "media", "default affidabilita' = media")
check(config$choice_mode == "casuale", "default scelta opzionali = casuale")
grades <- scenario$grades$grade[!scenario$grades$failed]
check(all(grades >= 18 & grades <= 30), "voti osservati in [18, 30]")
check(abs(scenario$metadata$observed_mean - 27.5) < 0.3,
      "media osservata vicina al target 27.5")
check(all(is.finite(scenario$percentile_scores$z)), "normal score tutti finiti")
check(all(scenario$student_scores$decile %in% 1:10), "decili in 1..10")
check(all(is.finite(scenario$recovery$overall$pearson)),
      "correlazioni di recovery finite")

# 2. Scenario con scelta strategica -------------------------------------------
strategic <- build_scenario(make_scenario_config(
  choice_mode = "strategica", strategic_strength = 1.5,
  hard_exam_preference = 1, seed = 42))
optional_n <- strategic$exam_statistics$n[!strategic$exam_statistics$mandatory]
check(length(optional_n) > 0 && sum(optional_n) > 0, "scenario strategico simulato")

# 3. Grafici principali --------------------------------------------------------
p1 <- plot_grade_distribution(grades)
p2 <- plot_exam_means(scenario$exam_statistics)
df_rec <- dplyr::inner_join(scenario$student_scores,
                            scenario$recovery$theta_std, by = "student_id")
p3 <- plot_recovery_scatter(df_rec, "estimate_from_percentiles",
                            "Percentili aggregati (normal score)")
tmp <- tempfile(fileext = ".png")
save_plot_png(tmp, p1)
check(file.exists(tmp) && file.size(tmp) > 0, "salvataggio PNG dei grafici")
unlink(tmp)
invisible(ggplot2::ggplot_build(p2))
invisible(ggplot2::ggplot_build(p3))
message("  OK: costruzione dei grafici ggplot")

# 4. Oggetto Shiny -------------------------------------------------------------
app <- shiny::shinyAppFile("app.R")
check(inherits(app, "shiny.appobj"), "app.R produce un oggetto shiny.appobj")

message("== Smoke test completato senza errori ==")
