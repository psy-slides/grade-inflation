# Laboratorio esplorativo su voti universitari, grade inflation e percentili.
# Tutti i dati sono simulati. Avvio: shiny::runApp() oppure Rscript scripts/run_app.R

library(shiny)
library(bslib)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(DT)
})

# Motore statistico puro (R/) e moduli Shiny (R/modules/).
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
for (f in list.files(file.path("R", "modules"), pattern = "\\.R$",
                     full.names = TRUE)) source(f)

shiny::addResourcePath("assets", file.path("inst", "app", "www"))

app_theme <- bslib::bs_theme(
  version = 5,
  bg = "#f9f9f7",
  fg = "#0b0b0b",
  primary = "#2a78d6",
  secondary = "#52514e",
  success = "#0ca30c",
  warning = "#fab219",
  danger = "#d03b3b",
  "font-size-base" = "0.95rem"
)

ui <- bslib::page_navbar(
  title = "Laboratorio voti e percentili",
  window_title = "Laboratorio voti e percentili — dati simulati",
  theme = app_theme,
  fillable = FALSE,
  header = shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", href = "assets/custom.css")
  ),
  bslib::nav_panel("Configura lo scenario", mod_scenario_ui("scenario")),
  bslib::nav_panel("Quadro generale", mod_overview_ui("overview")),
  bslib::nav_panel("Distribuzioni degli esami", mod_distributions_ui("distributions")),
  bslib::nav_panel("Studente ipotetico", mod_hypothetical_student_ui("hypothetical")),
  bslib::nav_panel("Studente simulato", mod_sampled_student_ui("sampled")),
  bslib::nav_panel("Recovery complessivo", mod_recovery_ui("recovery")),
  bslib::nav_panel("Metodi e assunzioni", mod_methods_ui("methods")),
  bslib::nav_spacer(),
  bslib::nav_item(shiny::tags$span(class = "navbar-text small text-muted pe-2",
                                   "Dati interamente simulati"))
)

server <- function(input, output, session) {
  scenario_module <- mod_scenario_server("scenario")
  scenario <- scenario_module$scenario
  mod_overview_server("overview", scenario)
  mod_distributions_server("distributions", scenario)
  mod_hypothetical_student_server("hypothetical", scenario)
  mod_sampled_student_server("sampled", scenario)
  mod_recovery_server("recovery", scenario)
  mod_methods_server("methods")
}

shiny::shinyApp(ui, server)
