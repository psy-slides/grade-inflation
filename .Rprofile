# Attiva renv solo se l'infrastruttura locale esiste (renv::restore() o renv::init()
# la creano); in sua assenza l'app usa la libreria utente standard.
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}
options(
  shiny.autoreload = FALSE,
  stringsAsFactors = FALSE
)
