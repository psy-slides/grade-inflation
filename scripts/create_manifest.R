# Genera il manifest.json per il deploy (Posit Connect / shinyapps.io).
# Esecuzione: Rscript scripts/create_manifest.R (dalla radice della repository).
stopifnot(file.exists("app.R"))

rsconnect::writeManifest(
  appDir = ".",
  appFiles = c(
    "app.R",
    list.files("R", recursive = TRUE, full.names = TRUE),
    list.files("config", full.names = TRUE),
    list.files("inst", recursive = TRUE, full.names = TRUE)
  ),
  appPrimaryDoc = "app.R"
)
message("manifest.json creato nella radice della repository.")
