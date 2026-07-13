# Avvia l'app dalla radice della repository:
#   Rscript scripts/run_app.R [porta]
args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[[1]]) else 4321L

app_dir <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=",
  commandArgs(trailingOnly = FALSE), value = TRUE))), ".."))
setwd(app_dir)

shiny::runApp(app_dir, port = port, launch.browser = interactive(),
              host = "127.0.0.1")
