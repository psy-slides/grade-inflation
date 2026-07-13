# Reusable UI fragments: badges, mini histograms, notes and tooltips.
# Diagnostic levels always combine color + icon + text (never color alone).

#' Colored badge with icon and label for a stability/resolution level.
stability_badge <- function(level, tooltip = NULL) {
  css_class <- switch(as.character(level),
    fragile = "badge-fragile", moderata = "badge-moderata",
    buona = "badge-buona", "badge-missing")
  badge <- htmltools::span(
    class = paste("app-badge", css_class),
    paste(stability_icon(level), stability_label(level))
  )
  if (is.null(tooltip)) return(badge)
  bslib::tooltip(badge, tooltip)
}

#' Inline mini histogram (pure HTML/CSS) of a grade distribution 18-30.
#' Lightweight alternative to one ggplot per exam in the hypothetical-student tab.
mini_grade_histogram <- function(grades, grade_min = 18, grade_max = 30,
                                 highlight = NULL) {
  counts <- grade_count_table(grades, grade_min, grade_max)
  max_n <- max(counts$n, 1)
  bars <- lapply(seq_len(nrow(counts)), function(i) {
    height <- round(100 * counts$n[i] / max_n)
    is_highlight <- !is.null(highlight) && counts$grade[i] == highlight
    htmltools::div(
      class = paste("mini-bar", if (is_highlight) "mini-bar-highlight"),
      style = sprintf("height:%d%%;", max(height, 2)),
      title = sprintf("Voto %d: %d studenti", counts$grade[i], counts$n[i])
    )
  })
  htmltools::div(
    class = "mini-histogram",
    htmltools::div(class = "mini-histogram-bars", bars),
    htmltools::div(class = "mini-histogram-axis",
                   htmltools::span("18"), htmltools::span("30"))
  )
}

#' Prominent note reminding that all data are simulated.
simulated_data_note <- function() {
  htmltools::div(
    class = "alert alert-info app-disclaimer", role = "note",
    htmltools::HTML(paste(
      "<strong>Tutti i dati sono simulati.</strong>",
      "L'app non usa dati reali, non riproduce corsi di studio esistenti e non",
      "costituisce uno strumento validato o immediatamente implementabile."
    ))
  )
}

#' Note for the hypothetical student (no ground truth available).
hypothetical_note <- function() {
  htmltools::div(
    class = "alert alert-warning", role = "note",
    htmltools::HTML(paste(
      "<strong>Questo e' uno studente ipotetico costruito dall'utente.</strong>",
      "L'app puo' collocarlo nelle distribuzioni simulate, ma non conosce una sua",
      "abilita' vera: nessun confronto con theta e' possibile, e il percentile non",
      "va letto come la sua \"vera preparazione\"."
    ))
  )
}

#' Small helper: label with an information tooltip.
label_with_info <- function(label, info) {
  htmltools::span(
    label, " ",
    bslib::tooltip(
      htmltools::span(class = "info-icon", htmltools::HTML("&#9432;")),
      info
    )
  )
}

#' Standard header block for tabs: title + short descriptive text.
tab_header <- function(title, description) {
  htmltools::div(
    class = "tab-header",
    htmltools::h4(title),
    htmltools::p(class = "text-muted", description)
  )
}

#' Explicit message shown when the percentile reference of an exam is missing.
unobserved_exam_message <- function(exam_name) {
  htmltools::div(
    class = "alert alert-secondary",
    sprintf(paste(
      "L'esame \"%s\" non e' stato scelto da nessuno studente simulato:",
      "nessun percentile puo' essere calcolato su questo riferimento."), exam_name)
  )
}

#' Prepend metadata comment lines (seed, date, disclaimer) to exported CSV files.
#' Readable with read.csv(..., comment.char = "#").
write_csv_with_header <- function(df, file, scenario) {
  header <- c(
    "# Dati simulati - laboratorio esplorativo voti e percentili",
    sprintf("# Generato il: %s", format(scenario$metadata$created_at,
                                        "%Y-%m-%d %H:%M:%S")),
    sprintf("# Seed: %s", scenario$metadata$seed),
    sprintf("# Configurazione: %s", jsonlite::toJSON(
      unclass(scenario$config), auto_unbox = TRUE)),
    "# Nessun dato reale: non interpretare come misure di studenti esistenti."
  )
  writeLines(header, file)
  suppressWarnings(utils::write.table(
    df, file, append = TRUE, sep = ",", row.names = FALSE, qmethod = "double"
  ))
}
