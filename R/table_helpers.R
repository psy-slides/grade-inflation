# DT helpers with Italian localisation and consistent, restrained formatting.

dt_italian_language <- list(
  search = "Cerca:",
  lengthMenu = "Mostra _MENU_ righe",
  info = "Righe _START_-_END_ di _TOTAL_",
  infoEmpty = "Nessuna riga",
  infoFiltered = "(filtrate da _MAX_ righe)",
  zeroRecords = "Nessun risultato",
  paginate = list(previous = "Precedente", `next` = "Successiva")
)

#' Standard DT widget (compact, filterable when long).
app_datatable <- function(df, page_length = 10, searching = NULL, ...) {
  DT::datatable(
    df,
    rownames = FALSE,
    options = list(
      pageLength = page_length,
      language = dt_italian_language,
      searching = searching %||% (nrow(df) > page_length),
      lengthChange = FALSE,
      dom = if (nrow(df) > page_length) "ftip" else "t",
      scrollX = TRUE
    ),
    ...
  )
}

#' Table of exam statistics ready for display (labels, icons, formatted numbers).
exam_statistics_display <- function(exam_stats, courses) {
  df <- dplyr::left_join(
    dplyr::select(exam_stats, -dplyr::any_of("course_name")),
    courses[, c("course_id", "course_name")], by = "course_id")
  tibble::tibble(
    "CdS" = df$course_name,
    "Esame" = df$exam_name,
    "Tipo" = ifelse(df$mandatory, "Obbligatorio",
                    paste0("Opzionale (", df$choice_group, ")")),
    "N" = df$n,
    "Media" = fmt_grade(df$mean_grade),
    "Mediana" = fmt_grade(df$median_grade, 0),
    "DS" = fmt_grade(df$sd_grade, 2),
    "Quota di 30" = fmt_percent(df$share_top),
    "Valori distinti" = df$n_distinct,
    "Stabilita' riferimento" = paste(stability_icon(df$stability),
                                     stability_label(df$stability)),
    "Risoluzione" = resolution_label(df$resolution)
  )
}

#' Per-exam results of one student (real or hypothetical).
student_exams_display <- function(df) {
  tibble::tibble(
    "Esame" = df$exam_name,
    "Voto" = df$grade,
    "Percentile centrale" = fmt_percentile(100 * df$p_mid),
    "Intervallo pari merito" = fmt_tie_interval(df$p_lower, df$p_upper),
    "Normal score" = fmt_z(df$z),
    "N riferimento" = df$n_ref,
    "Stabilita'" = paste(stability_icon(df$stability),
                         stability_label(df$stability))
  )
}

#' Overall recovery metrics table for display.
recovery_overall_display <- function(overall) {
  tibble::tibble(
    "Indicatore" = overall$method,
    "r di Pearson" = fmt_correlation(overall$pearson),
    "rho di Spearman" = fmt_correlation(overall$spearman),
    "RMSE" = fmt_error(overall$rmse),
    "MAE" = fmt_error(overall$mae),
    "Bias medio" = fmt_error(overall$bias),
    "Top 10%: sensibilita'" = fmt_percent(overall$top10_sensitivity),
    "Top 10%: precisione" = fmt_percent(overall$top10_precision),
    "Top 10%: sovrapposizione" = fmt_percent(overall$top10_overlap),
    "Coppie concordanti" = fmt_percent(overall$concordant_pairs)
  )
}
