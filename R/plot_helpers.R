# ggplot2 helpers: sober academic style, discrete grade distributions first.
# Palette roles (validated for CVD separation; color is never the only channel:
# legends, direct labels and shapes always accompany it).

app_colors <- list(
  primary = "#2a78d6",    # serie principale (blu)
  secondary = "#1baf7a",  # seconda serie (aqua)
  tertiary = "#eda100",   # terza serie (giallo)
  accent = "#4a3aa7",     # evidenziazioni puntuali (violetto)
  good = "#0ca30c",
  warning = "#fab219",
  serious = "#ec835a",
  ink = "#0b0b0b",
  ink_secondary = "#52514e",
  muted = "#898781",
  grid = "#e1e0d9",
  baseline = "#c3c2b7",
  surface = "#fcfcfb"
)

stability_colors <- c(
  fragile = app_colors$serious,
  moderata = app_colors$warning,
  buona = app_colors$good
)

# Sequential blue ramp (light -> dark) for heatmaps / magnitude encodings.
sequential_blues <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf",
                      "#184f95", "#0d366b")

method_colors <- c(
  "Voto medio (standardizzato)" = "#2a78d6",
  "Percentili aggregati (normal score)" = "#1baf7a",
  "Media ingenua dei percentili" = "#eda100"
)

theme_app <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(color = app_colors$ink_secondary),
      plot.title = ggplot2::element_text(color = app_colors$ink, face = "bold",
                                         size = base_size + 1),
      plot.subtitle = ggplot2::element_text(color = app_colors$muted),
      axis.text = ggplot2::element_text(color = app_colors$ink_secondary),
      axis.title = ggplot2::element_text(color = app_colors$ink_secondary),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = app_colors$grid,
                                               linewidth = 0.35),
      strip.text = ggplot2::element_text(color = app_colors$ink, face = "bold"),
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = app_colors$surface,
                                              color = NA)
    )
}

#' Complete count table over the full discrete grade scale (18-30).
grade_count_table <- function(grades, grade_min = 18, grade_max = 30) {
  scale_values <- grade_min:grade_max
  counts <- table(factor(grades, levels = scale_values))
  tibble::tibble(grade = scale_values, n = as.integer(counts))
}

#' Discrete bar chart of observed grades (18-30), optionally highlighting one grade.
plot_grade_distribution <- function(grades, title = NULL, subtitle = NULL,
                                    highlight = NULL) {
  df <- grade_count_table(grades)
  df$fill <- ifelse(!is.null(highlight) & df$grade == (highlight %||% -1),
                    "highlight", "base")
  ggplot2::ggplot(df, ggplot2::aes(x = factor(grade), y = n, fill = fill)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::scale_fill_manual(
      values = c(base = app_colors$primary, highlight = app_colors$tertiary),
      guide = "none"
    ) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = "Voto verbalizzato", y = "Numero di studenti") +
    theme_app()
}

#' Faceted grade distributions for a set of exams.
plot_grade_distributions_faceted <- function(grades_df, ncol = 3) {
  full <- grades_df |>
    dplyr::group_by(panel_label) |>
    dplyr::group_modify(function(g, key) grade_count_table(g$grade)) |>
    dplyr::ungroup()
  ggplot2::ggplot(full, ggplot2::aes(x = factor(grade), y = n)) +
    ggplot2::geom_col(width = 0.72, fill = app_colors$primary) +
    ggplot2::facet_wrap(~panel_label, ncol = ncol, scales = "free_y") +
    ggplot2::labs(x = "Voto verbalizzato", y = "Numero di studenti") +
    theme_app() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8))
}

#' Heatmap exam x grade (share of takers), sequential single-hue ramp.
plot_exam_grade_heatmap <- function(grades_df) {
  shares <- grades_df |>
    dplyr::group_by(panel_label) |>
    dplyr::group_modify(function(g, key) {
      tab <- grade_count_table(g$grade)
      tab$share <- tab$n / max(sum(tab$n), 1)
      tab
    }) |>
    dplyr::ungroup()
  ggplot2::ggplot(shares, ggplot2::aes(x = factor(grade), y = panel_label,
                                       fill = share)) +
    ggplot2::geom_tile(color = app_colors$surface, linewidth = 0.6) +
    ggplot2::scale_fill_gradientn(colors = sequential_blues,
                                  labels = scales::percent_format(accuracy = 1),
                                  name = "Quota") +
    ggplot2::labs(x = "Voto verbalizzato", y = NULL) +
    theme_app()
}

#' Dot plot of exam means with the share of 30 as an explicit text label.
plot_exam_means <- function(exam_stats) {
  df <- exam_stats[exam_stats$n > 0, ]
  df$label <- paste0(df$exam_name, " (", df$course_id, ")")
  df$share_label <- paste0(fmt_num(100 * df$share_top, 0), "% di 30")
  x_min <- min(df$mean_grade)
  ggplot2::ggplot(df, ggplot2::aes(x = mean_grade,
                                   y = stats::reorder(label, mean_grade))) +
    ggplot2::geom_segment(ggplot2::aes(xend = mean_grade,
                                       yend = stats::reorder(label, mean_grade)),
                          x = x_min,
                          color = app_colors$grid, linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(color = share_top), size = 3.2) +
    ggplot2::geom_text(ggplot2::aes(label = share_label), hjust = -0.15,
                       size = 3, color = app_colors$ink_secondary) +
    ggplot2::scale_color_gradientn(colors = sequential_blues, guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.22))) +
    ggplot2::labs(x = "Voto medio dell'esame", y = NULL) +
    theme_app()
}

#' Effective enrollment per exam, mandatory vs optional (explicit labels).
plot_exam_enrollment <- function(exam_stats) {
  df <- exam_stats
  df$label <- paste0(df$exam_name, " (", df$course_id, ")")
  df$tipo <- ifelse(df$mandatory, "Obbligatorio", "Opzionale")
  ggplot2::ggplot(df, ggplot2::aes(x = n, y = stats::reorder(label, n),
                                   fill = tipo)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.2, size = 3,
                       color = app_colors$ink_secondary) +
    ggplot2::scale_fill_manual(values = c(Obbligatorio = app_colors$primary,
                                          Opzionale = app_colors$tertiary),
                               name = NULL) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = "Studenti che sostengono l'esame", y = NULL) +
    theme_app()
}

#' Histogram of the true simulated abilities.
plot_theta_distribution <- function(students) {
  ggplot2::ggplot(students, ggplot2::aes(x = theta)) +
    ggplot2::geom_histogram(bins = 40, fill = app_colors$primary,
                            color = app_colors$surface, linewidth = 0.3) +
    ggplot2::labs(x = "Abilita' vera simulata (theta)", y = "Numero di studenti") +
    theme_app()
}

#' Relation between theta and the observed grade (grades stay discrete).
plot_theta_vs_grade <- function(grades_students) {
  ggplot2::ggplot(grades_students,
                  ggplot2::aes(x = theta, y = grade)) +
    ggplot2::geom_jitter(height = 0.22, width = 0, alpha = 0.25, size = 0.8,
                         color = app_colors$primary) +
    ggplot2::scale_y_continuous(breaks = 18:30) +
    ggplot2::labs(x = "Abilita' vera simulata (theta)",
                  y = "Voto verbalizzato") +
    theme_app()
}

#' Latent (pre-threshold/ceiling) density vs observed discrete distribution.
plot_latent_vs_observed <- function(grades, latent_mean, latent_sd,
                                    title = NULL) {
  df <- grade_count_table(grades)
  n_total <- sum(df$n)
  curve_x <- seq(14, 34, by = 0.1)
  curve_df <- tibble::tibble(
    x = curve_x,
    y = dnorm(curve_x, latent_mean, latent_sd) * n_total
  )
  ggplot2::ggplot() +
    ggplot2::geom_col(data = df, ggplot2::aes(x = grade, y = n),
                      width = 0.72, fill = app_colors$primary, alpha = 0.85) +
    ggplot2::geom_line(data = curve_df, ggplot2::aes(x = x, y = y),
                       color = app_colors$serious, linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = c(17.5, 30.5), linetype = "dashed",
                        color = app_colors$muted) +
    ggplot2::scale_x_continuous(breaks = seq(14, 34, 2)) +
    ggplot2::labs(
      title = title,
      subtitle = "Barre: voti osservati. Linea: distribuzione latente prima di soglia (18), soffitto (30) e arrotondamento.",
      x = "Scala latente / voto", y = "Numero di studenti"
    ) +
    theme_app()
}

#' Scatter of one estimate against standardized theta, with identity line.
plot_recovery_scatter <- function(df, estimate_col, estimate_label) {
  ggplot2::ggplot(df, ggplot2::aes(x = theta_std, y = .data[[estimate_col]])) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = app_colors$muted,
                         linetype = "dashed") +
    ggplot2::geom_point(alpha = 0.3, size = 1,
                        color = unname(method_colors[estimate_label] %||%
                                         app_colors$primary)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Abilita' vera simulata (standardizzata)",
                  y = estimate_label) +
    theme_app()
}

#' Estimation errors along theta for the two main methods.
plot_recovery_errors <- function(df) {
  long <- tidyr::pivot_longer(
    df,
    cols = c("estimate_from_mean_grade", "estimate_from_percentiles"),
    names_to = "method", values_to = "estimate"
  )
  long$method <- ifelse(long$method == "estimate_from_mean_grade",
                        "Voto medio (standardizzato)",
                        "Percentili aggregati (normal score)")
  long$error <- long$estimate - long$theta_std
  ggplot2::ggplot(long, ggplot2::aes(x = theta_std, y = error,
                                     color = method)) +
    ggplot2::geom_hline(yintercept = 0, color = app_colors$baseline) +
    ggplot2::geom_point(alpha = 0.15, size = 0.7) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                         linewidth = 1) +
    ggplot2::scale_color_manual(values = method_colors, name = NULL) +
    ggplot2::labs(x = "Abilita' vera simulata (standardizzata)",
                  y = "Errore (stima − theta standardizzato)") +
    theme_app()
}

#' Decile classification matrix (true decile x estimated decile).
plot_decile_matrix <- function(true_decile, est_decile, estimate_label) {
  df <- as.data.frame(table(Vero = true_decile, Stimato = est_decile))
  ggplot2::ggplot(df, ggplot2::aes(x = Vero, y = Stimato, fill = Freq)) +
    ggplot2::geom_tile(color = app_colors$surface, linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(Freq > 0, Freq, "")),
                       size = 2.8, color = app_colors$ink) +
    ggplot2::scale_fill_gradientn(colors = c(app_colors$surface,
                                             sequential_blues),
                                  name = "Studenti") +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Decile dell'abilita' vera", y = "Decile stimato",
                  subtitle = estimate_label) +
    theme_app()
}

#' Distributions of the compared estimates (frequency polygons on one scale).
plot_estimate_distributions <- function(df) {
  long <- tidyr::pivot_longer(
    df,
    cols = c("estimate_from_mean_grade", "estimate_from_percentiles"),
    names_to = "method", values_to = "estimate"
  )
  long$method <- ifelse(long$method == "estimate_from_mean_grade",
                        "Voto medio (standardizzato)",
                        "Percentili aggregati (normal score)")
  ggplot2::ggplot(long, ggplot2::aes(x = estimate, color = method)) +
    ggplot2::geom_freqpoly(bins = 40, linewidth = 0.9) +
    ggplot2::scale_color_manual(values = method_colors, name = NULL) +
    ggplot2::labs(x = "Stima (scala standardizzata)", y = "Numero di studenti") +
    theme_app()
}

#' Pearson correlation with theta by course and method (dot plot).
plot_recovery_by_course <- function(by_course, courses) {
  df <- dplyr::left_join(by_course, courses, by = "course_id")
  ggplot2::ggplot(df, ggplot2::aes(x = pearson, y = course_name,
                                   color = method, shape = method)) +
    ggplot2::geom_point(size = 3.4, position = ggplot2::position_dodge(0.4)) +
    ggplot2::scale_color_manual(values = method_colors, name = NULL) +
    ggplot2::scale_shape_manual(values = c(16, 17), name = NULL) +
    ggplot2::labs(x = "Correlazione di Pearson con theta (entro CdS)", y = NULL) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
    theme_app()
}

#' Position comparison for a single student on one standardized scale.
#' `positions` is a tibble(label, value); theta (if present) is drawn as a
#' dashed reference line so estimation errors are immediately visible.
plot_student_positions <- function(positions, theta = NULL) {
  positions$label <- factor(positions$label, levels = rev(positions$label))
  p <- ggplot2::ggplot(positions, ggplot2::aes(x = value, y = label))
  if (!is.null(theta)) {
    p <- p +
      ggplot2::geom_vline(xintercept = theta, linetype = "dashed",
                          color = app_colors$ink_secondary) +
      ggplot2::geom_segment(ggplot2::aes(x = theta, xend = value, yend = label),
                            color = app_colors$muted, linewidth = 0.5)
  }
  p +
    ggplot2::geom_point(size = 4.5, color = app_colors$primary) +
    ggplot2::geom_text(ggplot2::aes(label = fmt_z(value)), vjust = -1.2,
                       size = 3.4, color = app_colors$ink) +
    ggplot2::scale_x_continuous(limits = function(lims) {
      range <- max(abs(c(lims, 2.6)), na.rm = TRUE)
      c(-range, range)
    }) +
    ggplot2::labs(x = "Scala standardizzata (unita' di deviazione standard)",
                  y = NULL) +
    theme_app() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Save a plot for download (PNG, sober defaults).
save_plot_png <- function(file, plot, width = 9, height = 6) {
  ggplot2::ggsave(file, plot = plot, width = width, height = height, dpi = 150,
                  bg = app_colors$surface)
}
