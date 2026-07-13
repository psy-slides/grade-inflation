# Number and label formatting (Italian locale conventions, no false precision).
# Formatting rules: grades 1-2 decimals, percentiles 0-1 decimals, correlations
# 2-3 decimals, RMSE/MAE 2 decimals, percentages 1 decimal.

fmt_num <- function(x, digits) {
  ifelse(is.na(x), "—",
         formatC(round(x, digits), format = "f", digits = digits,
                 big.mark = ".", decimal.mark = ","))
}

fmt_grade <- function(x, digits = 1) fmt_num(x, digits)
fmt_percentile <- function(x, digits = 0) fmt_num(x, digits)
fmt_correlation <- function(x, digits = 2) fmt_num(x, digits)
fmt_error <- function(x, digits = 2) fmt_num(x, digits)
fmt_z <- function(x, digits = 2) fmt_num(x, digits)

fmt_percent <- function(x, digits = 1) {
  ifelse(is.na(x), "—", paste0(fmt_num(100 * x, digits), "%"))
}

fmt_int <- function(x) {
  ifelse(is.na(x), "—",
         formatC(x, format = "d", big.mark = ".", decimal.mark = ","))
}

#' Human-readable label for the tie interval of a percentile (0-100 scale).
fmt_tie_interval <- function(p_lower, p_upper, digits = 0) {
  ifelse(is.na(p_lower), "—",
         paste0(fmt_num(100 * p_lower, digits), "–",
                fmt_num(100 * p_upper, digits)))
}

#' Italian labels and icons for stability / resolution levels (never color-only).
stability_label <- function(level) {
  labels <- c(fragile = "Fragile", moderata = "Moderata", buona = "Buona")
  out <- labels[as.character(level)]
  out[is.na(out)] <- "Non osservato"
  unname(out)
}

stability_icon <- function(level) {
  icons <- c(fragile = "⚠", moderata = "◎", buona = "✓")
  out <- icons[as.character(level)]
  out[is.na(out)] <- "∅"
  unname(out)
}

resolution_label <- function(level) {
  labels <- c(scarsa = "Scarsa", media = "Media", buona = "Buona")
  out <- labels[as.character(level)]
  out[is.na(out)] <- "Non osservato"
  unname(out)
}
