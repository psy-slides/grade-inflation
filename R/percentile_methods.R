# Percentile methods with explicit tie handling.
#
# For a grade x within one exam, with:
#   L = number of grades strictly below x,
#   E = number of grades equal to x,
#   N = total number of grades in that exam,
# we define:
#   mid-rank percentile   p_mid  = (L + 0.5 * E) / N   (default method)
#   tie interval          [p_lower, p_upper] = [L / N, (L + E) / N]  (always available)
#   conservative method   p_cons = L / N                (optional, explicitly "severe")
#   upper method          p_up   = (L + E) / N          (illustrative only)
#
# The reference population of each exam is: all and only the simulated students who
# took that exam, in the single simulated cycle. No historical pooling.

#' Percentile table for one exam's observed grades.
#'
#' @param grades integer vector of observed grades for ONE exam.
#' @return tibble with one row per grade in `grades` (same order), columns:
#'   p_mid, p_lower, p_upper, p_conservative, n_ties, n_ref.
percentile_table <- function(grades) {
  n <- length(grades)
  if (n == 0) {
    return(tibble::tibble(
      p_mid = numeric(0), p_lower = numeric(0), p_upper = numeric(0),
      p_conservative = numeric(0), n_ties = integer(0), n_ref = integer(0)
    ))
  }
  counts <- table(grades)
  values <- as.numeric(names(counts))
  below <- cumsum(c(0, as.numeric(counts)))[seq_along(counts)]
  idx <- match(grades, values)
  L <- below[idx]
  E <- as.numeric(counts)[idx]
  tibble::tibble(
    p_mid = (L + 0.5 * E) / n,
    p_lower = L / n,
    p_upper = (L + E) / n,
    p_conservative = L / n,
    n_ties = as.integer(E),
    n_ref = n
  )
}

#' Percentiles of a hypothetical grade placed against an existing reference.
#' The hypothetical student is NOT added to the reference (documented choice):
#' the app locates the grade relative to the simulated distribution.
percentile_of_grade <- function(grade, reference_grades) {
  n <- length(reference_grades)
  if (n == 0) {
    return(list(p_mid = NA_real_, p_lower = NA_real_, p_upper = NA_real_,
                p_conservative = NA_real_, n_ties = NA_integer_, n_ref = 0L))
  }
  L <- sum(reference_grades < grade)
  E <- sum(reference_grades == grade)
  list(
    p_mid = (L + 0.5 * E) / n,
    p_lower = L / n,
    p_upper = (L + E) / n,
    p_conservative = L / n,
    n_ties = as.integer(E),
    n_ref = as.integer(n)
  )
}

#' Clip percentiles away from exactly 0 and 1 before qnorm().
#' Documented rule: p is restricted to [0.5/N, 1 - 0.5/N].
clip_percentile <- function(p, n_ref) {
  stopifnot(all(n_ref > 0 | is.na(n_ref)))
  pmin(pmax(p, 0.5 / n_ref), 1 - 0.5 / n_ref)
}

#' Normal score of a percentile (with mandatory clipping).
percentile_to_normal_score <- function(p, n_ref) {
  qnorm(clip_percentile(p, n_ref))
}

#' Select the percentile column used for scoring, given the configured method.
select_percentile_method <- function(tbl, method = c("rango_medio", "conservativo")) {
  method <- match.arg(method)
  if (method == "rango_medio") tbl$p_mid else tbl$p_conservative
}
