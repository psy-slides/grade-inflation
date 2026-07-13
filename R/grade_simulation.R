# Latent grade model, deterministic calibration of base_location, and grade simulation.
#
# Latent model: y*_ij = base_location + delta_j + lambda * theta_i + eps_ij,
# eps_ij ~ N(0, sigma_eps^2). lambda and sigma_eps are COMMON to all exams
# (declared simplification: heterogeneity concerns exam generosity delta_j only).

#' Expected observed mean grade of each exam, computed deterministically.
#'
#' Integrates the conditional expectation of the observed grade (rounding, ceiling
#' at grade_max, retry-below-grade_min = truncation of each attempt) over a fixed
#' grid of N(0,1) quantile midpoints for theta. No Monte Carlo: fast, reproducible,
#' used both for calibrating base_location and as expected_grade_j in the strategic
#' choice mechanism.
#'
#' @return numeric vector, one expected observed mean per exam.
expected_exam_means <- function(base_location, delta, sigma_eps, lambda,
                                grade_min, grade_max, n_theta_grid) {
  theta_grid <- qnorm((seq_len(n_theta_grid) - 0.5) / n_theta_grid)
  # Conditional means: one row per exam, one column per theta node.
  mu <- outer(base_location + delta, lambda * theta_grid, `+`)
  grades <- grade_min:grade_max
  # Interval bounds for each observed grade after rounding within [grade_min, grade_max]:
  # grade_min <- [grade_min, grade_min + 0.5); k <- [k - 0.5, k + 0.5); grade_max <- [grade_max - 0.5, Inf).
  lower <- c(grade_min, grades[-1] - 0.5)
  upper <- c(grades[-length(grades)] + 0.5, Inf)
  numerator <- 0
  total_prob <- 0
  for (g in seq_along(grades)) {
    prob <- pnorm(upper[g], mu, sigma_eps) - pnorm(lower[g], mu, sigma_eps)
    numerator <- numerator + grades[g] * prob
    total_prob <- total_prob + prob
  }
  # total_prob = P(attempt verbalized | theta); the retry procedure conditions on it.
  cond_mean <- ifelse(total_prob > 1e-15, numerator / total_prob, grade_min)
  rowMeans(cond_mean)
}

#' Expected overall observed mean, weighting exams by their expected share of takers.
#' Mandatory exams weigh 1; each alternative in a group weighs 1/n_alternatives
#' (random-choice approximation, adequate for the documented +/- tolerance).
expected_overall_mean <- function(base_location, exams, sigma_eps, lambda,
                                  grade_min, grade_max, n_theta_grid) {
  means <- expected_exam_means(base_location, exams$delta, sigma_eps, lambda,
                               grade_min, grade_max, n_theta_grid)
  n_alt <- stats::ave(seq_len(nrow(exams)), exams$choice_group,
                      FUN = length)
  weight <- ifelse(exams$mandatory, 1, 1 / n_alt)
  sum(means * weight) / sum(weight)
}

#' Calibrate base_location so that the expected overall observed mean matches the
#' target. One-dimensional root search on a monotone deterministic function.
#'
#' @return list(base_location, expected_mean, converged, message).
calibrate_base_location <- function(exams, config, sigma_eps,
                                    app_config = load_app_config()) {
  model <- app_config$model
  cal <- app_config$calibration
  target <- config$target_mean_grade
  f <- function(b) {
    expected_overall_mean(b, exams, sigma_eps, model$lambda,
                          model$grade_min, model$grade_max, cal$n_theta_grid) - target
  }
  lo <- cal$base_location_min
  hi <- cal$base_location_max
  f_lo <- f(lo)
  f_hi <- f(hi)
  if (f_lo >= 0) {
    # Even the lowest admissible base exceeds the target (very unlikely given limits).
    return(list(base_location = lo, expected_mean = f_lo + target, converged = FALSE,
                message = "Target medio non raggiungibile dal basso: uso il limite inferiore."))
  }
  if (f_hi <= 0) {
    return(list(base_location = hi, expected_mean = f_hi + target, converged = FALSE,
                message = "Target medio non raggiungibile per effetto soffitto: uso il limite superiore."))
  }
  root <- stats::uniroot(f, c(lo, hi), tol = 1e-4)
  expected <- root$f.root + target
  list(
    base_location = root$root,
    expected_mean = expected,
    converged = abs(expected - target) <= cal$tolerance,
    message = NULL
  )
}

#' Simulate observed grades for all enrollments (first verbalized grade).
#'
#' @param enrollments tibble student_id, exam_id.
#' @param students tibble with student_id, theta.
#' @param exams tibble with exam_id, delta.
#' @return tibble: student_id, exam_id, grade (int, NA if technically failed),
#'   n_attempts, failed.
simulate_grades <- function(enrollments, students, exams, base_location, sigma_eps,
                            max_attempts, app_config = load_app_config()) {
  model <- app_config$model
  df <- dplyr::left_join(enrollments, students[, c("student_id", "theta")],
                         by = "student_id")
  df <- dplyr::left_join(df, exams[, c("exam_id", "delta")], by = "exam_id")
  conditional_mean <- base_location + df$delta + model$lambda * df$theta
  drawn <- draw_first_verbalized_grade(
    conditional_mean, sigma_eps,
    grade_min = model$grade_min, grade_max = model$grade_max,
    max_attempts = max_attempts
  )
  tibble::tibble(
    student_id = df$student_id,
    exam_id = df$exam_id,
    grade = drawn$grade,
    n_attempts = drawn$n_attempts,
    failed = drawn$failed
  )
}
