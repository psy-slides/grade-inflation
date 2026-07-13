# Transformation from latent performance to observed (verbalized) grades.
#
# Rules (documented in METHODS.md):
#   - latent values > grade_max become grade_max (ceiling / grade inflation);
#   - latent values in [grade_min, grade_max] are rounded to the nearest integer;
#   - latent values < grade_min are NOT verbalized: the attempt is repeated with the
#     same student ability, regenerating only the exam-specific error, until a value
#     >= grade_min is obtained ("first verbalized grade" simplification).

#' Transform latent values into observed grades. Values below grade_min return NA
#' (not verbalized); the retry logic lives in draw_first_verbalized_grade().
latent_to_grade <- function(latent, grade_min = 18, grade_max = 30) {
  grade <- round(latent)
  grade[latent > grade_max] <- grade_max
  grade[latent < grade_min] <- NA_integer_
  as.integer(grade)
}

#' First verbalized grade for a vector of students on one "conditional mean" each.
#'
#' @param conditional_mean numeric vector: base_location + delta_j + lambda * theta_i,
#'   one entry per (student, exam) enrollment.
#' @param sigma_eps common residual sd (identical across exams by design).
#' @param max_attempts technical cap on the number of attempts; exceeding it is an
#'   explicit failure state (grade NA, flag in the result), not a silent error.
#' @return list(grade = integer vector, n_attempts = integer vector,
#'              failed = logical vector).
draw_first_verbalized_grade <- function(conditional_mean, sigma_eps,
                                        grade_min = 18, grade_max = 30,
                                        max_attempts = 1000) {
  n <- length(conditional_mean)
  grade <- rep(NA_integer_, n)
  n_attempts <- rep(0L, n)
  pending <- seq_len(n)
  attempt <- 0L
  while (length(pending) > 0 && attempt < max_attempts) {
    attempt <- attempt + 1L
    latent <- conditional_mean[pending] + rnorm(length(pending), 0, sigma_eps)
    drawn <- latent_to_grade(latent, grade_min, grade_max)
    verbalized <- !is.na(drawn)
    n_attempts[pending] <- attempt
    grade[pending[verbalized]] <- drawn[verbalized]
    pending <- pending[!verbalized]
  }
  list(
    grade = grade,
    n_attempts = n_attempts,
    failed = is.na(grade)
  )
}
