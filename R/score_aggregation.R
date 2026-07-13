# Aggregation of exam-level percentiles into a student-level indicator.
#
# Main method (documented in METHODS.md):
#   1. exam percentile (mid-rank by default) -> clipping -> normal score z = qnorm(p);
#   2. student score = mean of the student's normal scores;
#   3. the score is ranked (mid-rank, tie-aware) against ALL simulated students of
#      ALL degree programmes: exams are already standardized against their own
#      reference, so the final comparison pools every programme;
#   4. the final percentile (0-100) and the decile (1-10) derive from that rank.
#
# Comparison methods computed alongside: raw mean grade, pooled z-score of the mean
# grade, naive mean of percentiles (explicitly labelled as naive).

#' Mid-rank percentile of each value within the full vector (tie-aware).
midrank_percentile <- function(x) {
  n <- length(x)
  if (n == 0) return(numeric(0))
  (rank(x, ties.method = "average") - 0.5) / n
}

#' Decile (1-10) from a percentile in (0, 1).
percentile_to_decile <- function(p) {
  as.integer(pmin(pmax(ceiling(p * 10), 1L), 10L))
}

#' Aggregate per-student scores from the long (student x exam) percentile table.
#'
#' @param percentile_scores tibble with columns student_id, z (normal score of the
#'   clipped percentile of the configured method), p_used (clipped percentile),
#'   plus p_mid / p_lower / p_upper for diagnostics.
#' @param grades tibble with student_id, grade.
#' @return tibble, one row per student: mean_grade, mean_grade_z, mean_percentile_naive,
#'   aggregate_z (mean of normal scores), aggregate_percentile (0-100), decile,
#'   n_exams_taken.
aggregate_student_scores <- function(percentile_scores, grades) {
  per_student <- percentile_scores |>
    dplyr::group_by(student_id) |>
    dplyr::summarise(
      aggregate_z = mean(z),
      mean_percentile_naive = mean(p_used) * 100,
      n_exams_taken = dplyr::n(),
      .groups = "drop"
    )
  grade_summary <- grades |>
    dplyr::group_by(student_id) |>
    dplyr::summarise(mean_grade = mean(grade), .groups = "drop")
  scores <- dplyr::left_join(per_student, grade_summary, by = "student_id")

  # Pooled standardization of the mean grade: differences in programme/exam
  # generosity remain visible on purpose (that is the phenomenon under study).
  scores$mean_grade_z <- as.numeric(scale(scores$mean_grade))
  if (all(is.na(scores$mean_grade_z))) scores$mean_grade_z <- rep(0, nrow(scores))

  p_final <- midrank_percentile(scores$aggregate_z)
  n_students <- nrow(scores)
  p_final_clipped <- clip_percentile(p_final, n_students)
  scores$aggregate_percentile <- p_final * 100
  scores$decile <- percentile_to_decile(p_final)
  # Comparable-scale estimate: normal score of the clipped aggregate percentile.
  scores$estimate_from_percentiles <- qnorm(p_final_clipped)
  scores$estimate_from_mean_grade <- scores$mean_grade_z
  scores$estimate_from_naive_mean <- as.numeric(scale(scores$mean_percentile_naive))
  if (all(is.na(scores$estimate_from_naive_mean))) {
    scores$estimate_from_naive_mean <- rep(0, nrow(scores))
  }
  scores
}

#' Place a hypothetical student's aggregate score within the simulated population.
#' Mid-rank against the reference scores WITHOUT adding the student to it.
place_score_in_population <- function(score, reference_scores) {
  n <- length(reference_scores)
  if (n == 0 || is.na(score)) {
    return(list(percentile = NA_real_, decile = NA_integer_))
  }
  L <- sum(reference_scores < score)
  E <- sum(reference_scores == score)
  p <- (L + 0.5 * E) / n
  list(percentile = p * 100, decile = percentile_to_decile(max(p, 1e-9)))
}
