# Assignment of students to exams: all mandatory exams plus one alternative per
# choice group, selected through a transparent probabilistic mechanism.
#
# Deterministic utility for student i and alternative j within a choice group:
#   utility_ij = strategic_strength * expected_grade_std_j
#              + hard_exam_preference * theta_i * difficulty_std_j
# Choice probabilities are softmax(utility / temperature); the individual random
# component is realized by the multinomial sampling itself (equivalent to adding
# Gumbel noise to the utilities). With both parameters at 0 the choice is uniform.
#
# expected_grade_std and difficulty_std are standardized WITHIN the choice group;
# difficulty = -delta, so higher values mean harder exams (documented convention).

#' Standardize a vector within a group; constant vectors map to zeros.
standardize_in_group <- function(x) {
  s <- stats::sd(x)
  if (length(x) < 2 || is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x)) / s
}

#' Row-wise softmax with temperature.
softmax_rows <- function(utility, temperature = 1) {
  stopifnot(temperature > 0)
  scaled <- utility / temperature
  scaled <- scaled - apply(scaled, 1, max)  # numerical stability
  expu <- exp(scaled)
  expu / rowSums(expu)
}

#' Vectorized categorical sampling from a matrix of row probabilities.
sample_from_rows <- function(prob) {
  n <- nrow(prob)
  j <- ncol(prob)
  cum <- prob
  if (j > 1) for (col in 2:j) cum[, col] <- cum[, col - 1] + cum[, col]
  u <- runif(n)
  1L + rowSums(cum < u)
}

#' Assign every student to their full exam path (mandatory + one per choice group).
#'
#' @param students tibble with student_id, course_id, theta.
#' @param exams tibble with exam_id, course_id, mandatory, choice_group, difficulty,
#'   expected_grade (expected observed mean, from the calibration machinery).
#' @param config scenario_config (strategic_strength, hard_exam_preference,
#'   softmax_temperature, choice_mode).
#' @return enrollments tibble: student_id, exam_id.
assign_exam_paths <- function(students, exams, config) {
  strategic <- if (identical(config$choice_mode, "casuale")) 0 else config$strategic_strength
  hard_pref <- if (identical(config$choice_mode, "casuale")) 0 else config$hard_exam_preference

  mandatory <- dplyr::inner_join(
    students[, c("student_id", "course_id")],
    exams[exams$mandatory, c("exam_id", "course_id")],
    by = "course_id", relationship = "many-to-many"
  )[, c("student_id", "exam_id")]

  groups <- unique(stats::na.omit(exams$choice_group))
  chosen <- lapply(groups, function(g) {
    alternatives <- exams[!is.na(exams$choice_group) & exams$choice_group == g, ]
    members <- students[students$course_id == alternatives$course_id[1], ]
    eg_std <- standardize_in_group(alternatives$expected_grade)
    diff_std <- standardize_in_group(alternatives$difficulty)
    utility <- strategic * matrix(eg_std, nrow(members), nrow(alternatives), byrow = TRUE) +
      hard_pref * outer(members$theta, diff_std)
    prob <- softmax_rows(utility, config$softmax_temperature)
    pick <- sample_from_rows(prob)
    tibble::tibble(student_id = members$student_id,
                   exam_id = alternatives$exam_id[pick])
  })

  enrollments <- rbind(mandatory, do.call(rbind, chosen))
  enrollments[order(enrollments$student_id, enrollments$exam_id), ]
}
