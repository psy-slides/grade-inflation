# Generation of the simulated student population.
# Each student has a single latent general ability theta ~ N(0, 1); the whole
# simulation covers ONE hypothetical cycle (no multiple academic years).

#' Generate the students table.
#'
#' @param courses tibble from generate_courses().
#' @return tibble: student_id, course_id, theta.
generate_students <- function(courses) {
  n_total <- sum(courses$n_students)
  tibble::tibble(
    student_id = sprintf("S%04d", seq_len(n_total)),
    course_id = rep(courses$course_id, courses$n_students),
    theta = rnorm(n_total, 0, 1)
  )
}
