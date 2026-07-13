# Meccanismo di scelta degli esami opzionali (softmax su utilita' trasparenti).

make_choice_setup <- function(n_students = 4000, seed = 42) {
  set.seed(seed)
  students <- tibble::tibble(
    student_id = sprintf("S%04d", seq_len(n_students)),
    course_id = "C1",
    theta = rnorm(n_students)
  )
  exams <- tibble::tibble(
    exam_id = c("E1", "E2", "E3"),
    exam_name = c("Obbligatorio", "Opzione A", "Opzione B"),
    course_id = "C1",
    mandatory = c(TRUE, FALSE, FALSE),
    choice_group = c(NA, "C1-G1", "C1-G1"),
    delta = c(0, 1, -1),          # A piu' generoso, B piu' difficile
    difficulty = c(0, -1, 1),
    expected_grade = c(27, 28.5, 26)
  )
  list(students = students, exams = exams)
}

choice_shares <- function(enrollments, exams) {
  chosen <- enrollments[enrollments$exam_id %in%
                          exams$exam_id[!exams$mandatory], ]
  table(chosen$exam_id) / nrow(chosen)
}

test_that("la scelta casuale e' circa uniforme in grandi campioni", {
  setup <- make_choice_setup()
  config <- small_config()  # default: casuale, forze a 0
  enrollments <- assign_exam_paths(setup$students, setup$exams, config)
  shares <- choice_shares(enrollments, setup$exams)
  expect_equal(unname(shares[["E2"]]), 0.5, tolerance = 0.05)
  # Ogni studente ha un percorso completo: obbligatorio + una alternativa.
  per_student <- table(enrollments$student_id)
  expect_true(all(per_student == 2))
})

test_that("la scelta strategica orienta verso l'esame con voto atteso piu' alto", {
  setup <- make_choice_setup()
  config <- small_config(choice_mode = "strategica", strategic_strength = 1.5)
  enrollments <- assign_exam_paths(setup$students, setup$exams, config)
  shares <- choice_shares(enrollments, setup$exams)
  expect_gt(unname(shares[["E2"]]), 0.7)  # E2 ha voto atteso maggiore
})

test_that("la preferenza dei migliori orienta i theta alti verso esami difficili", {
  setup <- make_choice_setup()
  config <- small_config(choice_mode = "strategica", hard_exam_preference = 1.5)
  enrollments <- assign_exam_paths(setup$students, setup$exams, config)
  chosen <- dplyr::inner_join(
    enrollments[enrollments$exam_id %in% c("E2", "E3"), ],
    setup$students, by = "student_id"
  )
  chosen <- dplyr::inner_join(chosen,
                              setup$exams[, c("exam_id", "difficulty")],
                              by = "exam_id")
  expect_gt(cor(chosen$theta, chosen$difficulty), 0.2)
  # Il meccanismo resta probabilistico: anche i theta bassi scelgono talvolta
  # l'esame difficile (nessuna forzatura deterministica simmetrica).
  low <- chosen[chosen$theta < -1, ]
  expect_gt(mean(low$exam_id == "E3"), 0.01)
  expect_lt(mean(low$exam_id == "E3"), 0.5)
})

test_that("softmax_rows restituisce probabilita' valide", {
  u <- matrix(c(0, 1, 10, -5, 0, 5), nrow = 2, byrow = TRUE)
  p <- softmax_rows(u, temperature = 1)
  expect_equal(rowSums(p), c(1, 1))
  expect_true(all(p > 0 & p < 1))
  # Temperatura alta -> scelta piu' vicina all'uniforme.
  p_hot <- softmax_rows(u, temperature = 100)
  expect_lt(max(abs(p_hot - 1 / 3)), 0.05)
})
