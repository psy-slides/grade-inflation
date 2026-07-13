# Costruzione dello scenario completo: riproducibilita', vincoli, casi limite.

test_that("stesso seed = stesso scenario; seed diverso = scenario diverso", {
  a <- build_scenario(small_config(seed = 7))
  b <- build_scenario(small_config(seed = 7))
  c <- build_scenario(small_config(seed = 8))
  expect_identical(a$grades$grade, b$grades$grade)
  expect_identical(a$students$theta, b$students$theta)
  expect_false(identical(a$grades$grade, c$grades$grade))
})

test_that("lambda e sigma_eps sono unici e comuni a tutti gli esami", {
  scenario <- build_scenario(small_config())
  expect_length(scenario$metadata$lambda, 1)
  expect_length(scenario$metadata$sigma_eps, 1)
  # Il modello non prevede colonne per discriminazioni o errori specifici per esame.
  expect_false(any(c("lambda", "sigma_eps") %in% names(scenario$exams)))
})

test_that("i default sono: affidabilita' media, scelta casuale, rango medio", {
  config <- make_scenario_config()
  expect_identical(config$reliability_level, "media")
  expect_identical(config$choice_mode, "casuale")
  expect_identical(config$percentile_method, "rango_medio")
  expect_equal(config$strategic_strength, 0)
  expect_equal(config$hard_exam_preference, 0)
})

test_that("nessun voto fuori da [18, 30] anche con affidabilita' bassa e target basso", {
  scenario <- build_scenario(small_config(
    reliability_level = "bassa", target_mean_grade = 22, seed = 11
  ))
  grades <- scenario$grades$grade[!scenario$grades$failed]
  expect_true(all(grades >= 18 & grades <= 30))
})

test_that("la media osservata rispetta la tolleranza di calibrazione documentata", {
  scenario <- build_scenario(make_scenario_config(seed = 5))
  # Tolleranza attesa (deterministica) +/- 0.10; sul campione simulato ammettiamo
  # anche l'errore campionario.
  expect_lt(abs(scenario$metadata$observed_mean - 27.5), 0.25)
  expect_true(scenario$metadata$calibration_converged)
})

test_that("N determina le numerosita' effettive e le spie di stabilita'", {
  small_n <- build_scenario(small_config(n_students_per_course = 30, seed = 3))
  big_n <- build_scenario(small_config(n_students_per_course = 400, seed = 3))
  opt_small <- small_n$exam_statistics[!small_n$exam_statistics$mandatory, ]
  opt_big <- big_n$exam_statistics[!big_n$exam_statistics$mandatory, ]
  expect_true(all(opt_small$n < 30))
  expect_true(all(opt_small$stability == "fragile"))
  expect_true(all(opt_big$stability %in% c("moderata", "buona")))
  mand_big <- big_n$exam_statistics[big_n$exam_statistics$mandatory, ]
  expect_true(all(mand_big$stability == "buona"))
})

test_that("N differenti per CdS sono supportati", {
  scenario <- build_scenario(small_config(students_per_course_custom = "40, 80"))
  counts <- table(scenario$students$course_id)
  expect_equal(unname(c(counts)), c(40, 80))
})

test_that("il riferimento percentile e' costituito dai soli iscritti all'esame", {
  scenario <- build_scenario(small_config())
  by_exam <- scenario$percentile_scores |>
    dplyr::group_by(exam_id) |>
    dplyr::summarise(n_rows = dplyr::n(), n_ref = unique(n_ref))
  expect_equal(by_exam$n_rows, by_exam$n_ref)
})

test_that("scenario minimo e scenario massimo funzionano", {
  minimal <- build_scenario(make_scenario_config(
    n_courses = 2, n_mandatory = 2, n_optional_groups = 0,
    n_students_per_course = 20, seed = 21
  ))
  expect_s3_class(minimal, "grade_scenario")
  expect_true(all(minimal$student_scores$decile %in% 1:10))

  maximal <- build_scenario(make_scenario_config(
    n_courses = 5, n_mandatory = 8, n_optional_groups = 2,
    n_alternatives_per_group = 2, n_students_per_course = 1000, seed = 22
  ))
  expect_s3_class(maximal, "grade_scenario")
  expect_equal(nrow(maximal$students), 5000)
  expect_true(all(is.finite(maximal$student_scores$aggregate_z)))
})

test_that("un esame senza iscritti non fa fallire le statistiche", {
  grades <- tibble::tibble(
    student_id = c("S1", "S2"), exam_id = c("E1", "E1"),
    grade = c(25L, 28L), n_attempts = c(1L, 1L), failed = c(FALSE, FALSE)
  )
  exams <- tibble::tibble(
    exam_id = c("E1", "E2"), exam_name = c("Uno", "Due"), course_id = "C1",
    mandatory = c(TRUE, FALSE), choice_group = c(NA, "C1-G1"),
    delta = c(0, 0), difficulty = c(0, 0)
  )
  stats_tbl <- compute_exam_statistics(grades, exams)
  expect_equal(stats_tbl$n[stats_tbl$exam_id == "E2"], 0)
  expect_true(is.na(stats_tbl$stability[stats_tbl$exam_id == "E2"]))
  expect_true(is.na(stats_tbl$resolution[stats_tbl$exam_id == "E2"]))
})

test_that("la validazione blocca configurazioni non plausibili", {
  bad_courses <- make_scenario_config(n_courses = 9)
  expect_false(validate_scenario_config(bad_courses)$valid)
  bad_target <- make_scenario_config(target_mean_grade = 35)
  expect_false(validate_scenario_config(bad_target)$valid)
  bad_custom <- make_scenario_config(students_per_course_custom = "10, abc")
  expect_false(validate_scenario_config(bad_custom)$valid)
  bad_seed <- make_scenario_config(seed = 1.5)
  expect_false(validate_scenario_config(bad_seed)$valid)
  ok <- make_scenario_config()
  expect_true(validate_scenario_config(ok)$valid)
})
