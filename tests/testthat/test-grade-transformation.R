# Trasformazione dei voti latenti in voti osservati (18-30, soffitto, tentativi).

test_that("latent_to_grade rispetta soglia, arrotondamento e soffitto", {
  latent <- c(17.9, 18, 18.4, 24.5, 29.6, 30, 30.4, 45, 10)
  grade <- latent_to_grade(latent)
  expect_identical(grade, as.integer(c(NA, 18, 18, 24, 30, 30, 30, 30, NA)))
})

test_that("nessun voto osservato sotto 18 o sopra 30", {
  set.seed(1)
  drawn <- draw_first_verbalized_grade(rep(20, 5000), sigma_eps = 3)
  expect_true(all(drawn$grade >= 18))
  expect_true(all(drawn$grade <= 30))
  expect_false(any(drawn$failed))
})

test_that("i tentativi sotto 18 vengono ripetuti mantenendo la stessa abilita'", {
  set.seed(2)
  # Media condizionata bassa: molti primi tentativi non verbalizzati.
  drawn <- draw_first_verbalized_grade(rep(15, 2000), sigma_eps = 3)
  expect_true(all(drawn$grade >= 18, na.rm = TRUE))
  expect_gt(mean(drawn$n_attempts), 1)
})

test_that("il limite tecnico di tentativi produce un fallimento esplicito", {
  set.seed(3)
  drawn <- draw_first_verbalized_grade(-100, sigma_eps = 1, max_attempts = 5)
  expect_true(drawn$failed)
  expect_identical(drawn$grade, NA_integer_)
  expect_identical(drawn$n_attempts, 5L)
})

test_that("l'effetto soffitto emerge quando i parametri lo richiedono", {
  set.seed(4)
  high <- draw_first_verbalized_grade(rep(31, 3000), sigma_eps = 1)
  expect_gt(mean(high$grade == 30), 0.6)
  low <- draw_first_verbalized_grade(rep(24, 3000), sigma_eps = 1)
  expect_lt(mean(low$grade == 30), 0.01)
})
