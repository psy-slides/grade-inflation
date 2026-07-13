# Percentili con pari merito: rango medio, intervallo, metodo conservativo, clipping.

test_that("il rango medio e l'intervallo dei pari merito sono corretti", {
  grades <- c(18, 20, 20, 25)
  tbl <- percentile_table(grades)
  i20 <- which(grades == 20)[1]
  expect_equal(tbl$p_mid[i20], (1 + 0.5 * 2) / 4)
  expect_equal(tbl$p_lower[i20], 1 / 4)
  expect_equal(tbl$p_upper[i20], 3 / 4)
  expect_equal(tbl$n_ties[i20], 2L)
  expect_equal(tbl$p_conservative[i20], 1 / 4)
})

test_that("tutti voti uguali: rango medio 0.5, intervallo pieno", {
  tbl <- percentile_table(rep(30, 50))
  expect_true(all(tbl$p_mid == 0.5))
  expect_true(all(tbl$p_lower == 0))
  expect_true(all(tbl$p_upper == 1))
})

test_that("un solo studente e due studenti", {
  one <- percentile_table(27)
  expect_equal(one$p_mid, 0.5)
  two <- percentile_table(c(24, 28))
  expect_equal(sort(two$p_mid), c(0.25, 0.75))
})

test_that("voto minimo e massimo", {
  tbl <- percentile_table(c(18, 22, 26, 30))
  expect_equal(tbl$p_lower[1], 0)
  expect_equal(tbl$p_upper[4], 1)
  expect_equal(tbl$p_conservative[1], 0)
})

test_that("moltissimi pari merito: il percentile centrale resta coerente", {
  grades <- c(rep(30, 90), rep(28, 10))
  tbl <- percentile_table(grades)
  i30 <- which(grades == 30)[1]
  expect_equal(tbl$p_mid[i30], (10 + 45) / 100)
  expect_equal(tbl$p_lower[i30], 0.10)
  expect_equal(tbl$p_upper[i30], 1)
})

test_that("il clipping evita esattamente 0 e 1 e qnorm resta finito", {
  p <- c(0, 1, 0.5, 0.001, 0.999)
  clipped <- clip_percentile(p, n_ref = 100)
  expect_true(all(clipped > 0 & clipped < 1))
  expect_true(all(is.finite(qnorm(clipped))))
  expect_equal(min(clipped), 0.5 / 100)
  expect_equal(max(clipped), 1 - 0.5 / 100)
  # Anche nel caso limite N = 1.
  expect_true(is.finite(percentile_to_normal_score(0, 1)))
})

test_that("il metodo conservativo assegna il limite inferiore della fascia", {
  grades <- c(rep(25, 4), rep(30, 6))
  tbl <- percentile_table(grades)
  cons <- select_percentile_method(tbl, "conservativo")
  expect_equal(unique(cons[grades == 25]), 0)
  expect_equal(unique(cons[grades == 30]), 0.4)
})

test_that("percentile_of_grade e' coerente con percentile_table", {
  ref <- c(18, 20, 20, 25, 30)
  hyp <- percentile_of_grade(20, ref)
  expect_equal(hyp$p_mid, (1 + 0.5 * 2) / 5)
  expect_equal(hyp$n_ref, 5L)
  empty <- percentile_of_grade(25, integer(0))
  expect_true(is.na(empty$p_mid))
})
