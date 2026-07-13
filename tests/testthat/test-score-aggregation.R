# Aggregazione dei percentili in normal score e indicatori finali.

make_fake_scores <- function() {
  grades <- tibble::tibble(
    student_id = rep(c("S1", "S2", "S3"), each = 2),
    exam_id = rep(c("E1", "E2"), times = 3),
    grade = c(30, 28, 24, 22, 27, 25)
  )
  scores <- grades |>
    dplyr::group_by(exam_id) |>
    dplyr::group_modify(function(g, key) dplyr::bind_cols(g, percentile_table(g$grade))) |>
    dplyr::ungroup()
  scores$p_used <- clip_percentile(scores$p_mid, scores$n_ref)
  scores$z <- qnorm(scores$p_used)
  list(percentile_scores = scores, grades = grades)
}

test_that("nessun infinito nei normal score", {
  fake <- make_fake_scores()
  expect_true(all(is.finite(fake$percentile_scores$z)))
})

test_that("l'aggregazione non dipende dall'ordine degli esami", {
  fake <- make_fake_scores()
  a <- aggregate_student_scores(fake$percentile_scores, fake$grades)
  shuffled <- fake$percentile_scores[sample(nrow(fake$percentile_scores)), ]
  b <- aggregate_student_scores(shuffled, fake$grades)
  a <- a[order(a$student_id), ]
  b <- b[order(b$student_id), ]
  expect_equal(a$aggregate_z, b$aggregate_z)
  expect_equal(a$aggregate_percentile, b$aggregate_percentile)
})

test_that("percentile aggregato nei limiti e decile tra 1 e 10", {
  fake <- make_fake_scores()
  out <- aggregate_student_scores(fake$percentile_scores, fake$grades)
  expect_true(all(out$aggregate_percentile >= 0 & out$aggregate_percentile <= 100))
  expect_true(all(out$decile >= 1 & out$decile <= 10))
  expect_true(is.integer(out$decile))
})

test_that("midrank_percentile gestisce i pari merito con rango medio", {
  p <- midrank_percentile(c(10, 20, 20, 30))
  expect_equal(p, c(0.5 / 4, 2 / 4, 2 / 4, 3.5 / 4))
})

test_that("place_score_in_population colloca senza aggiungere al riferimento", {
  ref <- c(-1, 0, 1)
  out <- place_score_in_population(0.5, ref)
  expect_equal(out$percentile, (2 + 0) / 3 * 100)
  tie <- place_score_in_population(0, ref)
  expect_equal(tie$percentile, (1 + 0.5) / 3 * 100)
  expect_true(is.na(place_score_in_population(0.5, numeric(0))$percentile))
})

test_that("i decili estremi restano in [1, 10]", {
  expect_equal(percentile_to_decile(c(1e-9, 0.05, 0.95, 1 - 1e-12)),
               c(1L, 1L, 10L, 10L))
})
