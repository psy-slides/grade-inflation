# Metriche di recovery dell'abilita' simulata.

test_that("le metriche di recovery sono finite in uno scenario standard", {
  scenario <- build_scenario(small_config())
  overall <- scenario$recovery$overall
  metric_cols <- c("pearson", "spearman", "rmse", "mae", "bias",
                   "top10_overlap", "top10_sensitivity", "top10_precision",
                   "concordant_pairs")
  for (col in metric_cols) {
    expect_true(all(is.finite(overall[[col]])), info = col)
  }
  expect_true(all(overall$pearson > 0))  # gli indicatori restano informativi
})

test_that("recovery_metrics_one gestisce input degeneri senza errori", {
  constant <- recovery_metrics_one(rep(1, 50), rnorm(50))
  expect_true(is.na(constant$pearson))
  tiny <- recovery_metrics_one(c(1, 2), c(1, 2))
  expect_true(is.na(tiny$pearson))
})

test_that("la quota di coppie concordanti e' corretta nei casi noti", {
  th <- c(-2, -1, 0, 1, 2)
  expect_equal(concordant_pair_share(th, th), 1)
  expect_equal(concordant_pair_share(-th, th), 0)
  # Stima completamente costante: tutte le coppie contano 0.5.
  expect_equal(concordant_pair_share(rep(0, 5), th), 0.5)
})

test_that("l'accordo sul top 10% e' perfetto quando la stima replica theta", {
  th <- seq(-3, 3, length.out = 200)
  agree <- top_decile_agreement(th, th)
  expect_equal(agree$sensitivity, 1)
  expect_equal(agree$precision, 1)
  expect_equal(agree$overlap, 1)
})

test_that("recovery per CdS e per percorso opzionale sono disponibili", {
  scenario <- build_scenario(small_config())
  expect_gt(nrow(scenario$recovery$by_course), 0)
  expect_gt(nrow(scenario$recovery$by_path), 0)
  expect_true(all(c("course_id", "method", "pearson") %in%
                    names(scenario$recovery$by_course)))
})

test_that("esami con N molto piccolo non producono errori nei percentili", {
  tiny <- percentile_table(c(27L, 27L))
  expect_equal(tiny$p_mid, c(0.5, 0.5))
  z <- percentile_to_normal_score(tiny$p_mid, tiny$n_ref)
  expect_true(all(is.finite(z)))
})
