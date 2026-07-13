# Recovery metrics: how well each indicator recovers the simulated ability theta.
#
# All estimates are compared with the EMPIRICALLY standardized theta (z-score of
# theta within the simulated population), so that every quantity lives on a
# comparable standardized scale. The transformation of each estimate is documented
# in METHODS.md; none of them is labelled generically as "estimated ability".

#' Metrics for one estimate vector against standardized theta.
#'
#' @return one-row tibble of metrics (all finite for non-degenerate inputs).
recovery_metrics_one <- function(estimate, theta_std, max_exact_pairs = 500000) {
  ok <- is.finite(estimate) & is.finite(theta_std)
  est <- estimate[ok]
  th <- theta_std[ok]
  n <- length(est)
  if (n < 3 || stats::sd(est) == 0 || stats::sd(th) == 0) {
    return(tibble::tibble(
      n = n, pearson = NA_real_, spearman = NA_real_, rmse = NA_real_,
      mae = NA_real_, bias = NA_real_, top10_overlap = NA_real_,
      top10_sensitivity = NA_real_, top10_precision = NA_real_,
      concordant_pairs = NA_real_
    ))
  }
  top10 <- top_decile_agreement(est, th)
  tibble::tibble(
    n = n,
    pearson = stats::cor(est, th),
    spearman = stats::cor(est, th, method = "spearman"),
    rmse = sqrt(mean((est - th)^2)),
    mae = mean(abs(est - th)),
    bias = mean(est - th),
    top10_overlap = top10$overlap,
    top10_sensitivity = top10$sensitivity,
    top10_precision = top10$precision,
    concordant_pairs = concordant_pair_share(est, th, max_exact_pairs)
  )
}

#' Agreement between true and estimated top 10% (threshold-based sets: ties in the
#' estimate can make the estimated set larger than 10%, which keeps sensitivity
#' and precision distinct).
top_decile_agreement <- function(estimate, theta_std) {
  true_top <- theta_std >= stats::quantile(theta_std, 0.9)
  est_top <- estimate >= stats::quantile(estimate, 0.9)
  inter <- sum(true_top & est_top)
  list(
    overlap = inter / max(sum(true_top | est_top), 1),          # Jaccard
    sensitivity = inter / max(sum(true_top), 1),
    precision = inter / max(sum(est_top), 1)
  )
}

#' Share of correctly ordered pairs (ties in the estimate count 0.5). Exact
#' enumeration for small populations, fixed-size random sample of pairs otherwise.
concordant_pair_share <- function(estimate, theta_std, max_exact_pairs = 500000) {
  n <- length(estimate)
  n_pairs <- n * (n - 1) / 2
  if (n_pairs <= max_exact_pairs) {
    idx <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
    i <- idx[, 1]; j <- idx[, 2]
  } else {
    i <- sample.int(n, max_exact_pairs, replace = TRUE)
    j <- sample.int(n, max_exact_pairs, replace = TRUE)
    keep <- i != j
    i <- i[keep]; j <- j[keep]
  }
  d_theta <- theta_std[i] - theta_std[j]
  d_est <- estimate[i] - estimate[j]
  informative <- d_theta != 0
  agreement <- ifelse(d_est[informative] == 0, 0.5,
                      (sign(d_est[informative]) == sign(d_theta[informative])) * 1)
  mean(agreement)
}

#' Full recovery summary for a scenario.
#'
#' @param student_scores tibble from aggregate_student_scores() (with estimate_* cols).
#' @param students tibble with student_id, course_id, theta.
#' @param enrollments tibble student_id, exam_id (to derive optional-path groups).
#' @param exams tibble with exam_id, choice_group, exam_name.
#' @return list(overall = tibble, by_course = tibble, by_path = tibble, theta_std).
compute_recovery <- function(student_scores, students, enrollments, exams) {
  df <- dplyr::inner_join(student_scores,
                          students[, c("student_id", "course_id", "theta")],
                          by = "student_id")
  df$theta_std <- as.numeric(scale(df$theta))

  methods <- c(
    "Voto medio (standardizzato)" = "estimate_from_mean_grade",
    "Percentili aggregati (normal score)" = "estimate_from_percentiles",
    "Media ingenua dei percentili" = "estimate_from_naive_mean"
  )
  overall <- dplyr::bind_rows(lapply(names(methods), function(label) {
    m <- recovery_metrics_one(df[[methods[[label]]]], df$theta_std)
    dplyr::bind_cols(tibble::tibble(method = label), m)
  }))

  by_course <- df |>
    dplyr::group_by(course_id) |>
    dplyr::group_modify(function(g, key) {
      dplyr::bind_rows(lapply(names(methods)[1:2], function(label) {
        # Within-course comparison keeps the pooled estimates but re-standardizes theta.
        m <- recovery_metrics_one(g[[methods[[label]]]], as.numeric(scale(g$theta)))
        dplyr::bind_cols(tibble::tibble(method = label),
                         m[, c("n", "pearson", "spearman", "rmse")])
      }))
    }) |>
    dplyr::ungroup()

  by_path <- recovery_by_path(df, enrollments, exams, methods[1:2])

  list(overall = overall, by_course = by_course, by_path = by_path,
       theta_std = df[, c("student_id", "theta_std")])
}

#' Recovery by optional path (signature of chosen alternatives within each course).
recovery_by_path <- function(df, enrollments, exams, methods) {
  optional <- exams[!is.na(exams$choice_group), c("exam_id", "exam_name")]
  if (nrow(optional) == 0) {
    return(tibble::tibble(course_id = character(0), path = character(0),
                          method = character(0), n = integer(0),
                          pearson = numeric(0), rmse = numeric(0)))
  }
  chosen <- dplyr::inner_join(enrollments, optional, by = "exam_id") |>
    dplyr::group_by(student_id) |>
    dplyr::summarise(path = paste(sort(exam_name), collapse = " + "), .groups = "drop")
  dfp <- dplyr::inner_join(df, chosen, by = "student_id")
  dfp |>
    dplyr::group_by(course_id, path) |>
    dplyr::group_modify(function(g, key) {
      dplyr::bind_rows(lapply(names(methods), function(label) {
        m <- recovery_metrics_one(g[[methods[[label]]]], as.numeric(scale(g$theta)))
        tibble::tibble(method = label, n = m$n, pearson = m$pearson, rmse = m$rmse)
      }))
    }) |>
    dplyr::ungroup()
}
