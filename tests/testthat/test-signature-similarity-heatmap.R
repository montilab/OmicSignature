test_that("similarity extraction validates square named matrices", {
  comparison <- list(
    comparisons = list(
      positive = list(
        jaccard = matrix(
          c(1, 0.5, 0.5, 1),
          nrow = 2,
          dimnames = list(c("a", "b"), c("a", "b"))
        )
      )
    )
  )

  sim <- OmicSignature:::.ssh_similarity_from_comparison(comparison, "jaccard")
  expect_equal(names(sim), "positive")
  expect_equal(sim$positive["a", "b"], 0.5)

  comparison$comparisons$positive$jaccard <- matrix(1, nrow = 1, ncol = 2)
  expect_error(
    OmicSignature:::.ssh_similarity_from_comparison(comparison, "jaccard"),
    "square matrix"
  )
})

test_that("similarity extraction supports rank-based score matrices", {
  score_level1 <- matrix(
    c(1, 2, 3, 4),
    nrow = 2,
    dimnames = list(c("a", "b"), c("a", "b"))
  )
  score_level2 <- matrix(
    c(5, 6, 7, 8),
    nrow = 2,
    dimnames = list(c("a", "b"), c("a", "b"))
  )
  comparison <- list(
    method = "ks",
    comparisons = list(
      level1_vs_level1 = list(score = score_level1, pvalue = score_level1 / 10),
      level2_vs_level2 = list(score = score_level2, pvalue = score_level2 / 10)
    )
  )

  sim <- OmicSignature:::.ssh_similarity_from_comparison(comparison, "score")
  expect_equal(attr(sim, "comparison_method"), "ks")
  expect_equal(sim$positive["a", "b"], 3)
  expect_equal(sim$negative["a", "b"], 7)
  expect_error(
    OmicSignature:::.ssh_similarity_from_comparison(comparison, "jaccard"),
    "only available for overlap"
  )
})

test_that("heatmap function returns a ComplexHeatmap object when suggested packages are installed", {
  skip_if_not_installed("ComplexHeatmap")
  skip_if_not_installed("circlize")

  comparison <- list(
    comparisons = list(
      positive = list(
        jaccard = matrix(
          c(1, 0.5, 0.5, 1),
          nrow = 2,
          dimnames = list(c("a", "b"), c("a", "b"))
        )
      )
    )
  )

  ht <- signature_similarity_heatmap(comparison, draw = FALSE)
  expect_s4_class(ht, "Heatmap")
})

test_that("rank-based heatmaps support combined and separate modes only", {
  skip_if_not_installed("ComplexHeatmap")
  skip_if_not_installed("circlize")

  score_level1 <- matrix(
    c(1, 4, 2, 3),
    nrow = 2,
    dimnames = list(c("a", "b"), c("a", "b"))
  )
  score_level2 <- matrix(
    c(-1, -4, -2, -3),
    nrow = 2,
    dimnames = list(c("a", "b"), c("a", "b"))
  )
  comparison <- list(
    method = "ks",
    comparisons = list(
      level1_vs_level1 = list(score = score_level1, pvalue = abs(score_level1) / 10),
      level2_vs_level2 = list(score = score_level2, pvalue = abs(score_level2) / 10)
    )
  )

  combined_ht <- signature_similarity_heatmap(
    comparison,
    measure = "score",
    mode = "combined",
    draw = FALSE
  )
  separate_ht <- signature_similarity_heatmap(
    comparison,
    measure = "score",
    mode = "separate",
    draw = FALSE
  )
  averaged_ht <- signature_similarity_heatmap(
    comparison,
    measure = "score",
    mode = "combined",
    combined_triangle_threshold = 0,
    draw = FALSE
  )

  expect_s4_class(combined_ht, "Heatmap")
  expect_s4_class(separate_ht, "HeatmapList")
  expect_s4_class(averaged_ht, "Heatmap")
  expect_error(
    signature_similarity_heatmap(comparison, measure = "score", mode = "split", draw = FALSE),
    "not supported for rank-based"
  )
})
