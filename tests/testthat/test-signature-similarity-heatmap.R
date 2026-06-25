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
