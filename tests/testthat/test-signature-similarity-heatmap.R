.legend_titles <- function(legends) {
  vapply(legends, function(legend) {
    text_children <- vapply(
      legend@grob$children,
      function(child) inherits(child, "text"),
      logical(1)
    )
    legend@grob$children[[which(text_children)[1]]]$label
  }, character(1))
}

test_that("similarity extraction validates named matrices", {
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
    "row and column names"
  )
})

test_that("similarity extraction supports cross-list matrices", {
  cross_mat <- matrix(
    c(0.2, 0.6, 0.4, 0.8),
    nrow = 2,
    dimnames = list(c("query_a", "query_b"), c("ref_a", "ref_b"))
  )
  comparison <- list(
    method = "overlap",
    comparisons = list(
      level1_vs_level1 = list(jaccard = cross_mat),
      level2_vs_level2 = list(jaccard = cross_mat / 2)
    )
  )

  sim <- OmicSignature:::.ssh_similarity_from_comparison(comparison, "jaccard")

  expect_equal(rownames(sim$positive), c("query_a", "query_b"))
  expect_equal(colnames(sim$positive), c("ref_a", "ref_b"))
  expect_equal(sim$negative["query_b", "ref_b"], 0.4)
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

test_that("combined heatmap supports two-list overlap comparisons", {
  skip_if_not_installed("ComplexHeatmap")
  skip_if_not_installed("circlize")

  cross_level1 <- matrix(
    c(0.2, 0.6, 0.4, 0.8),
    nrow = 2,
    dimnames = list(c("query_a", "query_b"), c("ref_a", "ref_b"))
  )
  cross_level2 <- matrix(
    c(0.3, 0.7, 0.5, 0.9),
    nrow = 2,
    dimnames = list(c("query_a", "query_b"), c("ref_a", "ref_b"))
  )
  comparison <- list(
    method = "overlap",
    comparisons = list(
      level1_vs_level1 = list(jaccard = cross_level1),
      level2_vs_level2 = list(jaccard = cross_level2)
    )
  )

  combined_ht <- signature_similarity_heatmap(comparison, mode = "combined", draw = FALSE)
  separate_ht <- signature_similarity_heatmap(comparison, mode = "separate", draw = FALSE)

  expect_s4_class(combined_ht, "Heatmap")
  expect_s4_class(separate_ht, "HeatmapList")
  expect_equal(unname(sort(combined_ht@row_order)), seq_len(2))
  expect_equal(unname(sort(combined_ht@column_order)), seq_len(2))
  expect_error(
    signature_similarity_heatmap(comparison, mode = "split", draw = FALSE),
    "square self-comparison"
  )
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
  expect_equal(separate_ht@ht_list[[1]]@name, "lev1_vs_lev1")
  expect_equal(separate_ht@ht_list[[2]]@name, "lev2_vs_lev2")
  expect_equal(separate_ht@ht_list[[1]]@column_title, "lev1_vs_lev1")
  expect_equal(separate_ht@ht_list[[2]]@column_title, "lev2_vs_lev2")
  expect_error(
    signature_similarity_heatmap(comparison, measure = "score", mode = "split", draw = FALSE),
    "not supported for rank-based"
  )
})

test_that("rank-based combined heatmap overlays full separate matrices", {
  skip_if_not_installed("ComplexHeatmap")
  skip_if_not_installed("circlize")

  score_level1 <- matrix(
    c(1, 4, 2, 3),
    nrow = 2,
    dimnames = list(c("a", "b"), c("a", "b"))
  )
  score_level2 <- matrix(
    c(10, 40, 20, 30),
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

  seen <- character()
  pos_col_fun <- function(x) {
    seen <<- c(seen, paste0("level1:", x))
    "#000000"
  }
  neg_col_fun <- function(x) {
    seen <<- c(seen, paste0("level2:", x))
    "#111111"
  }
  combined_ht <- signature_similarity_heatmap(
    comparison,
    measure = "score",
    mode = "combined",
    draw = FALSE,
    pos_col_fun = pos_col_fun,
    neg_col_fun = neg_col_fun
  )
  seen <- character()
  grid::grid.newpage()
  combined_ht@matrix_param$cell_fun(
    1, 2,
    grid::unit(0.5, "npc"), grid::unit(0.5, "npc"),
    grid::unit(1, "npc"), grid::unit(1, "npc"),
    NA
  )

  averaged_ht <- signature_similarity_heatmap(
    comparison,
    measure = "score",
    mode = "combined",
    combined_triangle_threshold = 0,
    draw = FALSE
  )

  expect_equal(seen, c("level2:40", "level1:4"))
  expect_equal(averaged_ht@matrix, (score_level1 + score_level2) / 2)
})

test_that("combined heatmap adds abbreviated legends for distinct color scales", {
  skip_if_not_installed("ComplexHeatmap")
  skip_if_not_installed("circlize")

  score_level1 <- matrix(
    c(1, 4, 2, 3),
    nrow = 2,
    dimnames = list(c("a", "b"), c("a", "b"))
  )
  score_level2 <- matrix(
    c(10, 40, 20, 30),
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

  combined_ht <- signature_similarity_heatmap(
    comparison,
    measure = "score",
    mode = "combined",
    draw = FALSE,
    pos_col_fun = circlize::colorRamp2(c(1, 4), c("white", "red")),
    neg_col_fun = circlize::colorRamp2(c(10, 40), c("white", "blue"))
  )
  legends <- attr(combined_ht, "heatmap_legend_list")

  expect_length(legends, 2)
  expect_equal(.legend_titles(legends), c("lev2_vs_lev2", "lev1_vs_lev1"))
})
