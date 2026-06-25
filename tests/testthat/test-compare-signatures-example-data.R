test_that("compare_signatures_example supports overlap examples", {
  data(compare_signatures_example)

  expect_type(compare_signatures_example, "list")
  expect_named(
    compare_signatures_example,
    c("e7386_hsc3", "e7386_cal27", "icg001_hsc3", "icg001_cal27")
  )

  res <- compare_omic_signatures(
    compare_signatures_example[1:2],
    method = "overlap",
    score_cutoff = log2(1.025),
    adj_p_cutoff = 0.01,
    min_features = 10
  )

  expect_equal(dim(res$comparisons$level1_vs_level1$jaccard), c(2L, 2L))
  expect_equal(unname(diag(res$comparisons$level1_vs_level1$jaccard)), c(1, 1))
})
