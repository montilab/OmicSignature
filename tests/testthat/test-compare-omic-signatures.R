test_that("overlap comparison returns symmetric Jaccard and count matrices", {
  sigs <- make_test_signature_list()

  res <- compare_omic_signatures(
    sigs,
    method = "overlap",
    adj_p_cutoff = 0.05,
    min_features = 1,
    max_feature = 4
  )

  expect_equal(res$method, "overlap")
  expect_named(res$comparisons, c("level1_vs_level1", "level2_vs_level2"))

  positive <- res$comparisons$level1_vs_level1
  expect_equal(dim(positive$jaccard), c(2L, 2L))
  expect_equal(unname(diag(positive$jaccard)), c(1, 1))
  expect_equal(positive$jaccard["sig_a", "sig_b"], 1 / 3)
  expect_equal(positive$jaccard["sig_b", "sig_a"], 1 / 3)
  expect_equal(positive$counts["sig_a", "sig_b"], "2 | 4 | 4")
})

test_that("overlap comparison supports comparing two signature lists", {
  sigs <- make_test_signature_list()

  res <- compare_omic_signatures(
    sig_list1 = sigs[1],
    sig_list2 = sigs[2],
    method = "overlap",
    min_features = 1,
    max_feature = 4
  )

  positive <- res$comparisons$level1_vs_level1
  expect_equal(rownames(positive$jaccard), "sig_a")
  expect_equal(colnames(positive$jaccard), "sig_b")
  expect_equal(positive$jaccard[1, 1], 1 / 3)
  expect_true(length(res$background) >= 8)
})

test_that("KS comparison returns score and p-value matrices", {
  sigs <- make_test_signature_list()

  res <- compare_omic_signatures(
    sigs,
    method = "ks",
    min_features = 1,
    max_feature = 4
  )

  positive <- res$comparisons$level1_vs_level1
  expect_equal(dim(positive$score), c(2L, 2L))
  expect_equal(dim(positive$pvalue), c(2L, 2L))
  expect_true(all(is.finite(positive$pvalue[upper.tri(positive$pvalue)])))
})

test_that("comparison validates cutoffs and signature inputs", {
  sigs <- make_test_signature_list()

  expect_error(
    compare_omic_signatures(sigs, score_cutoff = -1),
    "score_cutoff"
  )
  expect_error(
    compare_omic_signatures(list(not_a_signature = data.frame())),
    "not OmicSignature"
  )
})
