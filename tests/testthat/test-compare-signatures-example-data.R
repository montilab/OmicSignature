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

test_that("compare_signatures_example has strong KS self-similarity", {
  data(compare_signatures_example)

  res <- compare_omic_signatures(
    compare_signatures_example,
    method = "ks",
    adj_p_cutoff = 0.01
  )

  level1 <- res$comparisons$level1_vs_level1
  expect_equal(unname(diag(level1$score)), unname(apply(level1$score, 1, max)))
  expect_true(all(diag(level1$pvalue) < 1e-10))
})

test_that("compare_label_pairing_example supports explicit label pairing", {
  data(compare_label_pairing_example)

  expect_named(compare_label_pairing_example, c("signature_a", "signature_b", "signature_c"))
  expect_equal(
    vapply(compare_label_pairing_example, function(sig) nrow(sig$signature), integer(1)),
    c(signature_a = 20L, signature_b = 20L, signature_c = 20L)
  )
  expect_equal(
    vapply(compare_label_pairing_example, function(sig) nrow(sig$difexp), integer(1)),
    c(signature_a = 100L, signature_b = 100L, signature_c = 100L)
  )

  paired_res <- compare_omic_signatures(
    compare_label_pairing_example,
    method = "overlap",
    label_pairing = list(
      signature_a = c("treated", "control"),
      signature_b = c("up", "down"),
      signature_c = c("resistant", "sensitive")
    )
  )

  level1 <- paired_res$comparisons$level1_vs_level1$jaccard
  level2 <- paired_res$comparisons$level2_vs_level2$jaccard

  expect_gt(level1["signature_a", "signature_b"], level1["signature_a", "signature_c"])
  expect_gt(level2["signature_a", "signature_b"], level2["signature_a", "signature_c"])
  expect_equal(level1["signature_a", "signature_b"], 8 / 12)
  expect_equal(level1["signature_a", "signature_c"], 5 / 15)
  expect_equal(level2["signature_a", "signature_b"], 8 / 12)
  expect_equal(level2["signature_a", "signature_c"], 5 / 15)
})
