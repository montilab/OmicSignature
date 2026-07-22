test_that("standardizeSigDF() preserves caller-specified group_label factor level order", {
  ## Regression test: standardizeSigDF() used to rebuild group_label via
  ## as.factor(as.character(...)), which silently reset the level order to
  ## alphabetical regardless of how the caller constructed it.
  df <- data.frame(
    probe_id = 1:4, feature_name = c("a", "b", "c", "d"),
    group_label = factor(c("ICG001", "ICG001", "DMSO", "DMSO"), levels = c("ICG001", "DMSO"))
  )
  result <- standardizeSigDF(df)
  expect_equal(levels(result$group_label), c("ICG001", "DMSO"))
})

test_that("standardizeSigDF() still drops group_label levels with no observed rows", {
  ## A signature pre-filtered to one group upstream shouldn't retain the
  ## other, unobserved level, even though its order-preserving fix now keeps
  ## the *order* of whatever levels remain.
  df <- data.frame(
    probe_id = 1:2, feature_name = c("a", "b"),
    group_label = factor(c("ICG001", "ICG001"), levels = c("ICG001", "DMSO"))
  )
  result <- standardizeSigDF(df)
  expect_equal(levels(result$group_label), "ICG001")
})

test_that("standardizeSigDF() preserves declared order even when a level is filtered out", {
  df <- data.frame(
    probe_id = 1:4, feature_name = c("a", "b", "c", "d"),
    group_label = factor(c("DMSO", "DMSO", "ICG001", "ICG001"), levels = c("DMSO", "ICG001"))
  )
  filtered <- df[df$group_label == "ICG001", ]
  result <- standardizeSigDF(filtered)
  expect_equal(levels(result$group_label), "ICG001")
})

test_that("standardizeSigDF() falls back to first-appearance order for non-factor group_label", {
  df <- data.frame(
    probe_id = 1:4, feature_name = c("a", "b", "c", "d"),
    group_label = c("ICG001", "ICG001", "DMSO", "DMSO"),
    stringsAsFactors = FALSE
  )
  result <- standardizeSigDF(df)
  expect_equal(levels(result$group_label), c("ICG001", "DMSO"))
})
