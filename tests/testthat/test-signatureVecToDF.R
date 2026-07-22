test_that("signatureVecToDF() converts a character vector into a feature_name-only dataframe", {
  df <- signatureVecToDF(c("gene1", "gene2", "gene3"))
  expect_equal(colnames(df), c("probe_id", "feature_name"))
  expect_equal(df$feature_name, c("gene1", "gene2", "gene3"))
})

test_that("signatureVecToDF() converts a named numeric vector into a scored dataframe", {
  scores <- c(gene1 = 0.45, gene2 = -3.21, gene3 = 2.44)
  df <- signatureVecToDF(scores)
  expect_equal(colnames(df), c("probe_id", "feature_name", "score"))
  expect_equal(df$feature_name, c("gene1", "gene2", "gene3"))
  expect_equal(df$score, unname(scores))
  expect_false("group_label" %in% colnames(df))
})

test_that("signatureVecToDF() assigns group_label from score sign when group_labels is given", {
  scores <- c(gene1 = 0.45, gene2 = -3.21, gene3 = 2.44)
  df <- signatureVecToDF(scores, group_labels = c("Group1", "Group2"))
  expect_equal(df$group_label, c("Group1", "Group2", "Group1"))
})

test_that("signatureVecToDF() warns when group_labels is not length 2", {
  scores <- c(gene1 = 0.45, gene2 = -3.21)
  expect_warning(
    df <- signatureVecToDF(scores, group_labels = c("only_one")),
    "must be length of 2"
  )
  expect_false("group_label" %in% colnames(df))
})

test_that("signatureVecToDF() errors on invalid input", {
  expect_error(signatureVecToDF(list(1, 2)), "not valid")
  expect_error(signatureVecToDF(c(1, 2, 3)), "not valid")
})
