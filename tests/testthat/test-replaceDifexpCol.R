test_that("replaceDifexpCol() renames known aliases and lowercases first", {
  ## Regression test (#66): the recode_values() rewrite lowercases x in a
  ## separate statement before matching, and `default = x` in the
  ## subsequent statement must refer to that already-lowercased vector, not
  ## the original mixed-case input.
  cols <- c("Gene.Symbol", "AveExpr", "Probe.ID", "log2FC", "P.Value", "adj.P.Val", "Q.Value", "T")
  suppressWarnings(result <- replaceDifexpCol(cols))
  expect_equal(
    result,
    c("gene_symbol", "mean", "probe_id", "logfc", "p_value", "adj_p", "q_value", "score")
  )
})

test_that("replaceDifexpCol() passes through unmatched column names unchanged (lowercased)", {
  cols <- c("probe_id", "feature_name", "score", "group_label", "MyCustomColumn")
  suppressWarnings(result <- replaceDifexpCol(cols))
  expect_true("mycustomcolumn" %in% result)
})

test_that("replaceDifexpCol() warns about missing required columns and missing p-value-like columns", {
  expect_warning(
    replaceDifexpCol(c("feature_name", "score", "p_value")),
    "Required column"
  )
  expect_warning(
    replaceDifexpCol(c("probe_id", "feature_name", "score", "group_label")),
    "p_value, q_value, adj_p"
  )
})

test_that("replaceDifexpCol() operates on a data.frame/matrix by renaming its columns and returning it", {
  ## Regression test: replaceDifexpCol() used to compute the renamed column
  ## names but never apply them back to the returned data.frame/matrix,
  ## silently returning the input unchanged.
  df <- data.frame(
    Probe.ID = 1:2, Gene.Symbol = c("a", "b"), T = c(1, -1),
    P.Value = c(0.01, 0.02), group_label = c("x", "y")
  )
  suppressWarnings(result <- replaceDifexpCol(df))
  expect_true(is.data.frame(result))
  expect_equal(colnames(result), c("probe_id", "gene_symbol", "score", "p_value", "group_label"))
  expect_equal(nrow(result), 2)

  mat <- as.matrix(df)
  suppressWarnings(result_mat <- replaceDifexpCol(mat))
  expect_equal(colnames(result_mat), c("probe_id", "gene_symbol", "score", "p_value", "group_label"))
})

test_that("replaceDifexpCol() errors on non-character, non-matrix, non-data.frame input", {
  expect_error(replaceDifexpCol(list(1, 2)), "should be the column names")
})
