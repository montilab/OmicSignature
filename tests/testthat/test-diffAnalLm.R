test_that("diffAnalLm() returns a limma topTable with a probe_id column", {
  set.seed(1)
  n_features <- 20
  n_ctrl <- 4
  n_trt <- 4
  dat <- matrix(
    rnorm(n_features * (n_ctrl + n_trt)),
    nrow = n_features,
    dimnames = list(paste0("probe", seq_len(n_features)), c(paste0("ctrl", 1:n_ctrl), paste0("trt", 1:n_trt)))
  )
  ## Give a handful of features a real treatment effect so the fit isn't degenerate.
  dat["probe1", (n_ctrl + 1):(n_ctrl + n_trt)] <- dat["probe1", (n_ctrl + 1):(n_ctrl + n_trt)] + 5
  dat <- as.data.frame(dat)
  dat$probe_id_col <- rownames(dat)

  result <- diffAnalLm(
    dat,
    ctrl_columns = paste0("ctrl", 1:n_ctrl),
    trt_columns = paste0("trt", 1:n_trt),
    id = "probe_id_col"
  )
  expect_true("probe_id" %in% colnames(result))
  expect_equal(nrow(result), n_features)
  expect_true("probe1" %in% result$probe_id[1:3])
})

test_that("diffAnalLm() accepts a numeric-column id vector and errors on invalid id", {
  set.seed(2)
  n_features <- 10
  dat <- matrix(
    rnorm(n_features * 6),
    nrow = n_features,
    dimnames = list(paste0("probe", seq_len(n_features)), c(paste0("ctrl", 1:3), paste0("trt", 1:3)))
  )
  dat <- as.data.frame(dat)

  result <- diffAnalLm(
    dat,
    ctrl_columns = 1:3, trt_columns = 4:6,
    id = paste0("probe", seq_len(n_features))
  )
  expect_equal(nrow(result), n_features)

  expect_error(
    diffAnalLm(dat, ctrl_columns = 1:3, trt_columns = 4:6, id = c("only_one", "still_not_all")),
    "Input id invalid"
  )
})
