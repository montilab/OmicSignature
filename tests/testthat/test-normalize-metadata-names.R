test_that(".normalize_metadata_names() renames direction_type to type and warns", {
  input <- list(signature_name = "s1", direction_type = "bi-directional", assay_type = "transcriptomics")
  expect_warning(
    out <- .normalize_metadata_names(input),
    "direction_type.*deprecated"
  )
  expect_equal(out$type, "bi-directional")
  expect_false("direction_type" %in% names(out))
  expect_equal(out$signature_name, "s1")
  expect_equal(out$assay_type, "transcriptomics")
})

test_that(".normalize_metadata_names() leaves a modern metadata list untouched and silent", {
  input <- list(signature_name = "s1", type = "uni-directional")
  expect_silent(out <- .normalize_metadata_names(input))
  expect_identical(out, input)
})

test_that(".normalize_metadata_names() errors when both keys are present", {
  input <- list(signature_name = "s1", type = "uni-directional", direction_type = "bi-directional")
  expect_error(
    .normalize_metadata_names(input),
    "both 'type' and the deprecated 'direction_type'"
  )
})

test_that(".normalize_metadata_names() passes through inputs it cannot normalize", {
  expect_silent(out <- .normalize_metadata_names(list()))
  expect_identical(out, list())
  expect_silent(out2 <- .normalize_metadata_names("not a list"))
  expect_identical(out2, "not a list")
})
