test_that("createMetadata() builds a metadata list with required fields and drops NULLs", {
  metadata <- createMetadata(
    signature_name = "sig1",
    organism = predefined_organisms[2],
    assay_type = predefined_assaytypes[1],
    type = "bi-directional",
    platform = predefined_platforms[2],
    phenotype = "test"
  )
  expect_equal(metadata$signature_name, "sig1")
  expect_equal(metadata$type, "bi-directional")
  expect_false("author" %in% names(metadata))
})

test_that("createMetadata() normalizes shorthand type and assay_type", {
  metadata <- createMetadata(
    signature_name = "sig2",
    organism = predefined_organisms[2],
    assay_type = "gene",
    type = "bi",
    platform = predefined_platforms[2],
    phenotype = "test"
  )
  expect_equal(metadata$type, "bi-directional")
  expect_equal(metadata$assay_type, "transcriptomics")
})

test_that("createMetadata() errors on invalid type", {
  ## type is validated before platform/organism/phenotype, so this
  ## errors before any of those defaults would otherwise warn.
  expect_error(
    createMetadata(
      signature_name = "sig3",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      type = "sideways"
    ),
    "type should be"
  )
})

test_that("createMetadata() requires category_num when type is categorical", {
  expect_error(
    suppressWarnings(createMetadata(
      signature_name = "sig4",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      type = "categorical",
      platform = predefined_platforms[2],
      phenotype = "test"
    )),
    "category_num is not specified"
  )
  metadata <- createMetadata(
    signature_name = "sig5",
    organism = predefined_organisms[2],
    assay_type = predefined_assaytypes[1],
    type = "categorical",
    platform = predefined_platforms[2],
    phenotype = "test",
    category_num = 3
  )
  expect_equal(metadata$category_num, 3)
})

test_that("createMetadata() warns on unrecognized organism, platform, and phenotype", {
  warns <- testthat::capture_warnings(
    createMetadata(
      signature_name = "sig6",
      organism = "not a real organism",
      assay_type = predefined_assaytypes[1],
      type = "uni-directional",
      platform = predefined_platforms[2],
      phenotype = "test"
    )
  )
  expect_true(any(grepl("not in the pre-defined list", warns)))

  warns <- testthat::capture_warnings(
    createMetadata(
      signature_name = "sig7",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      type = "uni-directional",
      platform = "not a real platform",
      phenotype = "test"
    )
  )
  expect_true(any(grepl("not in the pre-defined list", warns)))

  warns <- testthat::capture_warnings(
    createMetadata(
      signature_name = "sig8",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      type = "uni-directional",
      platform = predefined_platforms[2],
      phenotype = NULL
    )
  )
  expect_true(any(grepl("Phenotype information unknown", warns)))
})

test_that("createMetadata() errors when others is not a list", {
  expect_error(
    suppressWarnings(createMetadata(
      signature_name = "sig9",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      type = "uni-directional",
      platform = predefined_platforms[2],
      phenotype = "test",
      others = "not a list"
    )),
    "\"others\" must be a list"
  )
})

test_that("createMetadata() returns the direction under the name 'type'", {
  metadata <- createMetadata(
    signature_name = "sig_type",
    organism = predefined_organisms[2],
    assay_type = predefined_assaytypes[1],
    type = "bi-directional",
    platform = predefined_platforms[2],
    phenotype = "test"
  )
  expect_equal(metadata$type, "bi-directional")
  expect_false("direction_type" %in% names(metadata))
})

test_that("createMetadata() accepts deprecated direction_type and still recodes shorthand", {
  expect_warning(
    metadata <- createMetadata(
      signature_name = "sig_legacy",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      direction_type = "bi",
      platform = predefined_platforms[2],
      phenotype = "test"
    ),
    "'direction_type' argument is deprecated"
  )
  expect_equal(metadata$type, "bi-directional")
  expect_false("direction_type" %in% names(metadata))
})

test_that("createMetadata() errors when both type and direction_type are supplied", {
  expect_error(
    createMetadata(
      signature_name = "sig_both",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      type = "bi-directional",
      direction_type = "bi-directional",
      platform = predefined_platforms[2],
      phenotype = "test"
    ),
    "Supply only 'type'"
  )
})

test_that("createMetadata() errors when type is missing entirely", {
  expect_error(
    createMetadata(
      signature_name = "sig_none",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      platform = predefined_platforms[2],
      phenotype = "test"
    ),
    "'type' is required"
  )
})
