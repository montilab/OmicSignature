test_that("createMetadata() builds a metadata list with required fields and drops NULLs", {
  metadata <- createMetadata(
    signature_name = "sig1",
    organism = predefined_organisms[2],
    assay_type = predefined_assaytypes[1],
    direction_type = "bi-directional",
    platform = predefined_platforms[2],
    phenotype = "test"
  )
  expect_equal(metadata$signature_name, "sig1")
  expect_equal(metadata$direction_type, "bi-directional")
  expect_false("author" %in% names(metadata))
})

test_that("createMetadata() normalizes shorthand direction_type and assay_type", {
  metadata <- createMetadata(
    signature_name = "sig2",
    organism = predefined_organisms[2],
    assay_type = "gene",
    direction_type = "bi",
    platform = predefined_platforms[2],
    phenotype = "test"
  )
  expect_equal(metadata$direction_type, "bi-directional")
  expect_equal(metadata$assay_type, "transcriptomics")
})

test_that("createMetadata() errors on invalid direction_type", {
  ## direction_type is validated before platform/organism/phenotype, so this
  ## errors before any of those defaults would otherwise warn.
  expect_error(
    createMetadata(
      signature_name = "sig3",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      direction_type = "sideways"
    ),
    "direction_type should be"
  )
})

test_that("createMetadata() requires category_num when direction_type is categorical", {
  expect_error(
    suppressWarnings(createMetadata(
      signature_name = "sig4",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      direction_type = "categorical",
      platform = predefined_platforms[2],
      phenotype = "test"
    )),
    "category_num is not specified"
  )
  metadata <- createMetadata(
    signature_name = "sig5",
    organism = predefined_organisms[2],
    assay_type = predefined_assaytypes[1],
    direction_type = "categorical",
    platform = predefined_platforms[2],
    phenotype = "test",
    category_num = 3
  )
  expect_equal(metadata$category_num, 3)
})

test_that("createMetadata() warns on unrecognized organism, platform, and phenotype", {
  expect_warning(
    createMetadata(
      signature_name = "sig6",
      organism = "not a real organism",
      assay_type = predefined_assaytypes[1],
      direction_type = "uni-directional",
      platform = predefined_platforms[2],
      phenotype = "test"
    ),
    "not in the pre-defined list"
  )
  expect_warning(
    createMetadata(
      signature_name = "sig7",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      direction_type = "uni-directional",
      platform = "not a real platform",
      phenotype = "test"
    ),
    "not in the pre-defined list"
  )
  expect_warning(
    createMetadata(
      signature_name = "sig8",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      direction_type = "uni-directional",
      platform = predefined_platforms[2],
      phenotype = NULL
    ),
    "Phenotype information unknown"
  )
})

test_that("createMetadata() errors when others is not a list", {
  expect_error(
    suppressWarnings(createMetadata(
      signature_name = "sig9",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      direction_type = "uni-directional",
      platform = predefined_platforms[2],
      phenotype = "test",
      others = "not a list"
    )),
    "\"others\" must be a list"
  )
})
