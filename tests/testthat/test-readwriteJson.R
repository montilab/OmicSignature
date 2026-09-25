test_that("writeJson()/readJson() round trip preserves group_label factor level order", {
  ## Regression test: readJson() used to rebuild group_label via
  ## as.factor(as.character(...)), losing whatever level order the original
  ## object had, for both signature and difexp.
  metadata <- list(
    signature_name = "rt", phenotype = "test",
    organism = predefined_organisms[1], type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )
  signature <- data.frame(
    probe_id = 1:4, feature_name = c("a", "b", "c", "d"),
    group_label = factor(c("ICG001", "ICG001", "DMSO", "DMSO"), levels = c("ICG001", "DMSO"))
  )
  difexp <- data.frame(
    probe_id = 1:4, feature_name = c("a", "b", "c", "d"), score = c(2, 1, -1, -2),
    p_value = c(0.01, 0.02, 0.03, 0.04),
    group_label = factor(c("ICG001", "ICG001", "DMSO", "DMSO"), levels = c("ICG001", "DMSO"))
  )
  capture.output(obj <- OmicSignature$new(metadata = metadata, signature = signature, difexp = difexp))

  tmpfile <- tempfile(fileext = ".json")
  capture.output(writeJson(obj, tmpfile))
  capture.output(obj2 <- readJson(tmpfile))

  expect_equal(levels(obj2$signature$group_label), c("ICG001", "DMSO"))
  expect_equal(levels(obj2$difexp$group_label), c("ICG001", "DMSO"))
})

test_that("writeJson()/readJson() round trip recovers metadata by name, not position", {
  metadata <- list(
    signature_name = "rt2", phenotype = "test",
    organism = predefined_organisms[1], type = "uni-directional",
    assay_type = predefined_assaytypes[1], author = "someone"
  )
  signature <- data.frame(feature_name = c("a", "b"), score = c(1, 2))
  capture.output(obj <- OmicSignature$new(metadata = metadata, signature = signature))

  tmpfile <- tempfile(fileext = ".json")
  capture.output(writeJson(obj, tmpfile))
  capture.output(obj2 <- readJson(tmpfile))

  expect_equal(obj2$metadata$signature_name, "rt2")
  expect_equal(obj2$metadata$author, "someone")
  expect_equal(obj2$metadata$type, "uni-directional")
})

test_that("readJson() falls back to positional metadata lookup for files without metadata_fields", {
  ## Simulates a file written before metadata_fields was added, to confirm
  ## old files remain readable.
  raw <- jsonlite::fromJSON(jsonlite::toJSON(list(
    signature_name = "legacy", phenotype = "test",
    organism = predefined_organisms[1], type = "uni-directional",
    assay_type = predefined_assaytypes[1],
    metadata_length = 5,
    sig_probe_id = c("p1", "p2"),
    sig_feature_name = c("a", "b"),
    sig_score = c(1, 2)
  ), auto_unbox = TRUE))

  tmpfile <- tempfile(fileext = ".json")
  write(jsonlite::toJSON(raw, na = NULL, pretty = TRUE, auto_unbox = TRUE), tmpfile)

  capture.output(obj <- readJson(tmpfile))
  expect_equal(obj$metadata$signature_name, "legacy")
})

test_that("readJson() correctly reconstructs group_label for files without *_group_label_levels", {
  ## Regression test: factor(x, levels = NULL) is *not* equivalent to
  ## omitting levels (it produces an all-NA factor), so a naive fix that
  ## always passes levels = readJson$sig_group_label_levels would silently
  ## turn every group_label into NA for files written before that field
  ## existed - exactly the files this fallback is supposed to support.
  raw <- jsonlite::fromJSON(jsonlite::toJSON(list(
    signature_name = "legacy_bi", phenotype = "test",
    organism = predefined_organisms[1], type = "bi-directional",
    assay_type = predefined_assaytypes[1],
    metadata_length = 5,
    sig_probe_id = c("p1", "p2", "p3", "p4"),
    sig_feature_name = c("a", "b", "c", "d"),
    sig_score = c(2, 1, -1, -2),
    sig_group_label = c("up", "up", "down", "down")
  ), auto_unbox = TRUE))

  tmpfile <- tempfile(fileext = ".json")
  write(jsonlite::toJSON(raw, na = NULL, pretty = TRUE, auto_unbox = TRUE), tmpfile)

  capture.output(obj <- readJson(tmpfile))
  ## standardizeSigDF() sorts rows by descending |score|, so check group
  ## membership rather than row order.
  expect_false(anyNA(obj$signature$group_label))
  expect_setequal(
    obj$signature$feature_name[obj$signature$group_label == "up"], c("a", "b")
  )
  expect_setequal(
    obj$signature$feature_name[obj$signature$group_label == "down"], c("c", "d")
  )
})

test_that("readJson() loads a legacy file whose metadata uses direction_type", {
  path <- system.file("extdata", "OmS_legacy_direction_type.json", package = "OmicSignature")
  expect_true(file.exists(path))
  warns <- testthat::capture_warnings(capture.output(sig <- readJson(path)))
  expect_true(any(grepl("direction_type.*deprecated", warns)))
  expect_true(inherits(sig, "OmicSignature"))
  expect_equal(sig$metadata$type, "bi-directional")
  expect_false("direction_type" %in% names(sig$metadata))
})

test_that("readJson() loads a legacy file whose metadata_fields names direction_type", {
  ## OmS_legacy_direction_type.json predates metadata_fields entirely, so it
  ## only exercises readJson()'s positional fallback (the `else` branch at
  ## R/readwriteJson.R:78-81). A real 1.3.0-written file DOES carry
  ## metadata_fields (added before the type rename), naming direction_type,
  ## which takes the other branch (readJson[readJson$metadata_fields]).
  ## OmS_legacy_direction_type_with_metadata_fields.json was generated by
  ## calling the real writeJson() on a plain list standing in for a pre-1.4.0
  ## OmicSignature object (metadata keyed by direction_type), so its shape is
  ## a faithful stand-in for that vintage of file rather than hand-typed.
  path <- system.file(
    "extdata", "OmS_legacy_direction_type_with_metadata_fields.json",
    package = "OmicSignature"
  )
  expect_true(file.exists(path))

  raw <- jsonlite::fromJSON(path)
  expect_true("direction_type" %in% raw$metadata_fields)

  warns <- testthat::capture_warnings(capture.output(sig <- readJson(path)))
  expect_true(any(grepl("direction_type.*deprecated", warns)))
  expect_true(inherits(sig, "OmicSignature"))
  expect_equal(sig$metadata$type, "uni-directional")
  expect_false("direction_type" %in% names(sig$metadata))
})

test_that("a legacy file round-trips out under the new field name", {
  path <- system.file("extdata", "OmS_legacy_direction_type.json", package = "OmicSignature")
  capture.output(sig <- suppressWarnings(readJson(path)))
  ## tempfile() rather than withr::local_tempfile(): withr is not in this
  ## package's Suggests, and the rest of this file already uses tempfile().
  out <- tempfile(fileext = ".json")
  ## writeJson() and readJson() both print; the rest of this file wraps them in
  ## capture.output() for that reason. Do not use expect_silent() here.
  capture.output(writeJson(sig, out))
  capture.output(reread <- readJson(out))
  expect_equal(reread$metadata$type, "bi-directional")
  expect_false("direction_type" %in% names(reread$metadata))
})
