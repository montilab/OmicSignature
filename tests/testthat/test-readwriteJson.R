test_that("writeJson()/readJson() round trip preserves group_label factor level order", {
  ## Regression test: readJson() used to rebuild group_label via
  ## as.factor(as.character(...)), losing whatever level order the original
  ## object had, for both signature and difexp.
  metadata <- list(
    signature_name = "rt", phenotype = "test",
    organism = predefined_organisms[1], direction_type = "bi-directional",
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
    organism = predefined_organisms[1], direction_type = "uni-directional",
    assay_type = predefined_assaytypes[1], author = "someone"
  )
  signature <- data.frame(feature_name = c("a", "b"), score = c(1, 2))
  capture.output(obj <- OmicSignature$new(metadata = metadata, signature = signature))

  tmpfile <- tempfile(fileext = ".json")
  capture.output(writeJson(obj, tmpfile))
  capture.output(obj2 <- readJson(tmpfile))

  expect_equal(obj2$metadata$signature_name, "rt2")
  expect_equal(obj2$metadata$author, "someone")
  expect_equal(obj2$metadata$direction_type, "uni-directional")
})

test_that("readJson() falls back to positional metadata lookup for files without metadata_fields", {
  ## Simulates a file written before metadata_fields was added, to confirm
  ## old files remain readable.
  raw <- jsonlite::fromJSON(jsonlite::toJSON(list(
    signature_name = "legacy", phenotype = "test",
    organism = predefined_organisms[1], direction_type = "uni-directional",
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
    organism = predefined_organisms[1], direction_type = "bi-directional",
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
