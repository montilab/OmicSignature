test_that("OmicSigFromDifexp() errors when no criteria are available", {
  difexp <- data.frame(
    probe_id = paste0("p", 1:4),
    feature_name = c("a", "b", "c", "d"),
    score = c(2, -2, 1, -1),
    p_value = c(0.01, 0.02, 0.03, 0.04),
    group_label = factor(c("up", "down", "up", "down"), levels = c("up", "down"))
  )
  metadata <- list(
    signature_name = "no_criteria", phenotype = "test",
    organism = predefined_organisms[1], type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )

  ## Regression test (#63): without an explicit `criteria` string or at
  ## least one metadata cutoff field, OmicSigFromDifexp() used to silently
  ## keep every row of difexp as the signature instead of erroring.
  expect_error(
    OmicSigFromDifexp(difexp, metadata),
    "No filtering criteria available"
  )
  expect_error(
    OmicSigFromDifexp(difexp, metadata, criteria = "   "),
    "No filtering criteria available"
  )
})

test_that("OmicSigFromDifexp() builds a signature from an explicit criteria string", {
  difexp <- data.frame(
    probe_id = paste0("p", 1:4),
    feature_name = c("a", "b", "c", "d"),
    score = c(5, -5, 1, -1),
    p_value = c(0.001, 0.001, 0.5, 0.5),
    group_label = factor(c("up", "down", "up", "down"), levels = c("up", "down"))
  )
  metadata <- list(
    signature_name = "explicit_criteria", phenotype = "test",
    organism = predefined_organisms[1], type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )

  capture.output(
    sig <- OmicSigFromDifexp(difexp, metadata, criteria = "p_value < 0.01")
  )
  expect_setequal(sig$signature$feature_name, c("a", "b"))
})

test_that("OmicSigFromDifexp() derives criteria from metadata cutoff fields", {
  difexp <- data.frame(
    probe_id = paste0("p", 1:4),
    feature_name = c("a", "b", "c", "d"),
    score = c(5, -5, 1, -1),
    adj_p = c(0.001, 0.001, 0.5, 0.5),
    group_label = factor(c("up", "down", "up", "down"), levels = c("up", "down"))
  )
  metadata <- list(
    signature_name = "metadata_criteria", phenotype = "test",
    organism = predefined_organisms[1], type = "bi-directional",
    assay_type = predefined_assaytypes[1],
    score_cutoff = 3, adj_p_cutoff = 0.01
  )

  capture.output(
    sig <- OmicSigFromDifexp(difexp, metadata)
  )
  expect_setequal(sig$signature$feature_name, c("a", "b"))
})

test_that("OmicSigFromDifexp() derives group_label from score sign when missing", {
  difexp <- data.frame(
    probe_id = paste0("p", 1:4),
    feature_name = c("a", "b", "c", "d"),
    score = c(5, -5, 1, -1),
    p_value = c(0.001, 0.001, 0.5, 0.5)
  )
  metadata <- list(
    signature_name = "derived_group_label", phenotype = "test",
    organism = predefined_organisms[1], type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )

  capture.output(
    sig <- OmicSigFromDifexp(difexp, metadata, criteria = "p_value < 0.01")
  )
  expect_setequal(as.character(sig$signature$group_label), c("+", "-"))
})

test_that("OmicSigFromDifexp() errors when group_label and score are both missing for bi-directional", {
  difexp <- data.frame(
    probe_id = paste0("p", 1:2),
    feature_name = c("a", "b"),
    p_value = c(0.001, 0.5)
  )
  metadata <- list(
    signature_name = "no_score_no_group", phenotype = "test",
    organism = predefined_organisms[1], type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )

  expect_error(
    OmicSigFromDifexp(difexp, metadata, criteria = "p_value < 0.01"),
    "group_label or score"
  )
})

test_that("OmicSigFromDifexp() accepts legacy direction_type metadata and warns exactly once", {
  ## Regression test: OmicSigFromDifexp() used to read `metadata$type`
  ## directly, before OmicSignature$new() (and its checkMetadata() ->
  ## .normalize_metadata_names() shim) ever saw the metadata. With a legacy
  ## metadata list, that made signatureType NULL, and
  ## `if (signatureType == "bi-directional")` died with "argument is of
  ## length zero" -- naming neither `type` nor `direction_type`. This is the
  ## same bypass already fixed in initialize(), mechanically reintroduced
  ## here when the field was renamed.
  difexp <- data.frame(
    probe_id = paste0("p", 1:4),
    feature_name = c("a", "b", "c", "d"),
    score = c(5, -5, 1, -1),
    p_value = c(0.001, 0.001, 0.5, 0.5),
    group_label = factor(c("up", "down", "up", "down"), levels = c("up", "down"))
  )
  legacy_metadata <- list(
    signature_name = "legacy_from_difexp", phenotype = "test",
    organism = predefined_organisms[1], direction_type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )

  ## capture_warnings() rather than expect_warning(): if the normalization
  ## happened twice (e.g. once here and once inside $new()), or if some
  ## other unrelated checkMetadata() warning crept in, this fails.
  warns <- testthat::capture_warnings(
    capture.output(
      sig <- OmicSigFromDifexp(difexp, legacy_metadata, criteria = "p_value < 0.01")
    )
  )
  expect_length(warns, 1)
  expect_true(any(grepl("direction_type.*deprecated", warns)))
  expect_equal(sig$metadata$type, "bi-directional")
  expect_false("direction_type" %in% names(sig$metadata))
  expect_setequal(sig$signature$feature_name, c("a", "b"))
})

test_that("OmicSigFromDifexp() and OmicSignature$extractSignature() agree on retained rows", {
  ## Regression test (#64): both entry points now share
  ## .extract_signature_rows(), so filtering the same difexp table with the
  ## same criteria should retain the same features either way.
  difexp <- data.frame(
    probe_id = paste0("p", 1:5),
    feature_name = c("a", "b", "b", "c", "d"),
    score = c(5, -3, -3, 1, 6),
    p_value = c(0.001, 0.01, 0.01, 0.5, 0.001),
    group_label = factor(c("up", "down", "down", "up", "up"), levels = c("up", "down"))
  )
  metadata <- list(
    signature_name = "cross_check", phenotype = "test",
    organism = predefined_organisms[1], type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )

  capture.output(
    sig_from_difexp <- OmicSigFromDifexp(difexp, metadata, criteria = "p_value < 0.1")
  )
  capture.output(
    sig <- OmicSignature$new(metadata = metadata, signature = difexp, difexp = difexp)
  )
  via_method <- sig$extractSignature("p_value < 0.1")

  expect_setequal(sig_from_difexp$signature$feature_name, via_method$feature_name)
})
