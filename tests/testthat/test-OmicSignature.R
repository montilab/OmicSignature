test_that("signature and difexp active bindings accept updates after construction", {
  sig <- make_test_signature(
    "sig_a",
    positive_features = c("A", "B"), negative_features = c("W", "X"),
    positive_scores = c(2, 1), negative_scores = c(-2, -1)
  )

  ## Regression test: the signature/difexp setters used to pass
  ## print_message positionally where checkSignature()/checkDifexp() expect
  ## signatureType, so any post-construction assignment to $signature failed
  ## with "Signature type not specified" (and $difexp silently mis-validated
  ## uni-directional signatures).
  expect_no_error(sig$signature <- sig$signature)
  expect_no_error(sig$difexp <- sig$difexp)

  relabeled <- sig$signature
  relabeled$group_label <- factor(as.character(relabeled$group_label), levels = c("down", "up"))
  sig$signature <- relabeled
  expect_equal(levels(sig$signature$group_label), c("down", "up"))
})

test_that("checkMetadata() gives descriptive errors for invalid optional fields", {
  base_metadata <- list(
    signature_name = "t", phenotype = "test",
    organism = predefined_organisms[1], direction_type = "uni-directional",
    assay_type = predefined_assaytypes[1]
  )
  signature <- data.frame(feature_name = "a", score = 1)

  ## Regression test: these used to raise a generic stopifnot() error instead
  ## of a descriptive message explaining what's expected.
  expect_error(
    OmicSignature$new(metadata = modifyList(base_metadata, list(covariates = 5)), signature = signature),
    "covariates must be a character vector"
  )
  expect_error(
    OmicSignature$new(metadata = modifyList(base_metadata, list(keywords = 5)), signature = signature),
    "keywords must be a character vector"
  )
  expect_error(
    OmicSignature$new(metadata = modifyList(base_metadata, list(PMID = 123)), signature = signature),
    "PMID must be a character value"
  )
  expect_error(
    OmicSignature$new(metadata = modifyList(base_metadata, list(PMID = c("1", "2"))), signature = signature),
    "PMID must be a single-length character value"
  )
  expect_error(
    OmicSignature$new(metadata = modifyList(base_metadata, list(description = 5)), signature = signature),
    "description must be a character value"
  )
})

test_that("checkDifexp() does not require group_label for uni-directional signatures", {
  metadata <- list(
    signature_name = "u", phenotype = "test",
    organism = predefined_organisms[1], direction_type = "uni-directional",
    assay_type = predefined_assaytypes[1]
  )
  signature <- data.frame(feature_name = c("A", "B"), score = c(1, 2))
  difexp <- data.frame(
    probe_id = 1:2, feature_name = c("A", "B"), score = c(1, 2), p_value = c(0.01, 0.02)
  )

  ## Regression test: checkDifexp()'s required-columns list included
  ## group_label unconditionally, contradicting checkSignature()'s handling
  ## of the signature table and forcing a meaningless placeholder column.
  expect_no_error(
    capture.output(
      sig <- OmicSignature$new(metadata = metadata, signature = signature, difexp = difexp)
    )
  )
  expect_false("group_label" %in% colnames(sig$difexp))
})

test_that("metadata<- re-validates signature/difexp when direction_type changes", {
  bi_metadata <- list(
    signature_name = "bi", phenotype = "test",
    organism = predefined_organisms[1], direction_type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )
  bi_signature <- data.frame(
    probe_id = 1:4, feature_name = c("a", "b", "c", "d"),
    group_label = factor(c("up", "up", "down", "down"), levels = c("up", "down"))
  )
  capture.output(bi <- OmicSignature$new(metadata = bi_metadata, signature = bi_signature))

  ## Regression test: demoting direction_type to uni-directional used to
  ## succeed silently even though signature still had a multi-level
  ## group_label column, which compare_omic_signatures() would then ignore
  ## entirely based on metadata$direction_type alone.
  expect_error(
    bi$metadata <- modifyList(bi_metadata, list(direction_type = "uni-directional")),
    "multi-level group_label"
  )
  expect_equal(bi$metadata$direction_type, "bi-directional")

  uni_metadata <- list(
    signature_name = "u", phenotype = "test",
    organism = predefined_organisms[1], direction_type = "uni-directional",
    assay_type = predefined_assaytypes[1]
  )
  uni_signature <- data.frame(feature_name = c("a", "b"), score = c(1, 2))
  capture.output(uni <- OmicSignature$new(metadata = uni_metadata, signature = uni_signature))

  ## Promoting to bi-directional without a group_label column should also
  ## be rejected (re-validated via checkSignature()).
  expect_error(
    uni$metadata <- modifyList(uni_metadata, list(direction_type = "bi-directional")),
    "group_label"
  )

  ## An unrelated metadata change (same direction_type) is unaffected.
  capture.output(bi$metadata <- modifyList(bi_metadata, list(author = "someone")))
  expect_equal(bi$metadata$author, "someone")
})
