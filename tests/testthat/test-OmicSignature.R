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

test_that("auto-generated probe_id is unique when difexp is NULL", {
  ## Regression test: the difexp = NULL branch had seq() and paste0()
  ## transposed -- seq(paste0("feature_", nrow(signature))) collapses to
  ## seq("feature_N") == 1, assigning probe_id = "1" to every row. The object
  ## still built and validated, so its rows were mutually indistinguishable by
  ## probe. Curated / membership-only signatures (which take difexp = NULL) were
  ## the ones affected.
  metadata <- list(
    signature_name = "no_difexp",
    phenotype = "test",
    organism = predefined_organisms[1],
    direction_type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )
  signature <- data.frame(
    feature_name = c("A", "B", "C", "D"),
    score = c(2, 1, -1, -2),
    group_label = factor(c("up", "up", "down", "down"), levels = c("up", "down")),
    stringsAsFactors = FALSE
  )

  capture.output(
    sig <- OmicSignature$new(metadata = metadata, signature = signature, difexp = NULL)
  )

  expect_false(any(duplicated(sig$signature$probe_id)))
  expect_true(all(grepl("^feature_[0-9]+$", sig$signature$probe_id)))
  expect_equal(length(unique(sig$signature$probe_id)), nrow(signature))
})
