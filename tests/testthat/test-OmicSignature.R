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
