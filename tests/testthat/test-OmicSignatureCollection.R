test_that("OmicSignatureCollection construction requires collection_name and description", {
  sig_list <- make_test_signature_list()

  expect_error(
    OmicSignatureCollection$new(
      metadata = list(collection_name = "coll"),
      OmicSigList = sig_list
    ),
    "description"
  )
  expect_error(
    OmicSignatureCollection$new(
      metadata = list(description = "desc"),
      OmicSigList = sig_list
    ),
    "collection_name"
  )
})

test_that("OmicSignatureCollection construction rejects non-OmicSignature elements", {
  expect_error(
    OmicSignatureCollection$new(
      metadata = list(collection_name = "coll", description = "desc"),
      OmicSigList = list(1, 2)
    ),
    "not OmicSignature objects"
  )
  expect_error(
    OmicSignatureCollection$new(
      metadata = list(collection_name = "coll", description = "desc"),
      OmicSigList = list()
    )
  )
})

test_that("OmicSignatureCollection names its list by signature_name and supports print/extractSignature/metadataSummary", {
  sig_list <- make_test_signature_list()
  capture.output(
    coll <- OmicSignatureCollection$new(
      metadata = list(collection_name = "coll", description = "desc"),
      OmicSigList = sig_list
    )
  )
  expect_equal(names(coll$OmicSigList), c("sig_a", "sig_b"))

  out <- capture.output(coll$print())
  expect_true(any(grepl("Signature Collection", out, fixed = TRUE)))

  bound <- coll$extractSignature("p_value < 1")
  expect_true(is.data.frame(bound))
  expect_true("sig_name" %in% colnames(bound))

  unbound <- coll$extractSignature("p_value < 1", bind = FALSE)
  expect_type(unbound, "list")
  expect_equal(names(unbound), c("sig_a", "sig_b"))

  summ <- coll$metadataSummary()
  expect_true(is.matrix(summ))
  expect_equal(unname(colnames(summ)), c("sig_a", "sig_b"))
})

test_that("OmicSignatureCollection$metadata<- re-validates and OmicSigList<- re-checks elements", {
  sig_list <- make_test_signature_list()
  capture.output(
    coll <- OmicSignatureCollection$new(
      metadata = list(collection_name = "coll", description = "desc"),
      OmicSigList = sig_list
    )
  )

  coll$metadata <- list(collection_name = "coll2", description = "desc2")
  expect_equal(coll$metadata$collection_name, "coll2")

  expect_error(coll$metadata <- list(collection_name = "coll3"), "description")

  expect_error(coll$OmicSigList <- list(1, 2), "not OmicSignature objects")
})
