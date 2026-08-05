test_that("OmicS_searchAssayType() searches case-insensitively and returns all when x is empty", {
  expect_equal(OmicS_searchAssayType(), predefined_assaytypes)
  result <- OmicS_searchAssayType("TRANSCRIPT")
  expect_true(length(result) >= 1)
  expect_true(all(grepl("transcript", result, ignore.case = TRUE)))
})

test_that("OmicS_searchOrganism() searches case-insensitively and returns all when x is empty", {
  expect_equal(OmicS_searchOrganism(), predefined_organisms)
  result <- OmicS_searchOrganism("homo")
  expect_true(all(grepl("homo", result, ignore.case = TRUE)))
})

test_that("OmicS_searchPlatform() supports contain_all TRUE/FALSE across multiple terms", {
  expect_equal(OmicS_searchPlatform(), predefined_platforms)

  all_result <- OmicS_searchPlatform(c("transcript", "single-cell"), contain_all = TRUE)
  expect_true(all(grepl("transcript", all_result, ignore.case = TRUE)))
  expect_true(all(grepl("single-cell", all_result, ignore.case = TRUE)))

  any_result <- OmicS_searchPlatform(c("transcript", "nonexistentterm12345"), contain_all = FALSE)
  expect_true(length(any_result) >= length(all_result))
})

test_that("OmicS_searchSampleType() supports contain_all TRUE/FALSE and space-separated terms", {
  all_result <- OmicS_searchSampleType("blood plasma", contain_all = TRUE)
  expect_true(is.data.frame(all_result))
  expect_true(all(c("ID", "Name") %in% colnames(all_result)))

  any_result <- OmicS_searchSampleType(c("blood", "nonexistentterm12345"), contain_all = FALSE)
  expect_true(nrow(any_result) >= nrow(all_result))
})

test_that("BRENDAExistName() correctly identifies known and unknown BRENDA terms", {
  expect_true(BRENDAExistName(BRENDA$Name[1]))
  expect_false(BRENDAExistName("definitely not a real BRENDA term 12345"))
})
