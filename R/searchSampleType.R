#' @title search for a valid BRENDA ontology term to use for sample type
#' @description updated 02/2024. Using BRENDA ontology version 2021.
#' @param x A name to search for
#' @param file The BRENDA data frame, has columns ID and Name
#' @return TRUE or FALSE
#' @examples
#' BRENDAExistName("blood plasma")
#' BRENDAExistName("random tissue")
#' @export
BRENDAExistName <- function(x, file = BRENDA) {
  if (x %in% file$Name) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' @title OmicS_searchSampleType
#' @description search for BRENDA tissue name
#' updated 02/2024
#' @param x A string or character vector to search for (case-insensitive).
#' Multiple search terms can be separated by space, or input them as a character vector.
#' @param file The BRENDA data frame, has columns ID and Name.
#' @param contain_all if TRUE, will only return the results contain all search terms.
#' if FALSE, will return results contain any of the given pattern.
#' @return matrix including search result
#' @examples
#' # search for results that contain all of "HEK" "293" and "T":
#' OmicS_searchSampleType("HEK 293 T", contain_all = TRUE)
#' OmicS_searchSampleType(c("HEK", "293", "T"), contain_all = TRUE)
#' OmicS_searchSampleType(c("HEK 293", "T"), contain_all = TRUE)
#'
#' # search for results that contain any of "HEK", "SUM" or "HeLa":
#' OmicS_searchSampleType("HEK SUM HeLa", contain_all = FALSE)
#' OmicS_searchSampleType(c("HEK", "SUM", "HeLa"), contain_all = FALSE)
#' OmicS_searchSampleType(c("HEK SUM", "HeLa"), contain_all = FALSE)
#'
#' @export
OmicS_searchSampleType <- function(x, file = BRENDA, contain_all = TRUE) {
  x <- unlist(strsplit(x, split = " "))
  if (contain_all) {
    result <- c(1:nrow(file))
    for (i in x) {
      result0 <- grep(i, file$Name, ignore.case = TRUE)
      result <- intersect(result0, result)
    }
  } else {
    result <- numeric(0)
    for (i in x) {
      result0 <- grep(i, file$Name, ignore.case = TRUE)
      result <- union(result0, result)
    }
  }
  result <- file[result, ]
  return(result)
}
