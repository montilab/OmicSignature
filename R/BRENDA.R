#' @title BRENDAExistName
#' @description search if a string is a valid BRENDA tissue name
#' updated 02/2024
#' @param x A name to search for
#' @param file The BRENDA data frame, has columns ID and Name
#' @return TRUE or FALSE
#' @examples
#' BRENDAExistName("blood plasma")
#' BRENDAExistName("random tissue")
#' @export
BRENDAExistName <- function(x, file = BRENDA) {
  if (x %in% BRENDA$Name) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' @title BRENDASearch
#' @description search for BRENDA tissue name
#' updated 02/2024
#' @param x A string or character vector to search for. Multiple search terms
#' are separated by space.
#' @param file The BRENDA data frame, has columns ID and Name.
#' @param contain_all if TRUE, will only return the results contain all search terms.
#' if FALSE, will return results contain any of the given pattern.
#' @return matrix including search result
#' @examples
#' # search for results that contain all of "HEK" "293" and "T":
#' BRENDASearch("HEK 293 T", contain_all = TRUE)
#' BRENDASearch(c("HEK", "293", "T"), contain_all = TRUE)
#' BRENDASearch(c("HEK 293", "T"), contain_all = TRUE)
#'
#' # search for results that contain any of "HEK", "SUM" or "HeLa":
#' BRENDASearch("HEK SUM HeLa", contain_all = FALSE)
#' BRENDASearch(c("HEK", "SUM", "HeLa"), contain_all = FALSE)
#' BRENDASearch(c("HEK SUM", "HeLa"), contain_all = FALSE)
#'
#' @export
BRENDASearch <- function(x, file = BRENDA, contain_all = TRUE) {
  x <- unlist(strsplit(x, split = " "))
  if (contain_all) {
    result <- c(1:nrow(file))
    for (i in x) {
      result0 <- grep(i, file$Name)
      result <- intersect(result0, result)
    }
  } else {
    result <- numeric(0)
    for (i in x) {
      result0 <- grep(i, file$Name)
      result <- union(result0, result)
    }
  }
  result <- file[result, ]
  return(result)
}
