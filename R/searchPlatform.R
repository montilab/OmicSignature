#' @title search for a platform name to use
#' @description updated 05/2025
#' @param x a string or character vector to search for. input a character vector if searching for multiple terms. if empty, will return all available organisms.
#' @param platforms pre-defined platform information character variable.
#' @param contain_all if TRUE, results contain all search terms will be returned. if FALSE, results contain any of the given term will be returned.
#' @return character or dataframe of search result
#'
#' @examples
#' searchPlatform()
#' searchPlatform("proteomics")
#' searchPlatform(c("transcript", "single-cell"), contain_all = TRUE)
#' @export
searchPlatform <- function(x = "", platforms = predefined_platforms, contain_all = TRUE) {
  if (contain_all) {
    result <- seq(length(platforms))
    for (i in x) {
      result0 <- grep(i, platforms, ignore.case = TRUE)
      result <- intersect(result0, result)
    }
  } else {
    result <- numeric(0)
    for (i in x) {
      result0 <- grep(i, platforms, ignore.case = TRUE)
      result <- union(result0, result)
    }
  }
  return(platforms[result])
}
