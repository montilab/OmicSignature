#' @title search for an assay type to use
#' @description updated 10/2025
#' @param x a string to search for. if empty, will return all available assay types.
#' @return search result
#'
#' @examples
#' OmicS_searchAssayType()
#' OmicS_searchAssayType("transcript")
#' @export
OmicS_searchAssayType <- function(x = "") {
  result <- grep(x, predefined_assaytypes, ignore.case = TRUE)
  return(predefined_assaytypes[result])
}
