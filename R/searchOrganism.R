#' @title search for a pre-defined organism name to use
#' @description updated 05/2025
#' @param x a string to search for (case-insensitive). if empty, will return all available organisms.
#' @param organism pre-defined organism character variable.
#' @return character of search result
#'
#' @examples
#' OmicS_searchOrganism()
#' OmicS_searchOrganism("homo")
#' @export
OmicS_searchOrganism <- function(x = "", organism = predefined_organisms) {
  result <- grep(x, organism, ignore.case = TRUE)
  return(organism[result])
}
