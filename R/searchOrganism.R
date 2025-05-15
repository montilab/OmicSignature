#' @title searchOrganism
#' @description search for organism
#' updated 05/2025
#' @param x a string or character vector to search for. input character vector if searching for multiple terms.
#' @param organism pre-defined organism character variable.
#' @return character of search result
#'
#' @examples
#' searchOrganism("Homo")
#' @export
searchOrganism <- function(x, organism = predefined_organisms) {
  result <- grep(x, organism, ignore.case = TRUE)
  return(organism[result])
}
