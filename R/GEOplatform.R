#' @title GEOPlatformSearch
#' @description search for GEO platform name
#' updated 08/2020
#' @param x a string or character vector to search for. input character vector if searching for multiple terms.
#' @param species species name to narrow down the search. partial input of an organism name is allowed. e.g. results for species "Homo sapiens" will be pulled if input "homo".
#' @param file dataframe for platform infomation. contains "Title" to search for and "Taxnomy" for organism, and "Accession" for platform name. Optional. GEOplatform file will be loaded if not provided.
#' @param contain_all if TRUE, results contain all search terms will be returned. if FALSE, results contain any of the given term will be returned.
#' @param accession_only if TRUE, only accession id of the search will be returned as a character. if FALSE, the complete information of the platforms will be returned as a dataframe.
#' @return character or dataframe of search result
#'
#' @examples
#' GEOPlatformSearch("illumina")
#' GEOPlatformSearch("illumina", species = "homo sapiens")
#' GEOPlatformSearch(c("Affymetrix", "Transcriptome"), species = "mus", contain_all = TRUE)
#' @export
GEOPlatformSearch <- function(x, species = NULL, file = GEOplatform, contain_all = TRUE, accession_only = FALSE) {
  # x <- unlist(strsplit(x, split = " "))
  if (!is.null(species)) {
    file <- file[grep(species, file$Taxonomy, ignore.case = TRUE), ]
  }
  if (contain_all) {
    result <- c(1:nrow(file))
    for (i in x) {
      result0 <- grep(i, file$Title)
      result <- intersect(result0, result)
    }
  } else {
    result <- numeric(0)
    for (i in x) {
      result0 <- grep(i, file$Title)
      result <- union(result0, result)
    }
  }
  result_df <- file[result, ]
  if (accession_only) {
    result_df <- as.character(result_df$Accession)
  }
  return(result_df)
}
