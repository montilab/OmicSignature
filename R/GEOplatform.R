#' @title searchPlatform
#' @description search for GEO platform name
#' updated 09/2024
#' @param x a string or character vector to search for. input character vector if searching for multiple terms.
#' @param organism organism name to narrow down the search. partial input of an organism name is allowed. e.g. results for organism "Homo sapiens" will be pulled if input "homo".
#' @param file GEO platform information dataframe. columns: "Accession", "Name", "Technology", "Organism".
#' @param contain_all if TRUE, results contain all search terms will be returned. if FALSE, results contain any of the given term will be returned.
#' @param accession_only if TRUE, only accession id of the search will be returned as a character. if FALSE, the complete information of the platforms will be returned as a dataframe.
#' @return character or dataframe of search result
#'
#' @examples
#' searchPlatform("illumina")
#' searchPlatform("illumina", organism = "Homo sapiens")
#' searchPlatform(c("Affymetrix", "Transcriptome"), organism = "mus", contain_all = TRUE)
#' @export
searchPlatform <- function(x, organism = NULL, file = GEOplatform, contain_all = TRUE, accession_only = FALSE) {
  # x <- unlist(strsplit(x, split = " "))
  if (!is.null(organism)) {
    file <- file[grep(organism, file$Organism, ignore.case = TRUE), ]
  }
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
  result_df <- file[result, ]
  if (accession_only) {
    result_df <- as.character(result_df$Accession)
  }
  return(result_df)
}
