#' @title remove missing and duplicated entries in signature dataframe
#' updated 02/2024
#'
#' @importFrom dplyr distinct filter arrange mutate %>%
#' @importFrom stats complete.cases
#' @param sigdf signature dataframe
#' @return signature dataframe with empty, duplicate rows removed and ordered by score
#' @export
standardizeSigDF <- function(sigdf) {
  sigdf <- replaceSigCol(sigdf, long = FALSE)
  sigdf <- sigdf[complete.cases(sigdf), ]
  sigdf <- sigdf %>%
    dplyr::filter(symbol != "", score != "") %>%
    dplyr::arrange(desc(abs(score))) %>%
    dplyr::distinct(symbol, .keep_all = TRUE) %>%
    dplyr::mutate(
      score = as.numeric(as.character(score)),
      symbol = as.character(symbol),
      direction = as.factor(as.character(direction))
    )
  return(sigdf)
}
