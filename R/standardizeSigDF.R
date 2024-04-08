#' @title remove missing and duplicated entries in signature dataframe
#' updated 04/2024
#'
#' @importFrom dplyr filter arrange mutate %>%
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
    dplyr::mutate(
      id = as.character(id),
      symbol = as.character(symbol),
      score = as.numeric(as.character(score)),
      direction = as.factor(as.character(direction))
    )
  return(sigdf)
}
