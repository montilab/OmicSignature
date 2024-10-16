#' @title Standardize signature data frame
#' @description remove missing and duplicated symbols.
#' updated 10/2024.
#'
#' @importFrom dplyr filter arrange mutate %>%
#' @importFrom stats complete.cases
#' @param sigdf signature dataframe
#' @return signature dataframe with empty, duplicate rows removed and ordered by score
#' @export
standardizeSigDF <- function(sigdf) {
  sigdf <- sigdf %>%
    dplyr::filter(feature_symbol != "") %>%
    dplyr::mutate(
      probe_id = as.character(probe_id),
      feature_symbol = as.character(feature_symbol)
    )
  if ("score" %in% colnames(sigdf)) {
    sigdf <- sigdf %>%
      dplyr::filter(score != "") %>%
      dplyr::mutate(score = as.numeric(as.character(score))) %>%
      dplyr::arrange(desc(abs(score)))
  }
  if ("direction" %in% colnames(sigdf)) {
    sigdf <- sigdf %>%
      dplyr::mutate(direction = as.character(direction))
  }
  return(sigdf)
}
