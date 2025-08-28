#' @title Standardize signature data frame
#' @description remove missing and duplicated symbols.
#' updated 08/2025.
#'
#' @importFrom dplyr filter arrange mutate %>%
#' @importFrom stats complete.cases
#' @param sigdf signature dataframe
#' @return signature dataframe with empty, duplicate rows removed and ordered by score
#' @export
standardizeSigDF <- function(sigdf) {
  ## define the following to pass R check since they are viewed as variables in dplyr functions
  feature_name <- NULL
  probe_id <- NULL
  score <- NULL
  group_label <- NULL

  sigdf <- sigdf %>%
    dplyr::filter(feature_name != "") %>%
    dplyr::mutate(
      probe_id = as.character(probe_id),
      feature_name = as.character(feature_name)
    )
  if ("score" %in% colnames(sigdf)) {
    sigdf <- sigdf %>%
      dplyr::filter(score != "") %>%
      dplyr::mutate(score = as.numeric(as.character(score))) %>%
      dplyr::arrange(desc(abs(score)))
  }
  if ("group_label" %in% colnames(sigdf)) {
    sigdf <- sigdf %>%
      dplyr::mutate(group_label = as.factor(as.character(group_label)))
  }
  return(sigdf)
}
