#' @title Standardize signature data frame
#' @description remove rows with an empty feature name, coerce score to
#' numeric and order by absolute score. Rows whose score is missing or
#' cannot be parsed are kept (with an NA score) and sorted last, so a
#' uni-directional gene list without scores keeps all its features.
#' updated 10/2026.
#'
#' @importFrom dplyr filter arrange mutate %>%
#' @importFrom stats complete.cases
#' @param sigdf signature dataframe
#' @return signature dataframe with empty feature names removed and ordered by
#' absolute score, NA scores last
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
      dplyr::mutate(score = suppressWarnings(as.numeric(as.character(score)))) %>%
      dplyr::arrange(is.na(score), desc(abs(score)))
  }
  if ("group_label" %in% colnames(sigdf)) {
    ## Preserve the caller's factor level order (e.g. control-first) instead
    ## of resetting to alphabetical order; compare_omic_signatures() pairs
    ## signatures by level position, so a silently reordered level1/level2
    ## can compare mismatched conditions across signatures. Levels with no
    ## observed rows are still dropped (e.g. a signature pre-filtered to one
    ## group upstream), matching the prior as.factor()-based behavior.
    observed <- as.character(sigdf$group_label)
    declared_order <- if (is.factor(sigdf$group_label)) levels(sigdf$group_label) else unique(observed)
    group_label_levels <- intersect(declared_order, unique(observed))
    sigdf <- sigdf %>%
      dplyr::mutate(group_label = factor(as.character(group_label), levels = group_label_levels))
  }
  return(sigdf)
}
