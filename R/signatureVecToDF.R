#' @title change a signature vector into a dataframe to be saved into OmicSignature object
#' updated 08/2025
#' @param input a character vector of the significant feature names, or a numeric vector of scores and named by feature names.
#' @param group_labels optional. a character vector of length of 2. when group_labels = c("Group1", "Group2"), it indicates the analysis is Group1 vs Group2, and positive scores indicate a higher value in Group1.
#' @return signature dataframe with columns "feature_name", along with "score" and "group_label" if applicable.
#' @examples
#' signatures <- c("gene1", "gene2", "gene3")
#' signatureVecToDF(signatures)
#'
#' signatures <- c(0.45, -3.21, 2.44)
#' names(signatures) <- c("gene1", "gene2", "gene3")
#' signatureVecToDF(signatures)
#' signatureVecToDF(signatures, group_labels = c("Group1", "Group2"))
#' @export
#'
signatureVecToDF <- function(input, group_labels = NULL) {
  if (is.character(input) | is.factor(input)) {
    DF <- data.frame(
      "probe_id" = seq(length(input)),
      "feature_name" = as.character(input)
    )
  } else if (is.numeric(input) & !is.null(names(input))) {
    DF <- data.frame(
      "probe_id" = seq(length(input)),
      "feature_name" = names(input),
      "score" = input
    )
    if (!is.null(group_labels)) {
      if (length(group_labels) == 2) {
        DF$group_label <- ifelse(DF$score > 0, group_labels[1], group_labels[2])
      } else {
        warning("group_labels must be length of 2.")
      }
    }
  } else {
    stop("input signature vector is not valid. See `signatureVecToDF()`. ")
  }
  rownames(DF) <- NULL
  return(DF)
}
