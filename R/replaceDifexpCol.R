#' @title Standardize the column names of difexp
#' @description A helper function that rename some of the commonly used columns 
#' of a differential expression dataframe or matrix. 
#' (updated 10/2024)
#'
#' @importFrom dplyr %>%
#' @importFrom methods is
#' @param x A single character vector specifying the column names of the differential 
#' expression dataframe or matrix. Alternatively, the differential 
#' expression dataframe or matrix itself. If the latter, its column name will be 
#' modified and the matrix will be returned.
#' @return standardized column names or standardized matrix 
#' @export
replaceDifexpCol <- function(x) {
  difexp <- NULL
  if (methods::is(x, "matrix") | methods::is(x, "data.frame")) {
    difexp <- x
    x <- xs(difexp)
  }
  if (!methods::is(x, "character")) {
    stop("Input should be the column names of a difexp matrix, e.g. xs(difexp)")
  }
  x <- x %>%
    tolower() %>%
    dplyr::recode(
      # old = new
      "t" = "score",
      "gene.symbol" = "gene_symbol", "hgnc_symbol" = "gene_symbol",
      "aveexpr" = "mean", "average" = "mean",
      "probe.id" = "probe_id", "probe" = "probe_id", "id" = "probe_id",
      "log2fc" = "logfc", "log.fold.change" = "logfc",
      "p.value" = "p_value", "pval" = "p_value",
      "adj.p.val" = "adj_p", "adj.p.value" = "adj_p", "adj.p" = "adj_p", "fdr" = "adj_p",
      "q.value" = "q_value", "qval" = "q_value"
    )
  xMissing <- setdiff(c("probe_id", "feature_name", "score"), x)
  if (length(xMissing) > 0) {
    warning(
      "Required column for OmicSignature object difexp: ", paste(xMissing, collapse = ", "),
      ", is not found in your input. This may cause problem when creating your OmicSignature object."
    )
  }
  exist_p_columns <- intersect(x, c("adj_p", "p_value", "q_value"))
  if (length(exist_p_columns) == 0) {
    warning("difexp need to contain at least one of the following: p_value, q_value, adj_p.")
  }
  if (is.null(difexp)) {
    return(x)
  } else {
    return(difexp)
  }
}
