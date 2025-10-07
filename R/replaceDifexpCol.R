#' @title rename the columns of a differential analysis matrix for OmicSignature R6 object
#' @description updated 10/2024
#'
#' @importFrom dplyr %>%
#' @importFrom methods is
#' @param colname a character vector, especially the column name of the differential analysis matrix, i.e. colnames(difexp)
#' or, if it's the difexp matrix itself, its column name will be modified and the matrix will be returned.
#' @return standardized column names of the difexp matrix before creating the OmicSignature R6 object
#' @export
replaceDifexpCol <- function(colname) {
  difexp <- NULL
  if (methods::is(colname, "matrix") | methods::is(colname, "data.frame")) {
    difexp <- colname
    colname <- colnames(difexp)
  }
  if (!methods::is(colname, "character")) {
    stop("Input should be the column names of a difexp matrix, e.g. colnames(difexp)")
  }
  colname <- colname %>%
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
  colnameMissing <- setdiff(c("probe_id", "feature_name", "score"), colname)
  if (length(colnameMissing) > 0) {
    warning(
      "Required column for OmicSignature object difexp: ", paste(colnameMissing, collapse = ", "),
      ", is not found in your input. This may cause problem when creating your OmicSignature object."
    )
  }
  exist_p_columns <- intersect(colname, c("adj_p", "p_value", "q_value"))
  if (length(exist_p_columns) == 0) {
    warning("difexp need to contain at least one of the following: p_value, q_value, adj_p.")
  }
  if (is.null(difexp)) {
    return(colname)
  } else {
    return(difexp)
  }
}
