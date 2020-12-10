#' @title rename the columns of a differential analysis matrix for OmicSignature R6 object
#' updated 08/2020
#'
#' @importFrom dplyr %>%
#' @param colname a character vector, especially the column name of the differential analysis matrix, i.e. colnames(difexp)
#' or, if it's the difexp matrix itself, its column name will be modified and the matrix will be returned.
#' @return standardized column names of the difexp matrix before creating the OmicSignature R6 object
#' @export
replaceDifexpCol <- function(colname) {
  difexp <- NULL
  if (is(colname, "matrix") | is(colname, "data.frame")) {
    difexp <- colname
    colname <- colnames(difexp)
  }
  if (!is(colname, "character")) {
    stop("Input should be the column names of a difexp matrix, e.g. colnames(difexp)")
  }
  colname <- colname %>%
    tolower() %>%
    dplyr::recode(
      "t" = "score", "gene_symbol" = "symbol", "gene.symbol" = "symbol",
      "mean" = "aveexpr", "average" = "aveexpr",
      "probe.id" = "probe_id", "probe" = "probe_id", "id" = "probe_id",
      "log2fc" = "logfc", "log.fold.change" = "logfc",
      "p.value" = "p_value", "pval" = "p_value",
      "adj.p.val" = "adj_p", "adj.p.value" = "adj_p",
      "adj.p" = "adj_p", "fdr" = "adj_p",
      "q.value" = "q_value", "qval" = "q_value"
    )
  if (!("id" %in% colname) && "probe_id" %in% colname) {
    colname <- colname %>%
      dplyr::recode("probe_id" = "id")
  }
  colnameMissing <- setdiff(c("id", "symbol", "score", "adj_p"), colname)
  if (length(colnameMissing) > 0) {
    warning(
      "Required column for OmicSignature object difexp: ", paste(colnameMissing, collapse = ", "),
      ", is not found in your input. This may cause problem when creating your OmicSignature object."
    )
  }
  if (is.null(difexp)) {
    return(colname)
  } else {
    return(difexp)
  }
}
