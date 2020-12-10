#' @title differential expression analysis using limma package
#' @description updated 08/2020
#' @importFrom limma lmFit eBayes topTable
#' @param dat dataframe or matrix containing expression data. rows should be features (e.g. genes) and columns should be samples.
#' @param ctrl_columns column names (character vector) or column numbers (numeric vector) of control samples. input type need to be consistant with trt_columns.
#' @param trt_columns column names (character vector) or column numbers (numeric vector) of treatment samples. input type need to be consistant with ctrl_columns.
#' @param id the id for the features, usually probe id. either the column name of the input dataframe contains the id, or character vector of the actual ids for all features.
#' @return dataframe of differential analysis
#' @export
diffAnalLm <- function(dat, ctrl_columns = c(2:4), trt_columns = c(5:7), id = "ID_REF") {
  if (length(id) == 1) {
    rownames(dat) <- dat[, id]
  } else if (length(id) == nrow(dat)) {
    rownames(dat) <- id
  } else {
    stop("Input id invalid")
  }
  design_mat <- data.frame(cbind(
    rep(1, (length(ctrl_columns) + length(trt_columns))),
    c(rep(0, length(ctrl_columns)), rep(1, length(trt_columns)))
  ))
  colnames(design_mat) <- c("ctrl", "trt")
  if (is.numeric(ctrl_columns)) {
    rownames(design_mat) <- colnames(dat)[c(ctrl_columns, trt_columns)]
  } else {
    rownames(design_mat) <- c(ctrl_columns, trt_columns)
  }

  fit_lm <- limma::lmFit(dat[, c(ctrl_columns, trt_columns)], design_mat)
  fit_lm <- limma::eBayes(fit_lm)
  toptable_lm <- limma::topTable(fit_lm,
    coef = "trt", adjust = "BH",
    sort.by = "t", number = nrow(dat)
  )
  toptable_lm <- cbind(toptable_lm, "probe_id" = rownames(toptable_lm))
  return(toptable_lm)
}
