#### OmicSigFromDifexp() ####
#' @title create an OmicSignature object from differential expression matrix
#' @description updated 08/2025
#' @param difexp Differential expression matrix
#' @param metadata Metadata for the OmicSignature object. If `criteria` is `NULL`,
#' the criterias to extract signatures will need to be provided in metadata.
#' They can be specified in metadata fields as one or more of the followings:
#' `logfc_cutoff`, `score_cutoff`, `adj_p_cutoff`, `p_value_cutoff`.
#' @param criteria A character string of R expressions used to extract
#' signatures from difexp, e.g. "logfc > 5; score > 10", evaluated as R code
#' (via `rlang::parse_exprs()`) against `difexp` - `criteria` must only ever
#' come from a trusted source, not from untrusted/external input.
#' Alternatively, they can be provided in metadata fields:
#' list("logfc_cutoff" = 5, "score_cutoff" = 10). At least one of `criteria`
#' or a metadata cutoff field is required; without any, every row of
#' `difexp` would silently become the signature.
#' @importFrom dplyr filter select mutate arrange pull desc across everything %>%
#' @importFrom rlang parse_exprs
#' @return OmicSignature object
#' @export

OmicSigFromDifexp <- function(difexp, metadata, criteria = NULL) {
  ## define the following to pass R check since they are viewed as variables in dplyr functions
  score <- NULL

  signatureType <- metadata$direction_type

  ## if probe_id is not provided in difexp, setup numeric counter as probe_id
  if (!"probe_id" %in% colnames(difexp)) {
    difexp <- difexp %>% dplyr::mutate(probe_id = seq(nrow(difexp)), .before = dplyr::everything())
  }

  if (signatureType == "bi-directional") {
    if (!"group_label" %in% colnames(difexp)) {
      if (!"score" %in% colnames(difexp)) stop("difexp must contain group_label or score column.")
      difexp <- difexp %>% dplyr::mutate(group_label = factor(ifelse(score < 0, "-", "+"), levels = c("+", "-")), .after = score)
    }
  }

  if (is.null(criteria)) {
    criteria <- c()
    if (!is.null(metadata$logfc_cutoff)) criteria <- c(criteria, paste("logfc >=", metadata$logfc_cutoff))
    if (!is.null(metadata$score_cutoff)) criteria <- c(criteria, paste("abs(score) >=", metadata$score_cutoff))
    if (!is.null(metadata$adj_p_cutoff)) criteria <- c(criteria, paste("adj_p <=", metadata$adj_p_cutoff))
    if (!is.null(metadata$p_value_cutoff)) criteria <- c(criteria, paste("p_value <=", metadata$p_value_cutoff))
    criteria <- paste(criteria, collapse = "; ")
  }
  if (!nzchar(trimws(criteria))) {
    stop(
      "No filtering criteria available: pass `criteria` explicitly, or set at least one of ",
      "metadata$logfc_cutoff, metadata$score_cutoff, metadata$adj_p_cutoff, ",
      "metadata$p_value_cutoff. Without any criteria, every row of difexp would silently ",
      "become the signature."
    )
  }
  cat(paste("-- criterias used to extract signatures: ", criteria, ". \n\n"))

  signatures <- .extract_signature_rows(difexp, criteria, signatureType)

  OmicSig <- OmicSignature$new(
    metadata = metadata,
    signature = signatures,
    difexp = difexp
  )
  return(OmicSig)
}
