#### OmicSigFromDifexp() ####
#' @title create an OmicSignature object from differential expsignaturession matrix
#' updated 03/2021
#' @param difexp Differential expsignaturession matrix
#' @param metadata Metadata for the OmicSignature object. If `criteria` is `NULL`,
#' the criterias to extract signatures will need to be provided in metadata.
#' They can be specified in metadata fields as one or more of the followings:
#' `logfc_cutoff`, `score_cutoff`, `adj_p_cutoff`, `p_value_cutoff`.
#' @param criteria A character string to specify criterias used to extract
#' signatures from difexp. e.g. "logfc > 5; score > 10". Alternatively,
#' they can be provided in metadata fields: list("logfc_cutoff" = 5, "score_cutoff" = 10)
#' @importFrom dplyr filter select mutate arrange pull desc %>%
#' @importFrom rlang parse_exprs
#' @return OmicSignature object
#' @export

OmicSigFromDifexp <- function(difexp, metadata, criteria = NULL) {
  symbol <- NULL # not functional; define these only to pass R CMD check
  score <- NULL # R CMD wrongly view them as variables when used in dplyr functions
  if (is.null(criteria)) {
    criteria <- c()
    if (!is.null(metadata$logfc_cutoff)) {
      criteria <- c(criteria, paste("logfc >", metadata$logfc_cutoff))
    }
    if (!is.null(metadata$score_cutoff)) {
      criteria <- c(criteria, paste("abs(score) >", metadata$score_cutoff))
    }
    if (!is.null(metadata$adj_p_cutoff)) {
      criteria <- c(criteria, paste("adj_p <", metadata$adj_p_cutoff))
    }
    if (!is.null(metadata$p_value_cutoff)) {
      criteria <- c(criteria, paste("p_value <", metadata$p_value_cutoff))
    }
    criteria <- paste(criteria, collapse = "; ")
  }
  cat(paste("-- criterias used to extract signatures: ", criteria, ". \n\n"))
  v <- rlang::parse_exprs(criteria)
  signatures <- difexp %>%
    dplyr::filter(!!!v) %>%
    dplyr::select(symbol, score) %>%
    dplyr::mutate(direction = ifelse(score < 0, "-", "+")) %>%
    dplyr::arrange(dplyr::desc(abs(score)))
  signatures <- signatures[complete.cases(signatures), ]
  signatures <- signatures[signatures$symbol != "", ]
  signatures <- signatures[signatures$score != "", ]
  signatures <- distinct(signatures, signatures$symbol, .keep_all = T)[, c(1:3)]

  OmicSig <- OmicSignature$new(
    metadata = metadata,
    signature = signatures,
    difexp = difexp
  )
  return(OmicSig)
}
