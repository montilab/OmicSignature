#' @title rename the columns of a signature dataframe
#' updated 10/2020
#'
#' @importFrom dplyr recode %>%
#' @param sigdf signature dataframe
#' @param ObjtoGeneral if TRUE, will replace columns in OmicSignature object,
#' e.g. "signature_symbol", to general name, e.g. "symbol". if FALSE, will replace
#' general name, e.g. "name", to names in OmicSignature object, e.g. "signature_symbol".
#' @return signature dataframe with column names changed to standard
#' @export
replaceSigCol <- function(sigdf, ObjtoGeneral = TRUE) {
  if (!is(sigdf, "data.frame")) {
    stop("Input signature is not a dataframe.")
  }
  if (ObjtoGeneral) {
    # can't do rename() here since it requires the df to have the column specified
    colnames(sigdf) <- dplyr::recode(colnames(sigdf),
      "signature_symbol" = "symbol", "signature_score" = "score",
      "signature_direction" = "direction"
    )
  } else {
    colnames(sigdf) <- dplyr::recode(colnames(sigdf),
      "symbol" = "signature_symbol", "name" = "signature_symbol",
      "score" = "signature_score", "weight" = "signature_score",
      "direction" = "signature_direction"
    )
    sigdf$signature_score <- as.numeric(as.character(sigdf$signature_score))
    sigdf$signature_symbol <- as.character(sigdf$signature_symbol)
    sigdf$signature_direction <- as.factor(sigdf$signature_direction)
  }
  return(sigdf)
}
