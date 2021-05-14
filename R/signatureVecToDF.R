#' @title change a signature vector into a dataframe to be saved into OmicSignature object
#' updated 05/2021
#' @param input a character vector of the symbol of the signature, or a numeric vector which names are signature symbols and values are scores.
#' @param bi_directional logical.
#' @return signature dataframe with columns "signature_symbol", along with "signature_score" and "signature_direction" if applicable
#' @examples
#' signatures <- c("gene1", "gene2", "gene3")
#' signatureVecToDF(signatures)
#'
#' signatures <- c(0.45, -3.21, 2.44)
#' names(signatures) <- c("gene1", "gene2", "gene3")
#' signatureVecToDF(signatures, bi_directional = TRUE)
#' @export
#'
signatureVecToDF <- function(input, bi_directional = FALSE) {
  if (is.numeric(input) & !is.null(names(input))) {
    DF <- data.frame(
      "signature_symbol" = names(input),
      "signature_score" = input
    )
    if (bi_directional) {
      DF$signature_direction <- "+"
      DF$signature_direction[which(DF$signature_score < 0)] <- "-"
    } else {
      warning("If your input signature is bi-directional, set `bi_directional = TRUE`.")
    }
  } else if (is.character(input) | is.factor(input)) {
    DF <- data.frame("signature_symbol" = as.character(input))
  } else {
    stop("input signature vector is not valid. See `signatureVecToDF()`. ")
  }
  rownames(DF) <- NULL
  return(DF)
}
