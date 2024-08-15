#' @title rename the columns of the signature data frame
#' @description
#' updated 08/2024
#'
#' @importFrom dplyr case_match %>%
#' @param sigdf signature dataframe
#' @param to_long_name if TRUE, replace "symbol" to "signature_symbol", mainly
#' used to facilitate write into json format; if FALSE, replace "signature_symbol"
#' to "symbol", i.e. standard sigdf names.
#' @return signature dataframe with column names changed to standard
#' @export
replaceSigCol <- function(sigdf, to_long_name = FALSE) {
  if (!is(sigdf, "data.frame")) {
    stop("Input signature is not a dataframe.")
  }

  colnames(sigdf) <- tolower(colnames(sigdf))
  if (to_long_name) {
    # can not use rename() since it requires the df to have the column specified
    colnames(sigdf) <- dplyr::case_match(
      colnames(sigdf),
      .default = colnames(sigdf),
      "symbol" ~ "signature_symbol",
      "score" ~ "signature_score",
      "direction" ~ "signature_direction"
    )
  } else {
    colnames(sigdf) <- dplyr::case_match(
      colnames(sigdf),
      .default = colnames(sigdf),
      "signature_symbol" ~ "symbol",
      "signature_score" ~ "score",
      "signature_direction" ~ "direction"
    )
  }
  return(sigdf)
}
