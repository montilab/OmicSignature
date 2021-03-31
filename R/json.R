#### writeJson() ####
#' @title write OmicSignature object into json txt format
#' updated 03/2021
#'
#' @param OmicObj A OmicSignature object
#' @param file export file name
#' @importFrom jsonlite toJSON fromJSON
#' @return a "finished" message
#' @export
writeJson <- function(OmicObj, file) {
  # drop the previous signature_direction column, to save space:
  signatureDirection <- summary(OmicObj$signature$signature_direction)
  writeSignature <- OmicObj$signature
  writeSignature$signature_direction <- NULL
  writeDifexp <- NULL
  if (!is.null(OmicObj$difexp)) {
    writeDifexp <- c(
      list("lv1_colnames" = colnames(OmicObj$difexp)),
      OmicObj$difexp
    )
  }
  writeJsonObj <- jsonlite::toJSON(c(
    OmicObj$metadata,
    "metadata_length" = length(OmicObj$metadata),
    list("signature_direction_names" = names(signatureDirection)),
    signatureDirection,
    writeSignature,
    writeDifexp
  ), na = NULL, pretty = T)
  write(writeJsonObj, file)
  return("finished")
}

#### readJson() ####
#' @title read an OmicSignature object from json txt file created by writeJson()
#' updated 03/2021
#'
#' @param filename json file name to read in
#' @return OmicSignature object
#' @export
readJson <- function(filename) {
  readJson <- jsonlite::fromJSON(txt = filename)
  readMetadata <- readJson[c(1:readJson$metadata_length)]
  readLv1 <- NULL
  if ("lv1_colnames" %in% names(readJson)) {
    readLv1 <- data.frame(dplyr::bind_rows(readJson[c(readJson$lv1_colnames)]))
  } else {
    warning(paste("Notice: ", filename, "does not have difexp data."))
  }
  readLv2 <- data.frame(dplyr::bind_rows(readJson[c("signature_symbol", "signature_score")]))
  readLv2 <- cbind(readLv2, "signature_direction" = rep(
    readJson$signature_direction_names,
    unlist(readJson[readJson$signature_direction_names])
  ))
  readSigObj <- OmicSignature$new(readMetadata, readLv2, readLv1)
  return(readSigObj)
}
