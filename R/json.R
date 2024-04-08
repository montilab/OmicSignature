#### writeJson() ####
#' @title write OmicSignature object into json txt format
#' @description To avoid confusion, in the written json text file, the column
#' names in signature dataframe and difexp dataframe will have prefix "sig_"
#' and "difexp" added. This corresponds to readJson() function.
#' updated 04/2024
#'
#' @param OmicObj A OmicSignature object
#' @param file file name to write
#' @importFrom jsonlite toJSON fromJSON
#' @return NULL
#' @export
writeJson <- function(OmicObj, file) {
  #### sig df ####
  writeSignature <- OmicObj$signature
  # add "sig_" prefix to column names to avoid confusion with columns in difexp
  names(writeSignature) <- paste0("sig_", names(writeSignature))

  #### difexp ####
  writeDifexp <- NULL
  if (!is.null(OmicObj$difexp)) {
    # add "difexp_" prefix to column names to avoid confusion with other entries
    difexp_formatted <- OmicObj$difexp
    colnames(difexp_formatted) <- paste0("difexp_", colnames(difexp_formatted))
    writeDifexp <- c(
      list("difexp_colnames" = colnames(difexp_formatted)),
      difexp_formatted
    )
  }

  #### write json ####
  writeJsonObj <- jsonlite::toJSON(c(
    OmicObj$metadata,
    "metadata_length" = length(OmicObj$metadata),
    writeSignature,
    writeDifexp
  ), na = NULL, pretty = TRUE)
  write(writeJsonObj, file)

  cat(paste("Finish writing", file, "."))
}

#### readJson() ####
#' @title read an OmicSignature object from json txt file created by writeJson()
#' @description To avoid confusion, in the json text file, assume the column
#' names in signature dataframe and difexp dataframe have prefix "sig_"
#' and "difexp". This corresponds to writeJson() function.
#' updated 04/2024
#'
#' @param filename json file name to read in
#' @return OmicSignature object
#' @export
readJson <- function(filename) {
  readJson <- jsonlite::fromJSON(txt = filename)

  #### metadata ####
  readMetadata <- readJson[c(1:readJson$metadata_length)]

  #### difexp ####
  readDifexp <- NULL
  if ("difexp_colnames" %in% names(readJson)) {
    readDifexp <- data.frame(dplyr::bind_rows(readJson[c(readJson$difexp_colnames)]))
    colnames(readDifexp) <- gsub("difexp_", "", colnames(readDifexp))
  } else {
    cat(paste("Notice: ", filename, "does not have difexp data. \n"))
  }

  #### sig df ####
  readSignature <- data.frame("id" = readJson$sig_id, "symbol" = readJson$sig_symbol)
  if (!is.null(readJson$sig_score)) {
    readSignature$score <- readJson$sig_score
  }
  if (!is.null(readJson$sig_direction)) {
    readSignature$direction <- readJson$sig_direction
  }

  #### Obj ####
  readOmicObj <- OmicSignature$new(metadata = readMetadata, signature = readSignature, difexp = readDifexp)
  return(readOmicObj)
}
