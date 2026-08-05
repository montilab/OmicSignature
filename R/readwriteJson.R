#### writeJson() ####
#' @title write OmicSignature object into json txt format
#' @description To avoid confusion, in the written json text file, the column
#' names in signature dataframe and difexp dataframe will have prefix "sig_"
#' and "difexp" added. This corresponds to readJson() function.
#' updated 08/2025
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
  sig_group_label_levels <- if ("group_label" %in% colnames(OmicObj$signature)) {
    levels(OmicObj$signature$group_label)
  } else {
    NULL
  }

  #### difexp ####
  writeDifexp <- NULL
  difexp_group_label_levels <- NULL
  if (!is.null(OmicObj$difexp)) {
    # add "difexp_" prefix to column names to avoid confusion with other entries
    difexp_formatted <- OmicObj$difexp
    colnames(difexp_formatted) <- paste0("difexp_", colnames(difexp_formatted))
    writeDifexp <- c(
      list("difexp_colnames" = colnames(difexp_formatted)),
      difexp_formatted
    )
    if ("group_label" %in% colnames(OmicObj$difexp)) {
      difexp_group_label_levels <- levels(OmicObj$difexp$group_label)
    }
  }

  #### write json ####
  writeJsonObj <- jsonlite::toJSON(c(
    OmicObj$metadata,
    list(
      "metadata_length" = length(OmicObj$metadata),
      ## Written since this field was added, so readJson() can look metadata
      ## fields up by name instead of assuming they're the first
      ## metadata_length top-level keys in file order.
      "metadata_fields" = names(OmicObj$metadata)
    ),
    if (!is.null(sig_group_label_levels)) list(sig_group_label_levels = sig_group_label_levels),
    if (!is.null(difexp_group_label_levels)) list(difexp_group_label_levels = difexp_group_label_levels),
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
#' updated 08/2025
#'
#' @param filename json file name to read in
#' @return OmicSignature object
#' @export
readJson <- function(filename) {
  readJson <- jsonlite::fromJSON(txt = filename)

  #### metadata ####
  ## Prefer looking metadata fields up by name via metadata_fields (written
  ## by writeJson() since this was added); fall back to the legacy
  ## assumption that metadata fields are the first metadata_length top-level
  ## keys, for files written before metadata_fields existed.
  if (!is.null(readJson$metadata_fields)) {
    readMetadata <- readJson[readJson$metadata_fields]
  } else {
    readMetadata <- readJson[c(1:readJson$metadata_length)]
  }

  #### difexp ####
  readDifexp <- NULL
  if ("difexp_colnames" %in% names(readJson)) {
    readDifexp <- data.frame(dplyr::bind_rows(readJson[c(readJson$difexp_colnames)]))
    colnames(readDifexp) <- gsub("difexp_", "", colnames(readDifexp))
    if ("group_label" %in% colnames(readDifexp)) {
      ## difexp_group_label_levels is absent for files written before this
      ## was added; factor(x, levels = NULL) is *not* the same as omitting
      ## levels (it produces an all-NA factor), so levels must only be
      ## passed when actually present, falling back to auto-detected
      ## (sorted unique) levels otherwise, matching the previous behavior.
      readDifexp$group_label <- if (!is.null(readJson$difexp_group_label_levels)) {
        factor(as.character(readDifexp$group_label), levels = readJson$difexp_group_label_levels)
      } else {
        as.factor(as.character(readDifexp$group_label))
      }
    }
  } else {
    cat(paste("Notice: ", filename, "does not have difexp data. \n"))
  }

  #### sig df ####
  readSignature <- data.frame("probe_id" = readJson$sig_probe_id, "feature_name" = readJson$sig_feature_name)
  if (!is.null(readJson$sig_score)) {
    readSignature$score <- as.numeric(as.character(readJson$sig_score))
  }
  if (!is.null(readJson$sig_group_label)) {
    readSignature$group_label <- if (!is.null(readJson$sig_group_label_levels)) {
      factor(as.character(readJson$sig_group_label), levels = readJson$sig_group_label_levels)
    } else {
      as.factor(as.character(readJson$sig_group_label))
    }
  }

  #### Obj ####
  readOmicObj <- OmicSignature$new(metadata = readMetadata, signature = readSignature, difexp = readDifexp)
  return(readOmicObj)
}
