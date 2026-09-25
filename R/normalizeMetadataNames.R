#' Normalize deprecated metadata field names
#'
#' Accepts a metadata list that may use the pre-1.4.0 field name
#' `direction_type` and returns the same list using the current name `type`.
#' Called from `OmicSignature`'s private `checkMetadata()`, which every
#' construction and assignment path routes through, so callers do not need to
#' normalize themselves.
#'
#' @param metadata a metadata list. Inputs that are not named lists are
#' returned unchanged, leaving their validation to the caller.
#' @return the metadata list using `type`.
#' @keywords internal
.normalize_metadata_names <- function(metadata) {
  if (!is.list(metadata)) {
    return(metadata)
  }
  field_names <- names(metadata)
  if (is.null(field_names) || !"direction_type" %in% field_names) {
    return(metadata)
  }
  if ("type" %in% field_names) {
    stop(
      "metadata contains both 'type' and the deprecated 'direction_type'. ",
      "Keep only 'type'."
    )
  }
  names(metadata)[field_names == "direction_type"] <- "type"
  warning(
    "metadata field 'direction_type' is deprecated and was renamed to 'type'. ",
    "Update your metadata to use 'type'."
  )
  metadata
}
