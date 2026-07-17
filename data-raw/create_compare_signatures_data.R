## Create example signatures for comparison examples and vignettes.
##
## This script is adapted from the "BEGIN/END create data" block in
## ../MLscripts/R/compare_omics_signatures.R. It reads four local signature RDS
## files, reduces each difexp table to the union of stored signature features,
## and saves the resulting named list as package data.

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(OmicSignature)
}

sigrepo_path <- Sys.getenv(
  "SIGREPO_PATH",
  file.path(Sys.getenv("HOME"), "research/projects/sigRepo")
)

source_files <- list(
  e7386_collection = file.path(
    sigrepo_path,
    "signatures",
    "sub2025",
    "HNSCC_Hs_HSC&CAL27_E7386_vs_DMSO_MontiLab_OmSC.rds"
  ),
  icg001_hsc3 = file.path(
    sigrepo_path,
    "archive",
    "signatures",
    "ICG001_hsc3_oSig.rds"
  ),
  icg001_cal27 = file.path(
    sigrepo_path,
    "archive",
    "signatures",
    "ICG001_cal27_oSig.rds"
  )
)

missing_files <- source_files[!file.exists(unlist(source_files))]
if (length(missing_files) > 0) {
  stop(
    "Missing source signature file(s): ",
    paste(unlist(missing_files), collapse = ", ")
  )
}

compare_signatures_example <- c(
  readRDS(source_files$e7386_collection)$OmicSigList,
  list(
    icg001_hsc3 = readRDS(source_files$icg001_hsc3),
    icg001_cal27 = readRDS(source_files$icg001_cal27)
  )
)
names(compare_signatures_example)[1:2] <- c("e7386_hsc3", "e7386_cal27")

feature_union <- compare_signatures_example |>
  lapply(\(sig) sig$signature$feature_name) |>
  unlist(use.names = FALSE) |>
  unique()

## e7386_* and icg001_* signatures store group_label in different, and
## sometimes reversed, factor-level orders (DMSO/E7386 vs ICG001/DMSO). Left
## as-is, that made positional level-pairing in compare_omic_signatures()
## silently compare mismatched conditions (e.g. DMSO against ICG001) unless
## callers passed explicit label_pairing. Relevel every signature/difexp
## table so DMSO (control) is always level 1 and the drug is always level 2.
compare_signatures_example <- lapply(compare_signatures_example, \(sig) {
  treated_label <- setdiff(levels(sig$difexp$group_label), "DMSO")
  stopifnot(length(treated_label) == 1)
  canonical_levels <- c("DMSO", treated_label)

  difexp <- sig$difexp |>
    dplyr::filter(feature_name %in% feature_union) |>
    dplyr::mutate(group_label = factor(as.character(group_label), levels = canonical_levels))

  signature <- sig$signature |>
    dplyr::mutate(group_label = factor(as.character(group_label), levels = canonical_levels))

  OmicSignature$new(metadata = sig$metadata, signature = signature, difexp = difexp)
})

save(
  compare_signatures_example,
  file = file.path("data", "compare_signatures_example.rda"),
  compress = "xz"
)
