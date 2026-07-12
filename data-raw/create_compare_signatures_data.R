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

compare_signatures_example <- lapply(compare_signatures_example, \(sig) {
  sig_filtered <- sig$clone()
  sig_filtered$difexp <- sig_filtered$difexp |>
    dplyr::filter(feature_name %in% feature_union)
  sig_filtered
})

save(
  compare_signatures_example,
  file = file.path("data", "compare_signatures_example.rda"),
  compress = "xz"
)
