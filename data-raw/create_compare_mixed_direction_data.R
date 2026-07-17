## Create an example dataset mixing bi-directional and uni-directional
## signatures, for a vignette demonstrating compare_omic_signatures()'s
## uni-directional signature support.
##
## Reuses signature_a/signature_b/signature_c from
## create_compare_label_pairing_data.R unchanged, and adds one
## uni-directional signature (signature_d) sharing the same 100-gene
## universe. signature_d's features overlap strongly with the
## "treated"/"up"/"resistant" levels of signature_a/b/c and not at all with
## their "control"/"down"/"sensitive" levels.

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(OmicSignature)
}

source(file.path("data-raw", "create_compare_label_pairing_data.R"))
## compare_label_pairing_example and predefined_organisms/predefined_assaytypes
## are now available from the sourced script.

make_uni_simulated_signature <- function(signature_name, features, all_features) {
  signature <- data.frame(
    feature_name = features,
    score = seq(3, 1, length.out = length(features)),
    stringsAsFactors = FALSE
  )

  difexp <- data.frame(
    probe_id = paste0(signature_name, "_probe_", seq_along(all_features)),
    feature_name = all_features,
    score = stats::rnorm(length(all_features), mean = 0, sd = 0.25),
    p_value = 0.5,
    adj_p = 0.5,
    stringsAsFactors = FALSE
  )
  idx <- match(features, difexp$feature_name)
  difexp$score[idx] <- seq(3, 1, length.out = length(idx))
  difexp$p_value[idx] <- seq(1e-6, 1e-4, length.out = length(idx))
  difexp$adj_p[idx] <- seq(1e-5, 1e-3, length.out = length(idx))

  ## checkDifexp() currently requires a group_label column regardless of
  ## direction_type (a pre-existing inconsistency with checkSignature(),
  ## which only requires it for non-uni-directional signatures); its
  ## contents aren't validated for uni-directional signatures, so a
  ## constant placeholder column is enough to satisfy that requirement.
  difexp$group_label <- factor("uni", levels = "uni")

  metadata <- list(
    signature_name = signature_name,
    phenotype = "simulated_label_pairing",
    organism = predefined_organisms[1],
    direction_type = "uni-directional",
    assay_type = predefined_assaytypes[1]
  )

  OmicSignature$new(
    metadata = metadata,
    signature = signature,
    difexp = difexp,
    print_message = FALSE
  )
}

## Overlaps signature_a's "treated" (genes 001-010), signature_b's "up"
## (genes 001-008, 021, 022), and signature_c's "resistant" (genes 001-005,
## 061-065); shares nothing with any signature's "control"/"down"/
## "sensitive" level.
signature_d <- make_uni_simulated_signature(
  "signature_d",
  features = paste0("gene_", sprintf("%03d", 1:6)),
  all_features = paste0("gene_", sprintf("%03d", 1:100))
)

compare_mixed_direction_example <- c(compare_label_pairing_example, list(signature_d = signature_d))

save(
  compare_mixed_direction_example,
  file = file.path("data", "compare_mixed_direction_example.rda"),
  compress = "xz"
)
