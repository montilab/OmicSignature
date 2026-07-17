## Create an example dataset with only uni-directional signatures, for a
## vignette demonstrating compare_omic_signatures()'s uni-directional overlap
## support when no signature in the comparison has a group_label contrast at
## all.
##
## Three simulated signatures share a 100-gene universe: uni_x and uni_y
## partially overlap (5 of their 10 features each), and uni_z shares nothing
## with either.

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(OmicSignature)
}

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
    phenotype = "simulated_unidirectional",
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

all_features <- paste0("gene_", sprintf("%03d", 1:100))

feature_sets <- list(
  uni_x = paste0("gene_", sprintf("%03d", 1:10)),
  uni_y = paste0("gene_", sprintf("%03d", 6:15)),
  uni_z = paste0("gene_", sprintf("%03d", 50:59))
)

compare_unidirectional_example <- lapply(names(feature_sets), function(nm) {
  make_uni_simulated_signature(nm, features = feature_sets[[nm]], all_features = all_features)
})
names(compare_unidirectional_example) <- names(feature_sets)

save(
  compare_unidirectional_example,
  file = file.path("data", "compare_unidirectional_example.rda"),
  compress = "xz"
)
