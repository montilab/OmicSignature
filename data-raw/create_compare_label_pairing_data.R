## Create simulated signatures for label-pairing comparison examples.
##
## The data are deterministic and designed so that:
## - signature_a treated is most similar to signature_b up, then signature_c resistant
## - signature_a control is most similar to signature_b down, then signature_c sensitive
## Each label-specific stored signature has 10 features, and each difexp table
## has 100 rows.

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(OmicSignature)
}

make_simulated_signature <- function(signature_name, label_features, all_features) {
  group_levels <- names(label_features)
  stopifnot(length(group_levels) == 2)

  signature <- do.call(
    rbind,
    lapply(group_levels, function(label) {
      data.frame(
        feature_name = label_features[[label]],
        score = seq(3, 1, length.out = length(label_features[[label]])),
        group_label = factor(label, levels = group_levels),
        stringsAsFactors = FALSE
      )
    })
  )
  signature$group_label <- factor(as.character(signature$group_label), levels = group_levels)

  difexp_labels <- rep(group_levels, each = length(all_features) / 2)
  for (label in group_levels) {
    difexp_labels[all_features %in% label_features[[label]]] <- label
  }
  difexp <- data.frame(
    probe_id = paste0(signature_name, "_probe_", seq_along(all_features)),
    feature_name = all_features,
    score = stats::rnorm(length(all_features), mean = 0, sd = 0.25),
    p_value = 0.5,
    adj_p = 0.5,
    group_label = factor(difexp_labels, levels = group_levels),
    stringsAsFactors = FALSE
  )

  for (label in group_levels) {
    idx <- match(label_features[[label]], difexp$feature_name)
    difexp$score[idx] <- seq(3, 1, length.out = length(idx))
    difexp$p_value[idx] <- seq(1e-6, 1e-4, length.out = length(idx))
    difexp$adj_p[idx] <- seq(1e-5, 1e-3, length.out = length(idx))
  }

  metadata <- list(
    signature_name = signature_name,
    phenotype = "simulated_label_pairing",
    organism = predefined_organisms[1],
    direction_type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )

  OmicSignature$new(
    metadata = metadata,
    signature = signature,
    difexp = difexp,
    print_message = FALSE
  )
}

set.seed(20260627)

label_features <- list(
  signature_a = list(
    treated = paste0("gene_", sprintf("%03d", 1:10)),
    control = paste0("gene_", sprintf("%03d", 31:40))
  ),
  signature_b = list(
    up = c(paste0("gene_", sprintf("%03d", 1:8)), "gene_021", "gene_022"),
    down = c(paste0("gene_", sprintf("%03d", 31:38)), "gene_051", "gene_052")
  ),
  signature_c = list(
    resistant = c(paste0("gene_", sprintf("%03d", 1:5)), paste0("gene_", sprintf("%03d", 61:65))),
    sensitive = c(paste0("gene_", sprintf("%03d", 31:35)), paste0("gene_", sprintf("%03d", 81:85)))
  )
)

compare_label_pairing_example <- lapply(names(label_features), function(signature_name) {
  make_simulated_signature(
    signature_name = signature_name,
    label_features = label_features[[signature_name]],
    all_features = paste0("gene_", sprintf("%03d", 1:100))
  )
})
names(compare_label_pairing_example) <- names(label_features)

save(
  compare_label_pairing_example,
  file = file.path("data", "compare_label_pairing_example.rda"),
  compress = "xz"
)
