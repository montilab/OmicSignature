make_test_signature <- function(name, positive_features, negative_features, positive_scores, negative_scores) {
  group_label <- factor(
    c(rep("up", length(positive_features)), rep("down", length(negative_features))),
    levels = c("up", "down")
  )
  difexp <- data.frame(
    probe_id = paste0("probe_", seq_along(group_label)),
    feature_name = c(positive_features, negative_features),
    score = c(positive_scores, negative_scores),
    p_value = 10^-abs(c(positive_scores, negative_scores)),
    adj_p = rep(0.01, length(group_label)),
    group_label = group_label,
    stringsAsFactors = FALSE
  )
  difexp$group_label <- group_label
  metadata <- list(
    signature_name = name,
    phenotype = "test",
    organism = predefined_organisms[1],
    direction_type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )
  capture.output(
    sig <- OmicSignature$new(
      metadata = metadata,
      signature = difexp[, c("probe_id", "feature_name", "score", "group_label")],
      difexp = difexp
    )
  )
  sig
}

make_uni_test_signature <- function(name, features, scores, with_difexp = TRUE) {
  base <- data.frame(
    probe_id = paste0("probe_", seq_along(features)),
    feature_name = features,
    score = scores,
    p_value = 10^-abs(scores),
    adj_p = rep(0.01, length(features)),
    stringsAsFactors = FALSE
  )
  metadata <- list(
    signature_name = name,
    phenotype = "test",
    organism = predefined_organisms[1],
    direction_type = "uni-directional",
    assay_type = predefined_assaytypes[1]
  )
  difexp <- NULL
  if (with_difexp) {
    ## checkDifexp() currently requires a group_label column to exist
    ## regardless of direction_type (unlike checkSignature(), which only
    ## requires it for non-uni-directional signatures); its contents aren't
    ## validated for uni-directional signatures, so a constant placeholder
    ## column is enough to satisfy that requirement.
    difexp <- base
    difexp$group_label <- factor("uni", levels = "uni")
  }
  capture.output(
    sig <- OmicSignature$new(
      metadata = metadata,
      signature = base[, c("probe_id", "feature_name", "score")],
      difexp = difexp
    )
  )
  sig
}

make_test_signature_list <- function() {
  list(
    sig_a = make_test_signature(
      "sig_a",
      positive_features = c("A", "B", "C", "D"),
      negative_features = c("W", "X", "Y", "Z"),
      positive_scores = c(4, 3, 2, 1),
      negative_scores = c(-4, -3, -2, -1)
    ),
    sig_b = make_test_signature(
      "sig_b",
      positive_features = c("B", "C", "E", "F"),
      negative_features = c("X", "Y", "M", "N"),
      positive_scores = c(3.5, 2.5, 1.5, 0.5),
      negative_scores = c(-3.5, -2.5, -1.5, -0.5)
    )
  )
}
