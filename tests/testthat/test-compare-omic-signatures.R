test_that("overlap comparison returns symmetric Jaccard and count matrices", {
  sigs <- make_test_signature_list()

  res <- compare_omic_signatures(
    sigs,
    method = "overlap",
    adj_p_cutoff = 0.05,
    min_features = 3,
    max_feature = 4
  )

  expect_equal(res$method, "overlap")
  expect_named(res$comparisons, c("level1_vs_level1", "level2_vs_level2"))

  positive <- res$comparisons$level1_vs_level1
  expect_equal(dim(positive$jaccard), c(2L, 2L))
  expect_equal(unname(diag(positive$jaccard)), c(1, 1))
  expect_equal(positive$jaccard["sig_a", "sig_b"], 1 / 3)
  expect_equal(positive$jaccard["sig_b", "sig_a"], 1 / 3)
  expect_equal(positive$counts["sig_a", "sig_b"], "2 | 4 | 4")
})

test_that("overlap comparison supports comparing two signature lists", {
  sigs <- make_test_signature_list()

  res <- compare_omic_signatures(
    sig_list1 = sigs[1],
    sig_list2 = sigs[2],
    method = "overlap",
    min_features = 3,
    max_feature = 4
  )

  positive <- res$comparisons$level1_vs_level1
  expect_equal(rownames(positive$jaccard), "sig_a")
  expect_equal(colnames(positive$jaccard), "sig_b")
  expect_equal(positive$jaccard[1, 1], 1 / 3)
  expect_true(length(res$background) >= 8)
})

test_that("KS rank comparison returns score and p-value matrices", {
  sigs <- make_test_signature_list()

  res <- compare_omic_signatures(
    sigs,
    method = "ks",
    min_features = 3,
    max_feature = 4
  )

  expect_equal(res$method, "ks_rank")
  positive <- res$comparisons$level1_vs_level1
  expect_equal(dim(positive$score), c(2L, 2L))
  expect_equal(dim(positive$pvalue), c(2L, 2L))
  expect_true(all(is.finite(positive$pvalue[upper.tri(positive$pvalue)])))
  expect_equal(unname(diag(positive$score)), c(0.5, 0.5))
  expect_equal(unname(diag(positive$pvalue)), rep(0.2181818, 2), tolerance = 1e-6)
})

test_that("KS score comparison tests geneset score distributions", {
  sigs <- make_test_signature_list()

  res <- compare_omic_signatures(
    sigs,
    method = "ks_score",
    min_features = 3,
    max_feature = 4
  )

  positive <- res$comparisons$level1_vs_level1
  expect_equal(res$method, "ks_score")
  expect_equal(dim(positive$score), c(2L, 2L))
  expect_equal(dim(positive$pvalue), c(2L, 2L))
  expect_equal(unname(diag(positive$score)), c(1, 1))
  expect_equal(unname(diag(positive$pvalue)), rep(0.01428571, 2), tolerance = 1e-6)
})

test_that("KS ranking uses selected label versus contrast label", {
  sigs <- make_test_signature_list()

  stats_vec <- OmicSignature:::.cos_difexp_scores(
    sigs$sig_a,
    label = "up",
    feature_col = "feature_name",
    score_col = "score",
    p_value_col = "p_value",
    group_col = "group_label"
  )

  expect_equal(names(stats_vec), c("A", "B", "C", "D", "Z", "Y", "X", "W"))
  expect_equal(unname(stats_vec), c(4, 3, 2, 1, -1, -2, -3, -4))
})

test_that("comparison validates cutoffs and signature inputs", {
  sigs <- make_test_signature_list()

  expect_error(
    compare_omic_signatures(sigs, score_cutoff = -1),
    "score_cutoff"
  )
  expect_error(
    compare_omic_signatures(sigs, min_features = 2),
    "min_features"
  )
  expect_error(
    compare_omic_signatures(list(not_a_signature = data.frame())),
    "not OmicSignature"
  )
})

test_that("overlap comparison drops signatures that can't reach min_features", {
  sig_ok <- make_test_signature(
    "sig_ok",
    positive_features = c("A", "B", "C", "D"), negative_features = c("W", "X", "Y", "Z"),
    positive_scores = c(4, 3, 2, 1), negative_scores = c(-4, -3, -2, -1)
  )
  sig_thin <- make_test_signature(
    "sig_thin",
    positive_features = c("A", "B"), negative_features = c("M", "N", "O", "P"),
    positive_scores = c(4, 3), negative_scores = c(-4, -3, -2, -1)
  )

  expect_warning(
    res <- compare_omic_signatures(
      list(sig_ok = sig_ok, sig_thin = sig_thin),
      method = "overlap", min_features = 3, max_feature = 4
    ),
    "sig_thin"
  )

  ## The level with only 2 candidate features (sig_thin's "up") drops sig_thin
  ## entirely rather than reporting a false perfect self-similarity for it.
  dims <- vapply(res$comparisons, function(x) nrow(x$jaccard), integer(1))
  expect_true(any(dims == 1L))
  thin_level <- res$comparisons[[which(dims == 1L)]]
  expect_equal(rownames(thin_level$jaccard), "sig_ok")

  ## The level with 4 candidate features on both sides keeps both signatures
  ## and reports their true (non-hardcoded) overlap.
  full_level <- res$comparisons[[which(dims == 2L)]]
  expect_equal(unname(diag(full_level$jaccard)), c(1, 1))
  expect_equal(full_level$jaccard["sig_ok", "sig_thin"], 0)
})

test_that("group label levels are read from difexp when signature has fewer levels", {
  ## A signature table can legitimately contain only one factor level's rows
  ## (e.g. it was pre-filtered upstream) while difexp -- the table KS/GSEA
  ## ranking and overlap filtering actually use -- has both.
  difexp <- data.frame(
    probe_id = paste0("p", 1:8),
    feature_name = c("A", "B", "C", "D", "E", "F", "G", "H"),
    score = c(4, 3, 2, 1, -4, -3, -2, -1),
    p_value = 10^-c(4, 3, 2, 1, 4, 3, 2, 1),
    adj_p = rep(0.01, 8),
    group_label = factor(c(rep("up", 4), rep("down", 4)), levels = c("up", "down")),
    stringsAsFactors = FALSE
  )
  signature <- difexp[difexp$group_label == "up", c("probe_id", "feature_name", "score", "group_label")]
  metadata <- list(
    signature_name = "sig_biased", phenotype = "test",
    organism = predefined_organisms[1], direction_type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )
  capture.output(
    sig <- OmicSignature$new(metadata = metadata, signature = signature, difexp = difexp)
  )

  ## standardizeSigDF() collapses the signature table's factor to the single
  ## observed level, while difexp keeps both.
  expect_equal(levels(sig$signature$group_label), "up")
  expect_equal(levels(sig$difexp$group_label), c("up", "down"))

  expect_equal(
    OmicSignature:::.cos_group_label_levels(sig, "group_label"),
    c("up", "down")
  )
  expect_silent(
    res <- compare_omic_signatures(list(sig_biased = sig), method = "overlap", min_features = 3, max_feature = 4)
  )
  expect_named(res$comparisons, c("level1_vs_level1", "level2_vs_level2"))
})

test_that("cutoffs that can't be honored without difexp warn instead of being silently skipped", {
  metadata <- list(
    signature_name = "sig_no_difexp", phenotype = "test",
    organism = predefined_organisms[1], direction_type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )
  ## 5 "up" candidates so a strict adj_p_cutoff (0 pass) backfilling to
  ## min_features = 3 is distinguishable from the cutoff never having been
  ## applied at all (which would keep all 5).
  signature <- data.frame(
    probe_id = paste0("p", 1:10),
    feature_name = c("A", "B", "C", "D", "E", "V", "W", "X", "Y", "Z"),
    score = c(5, 4, 3, 2, 1, -5, -4, -3, -2, -1),
    adj_p = rep(0.9, 10),
    group_label = factor(c(rep("up", 5), rep("down", 5)), levels = c("up", "down")),
    stringsAsFactors = FALSE
  )
  capture.output(
    sig_with_adjp <- OmicSignature$new(metadata = metadata, signature = signature, difexp = NULL)
  )
  capture.output(
    sig_without_adjp <- OmicSignature$new(
      metadata = metadata, signature = signature[, c("probe_id", "feature_name", "score", "group_label")],
      difexp = NULL
    )
  )

  ## adj_p_cutoff is honored when the signature table carries an adj_p column:
  ## all 5 candidates fail the strict cutoff, so the retained set is backfilled
  ## down to just the min_features = 3 strongest, not the full pool of 5.
  feats <- OmicSignature:::.cos_signature_features(
    sig_with_adjp, "up", "feature_name", "score", "adj_p", "group_label",
    score_cutoff = 0, adj_p_cutoff = 0.001, min_features = 3, max_feature = 10
  )
  expect_equal(sort(feats), c("A", "B", "C"))

  ## When the column truly doesn't exist, the cutoff can't be honored -- warn
  ## rather than silently ignoring the caller's request, and keep all 5
  ## candidates since nothing was filtered out (min_features = 3 is already met).
  expect_warning(
    feats2 <- OmicSignature:::.cos_signature_features(
      sig_without_adjp, "up", "feature_name", "score", "adj_p", "group_label",
      score_cutoff = 0, adj_p_cutoff = 0.001, min_features = 3, max_feature = 10
    ),
    "adj_p_cutoff"
  )
  expect_equal(sort(feats2), c("A", "B", "C", "D", "E"))
})

test_that("no-difexp branch backfills to min_features like the difexp branch does", {
  metadata <- list(
    signature_name = "sig", phenotype = "test",
    organism = predefined_organisms[1], direction_type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )
  feature_name <- c("A", "B", "C", "D", "E", "V", "W", "X", "Y", "Z")
  score <- c(5, 4, 3, 2, 1, -5, -4, -3, -2, -1)
  group_label <- factor(c(rep("up", 5), rep("down", 5)), levels = c("up", "down"))

  difexp <- data.frame(
    probe_id = paste0("p", 1:10), feature_name = feature_name, score = score,
    p_value = 10^-abs(score), adj_p = rep(0.9, 10), group_label = group_label,
    stringsAsFactors = FALSE
  )
  capture.output(
    sig_with_difexp <- OmicSignature$new(
      metadata = metadata, signature = difexp[, c("probe_id", "feature_name", "score", "group_label")],
      difexp = difexp
    )
  )
  capture.output(
    sig_no_difexp <- OmicSignature$new(
      metadata = metadata,
      signature = data.frame(
        probe_id = paste0("p", 1:10), feature_name = feature_name, score = score,
        adj_p = rep(0.9, 10), group_label = group_label, stringsAsFactors = FALSE
      ),
      difexp = NULL
    )
  )

  args <- list(
    label = "up", feature_col = "feature_name", score_col = "score",
    adj_p_col = "adj_p", group_col = "group_label",
    score_cutoff = 0, adj_p_cutoff = 0.001, min_features = 3, max_feature = 10
  )
  feats_with_difexp <- do.call(OmicSignature:::.cos_signature_features, c(list(sig = sig_with_difexp), args))
  feats_no_difexp <- do.call(OmicSignature:::.cos_signature_features, c(list(sig = sig_no_difexp), args))

  ## Both branches backfill to the same 3 strongest features when the strict
  ## adj_p_cutoff excludes everything.
  expect_equal(sort(feats_with_difexp), c("A", "B", "C"))
  expect_equal(sort(feats_no_difexp), c("A", "B", "C"))
})

test_that("comparison rejects duplicate or overlapping signature names", {
  sigs <- make_test_signature_list()
  dup_list <- list(sig_a = sigs$sig_a, sig_a = sigs$sig_b)

  expect_error(
    compare_omic_signatures(dup_list, method = "overlap"),
    "duplicate signature names"
  )
  expect_error(
    compare_omic_signatures(sigs, dup_list, method = "overlap"),
    "duplicate signature names"
  )
  expect_error(
    compare_omic_signatures(sigs, sigs, method = "overlap"),
    "must not share signature names"
  )
})

test_that("KS/GSEA can't rank a signature without difexp, but it can still be a geneset", {
  sig_full <- make_test_signature(
    "sig_full",
    positive_features = c("A", "B", "C", "D"), negative_features = c("W", "X", "Y", "Z"),
    positive_scores = c(4, 3, 2, 1), negative_scores = c(-4, -3, -2, -1)
  )
  metadata <- list(
    signature_name = "sig_thin", phenotype = "test",
    organism = predefined_organisms[1], direction_type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )
  capture.output(
    sig_thin <- OmicSignature$new(
      metadata = metadata,
      signature = data.frame(
        probe_id = paste0("p", 1:8),
        feature_name = c("A", "B", "C", "D", "W", "X", "Y", "Z"),
        score = c(4, 3, 2, 1, -4, -3, -2, -1),
        adj_p = rep(0.001, 8),
        group_label = factor(c(rep("up", 4), rep("down", 4)), levels = c("up", "down")),
        stringsAsFactors = FALSE
      ),
      difexp = NULL
    )
  )

  ## adj_p is well within the default cutoff, so this only warns about
  ## sig_thin being excluded from the ranking side, not about a missing
  ## adj_p column (a separate, unrelated warning path).
  expect_warning(
    res <- compare_omic_signatures(
      list(sig_full = sig_full, sig_thin = sig_thin),
      method = "ks_rank", min_features = 3, max_feature = 4
    ),
    "sig_thin"
  )
  score <- res$comparisons$level1_vs_level1$score

  ## sig_thin as geneset (row) against sig_full's ranking (column) works.
  expect_true(is.finite(score["sig_thin", "sig_full"]))
  ## sig_thin as ranking (column) is impossible for anyone, including itself.
  expect_true(all(is.na(score[, "sig_thin"])))

  expect_error(
    compare_omic_signatures(list(sig_thin = sig_thin), method = "gsea"),
    "No signatures in sig_list2 have a difexp table"
  )
})
