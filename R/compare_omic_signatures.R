#######################################################################
## function: COMPARE OMIC SIGNATURES
##
## Pairwise comparison of OmicSignature objects by overlap, KS, or GSEA.
#######################################################################

#' Compare OmicSignature objects
#'
#' Compare signatures within one list, or between two lists, using set overlap,
#' rank-position KS, score-distribution KS, or fgsea.
#'
#' @param sig_list1 List of OmicSignature objects or an OmicSignatureCollection.
#'   Names must be unique within the list.
#' @param sig_list2 Optional second list of OmicSignature objects or
#'   collection. Names must be unique within the list and must not overlap
#'   with `sig_list1`'s names, so that a cross-list comparison can never be
#'   mistaken for a self-comparison. For `method = "ks_rank"`,
#'   `method = "ks_score"`, and `method = "gsea"`, `sig_list2` is the ranking
#'   side and each element needs to be bi-directional with a difexp table;
#'   elements that aren't (including uni-directional signatures, which have
#'   no group_label contrast to rank from) are excluded from `sig_list2`
#'   (with a warning) but remain usable as `sig_list1` genesets, which never
#'   require difexp or a group_label contrast. If no element of `sig_list2`
#'   can rank, this errors instead.
#' @param method Comparison method.
#' @param background Optional background feature vector for overlap tests.
#' @param score_cutoff Minimum absolute score to include in a signature. For
#'   signatures without a difexp table, this can only be applied if the
#'   signature table itself has a `score_col` column; otherwise a warning is
#'   issued and the cutoff is skipped.
#' @param adj_p_cutoff Maximum adjusted p-value to include in a signature. For
#'   signatures without a difexp table, this can only be applied if the
#'   signature table itself has an `adj_p_col` column; otherwise a warning is
#'   issued and the cutoff is skipped.
#' @param min_features Minimum number of features retained per label-specific
#'   signature. Must be at least 3. For `method = "overlap"`, any
#'   signature/label that cannot reach `min_features` retained features (even
#'   after backfilling) is dropped from the comparison with a warning, rather
#'   than being scored against a near-empty or empty feature set.
#' @param max_feature Maximum number of features retained per label-specific
#'   signature.
#' @param label_pairing Optional named list giving the two group-label levels
#'   to use for each signature in `sig_list1`. Signatures not named in
#'   `label_pairing` fall back to their own `group_label` factor-level order;
#'   if two or more of those signatures disagree on that order (e.g. one
#'   signature's factor levels are `c("control", "treated")` and another's are
#'   `c("treated", "control")`), a warning is issued, since `level1_vs_level1`/
#'   `level2_vs_level2` pairing is purely positional and may then compare
#'   mismatched conditions across signatures.
#' @param label_pairing2 Optional named list giving the two group-label levels
#'   to use for each signature in `sig_list2`. Same fallback and warning
#'   behavior as `label_pairing`.
#' @param feature_col Column containing feature identifiers.
#' @param score_col Column containing scores in signature and difexp tables.
#' @param adj_p_col Column containing adjusted p-values in difexp tables.
#' @param p_value_col Column containing p-values used to rank difexp tables for
#'   KS and GSEA comparisons.
#' @param group_col Column containing phenotype group labels.
#' @param adjust Logical; adjust p-values within each returned comparison.
#' @param p_adjust_method Multiple-testing correction method.
#' @param alternative Alternative hypothesis for Fisher and KS tests. For KS,
#'   `"greater"` tests whether the feature set is enriched at the top of the
#'   signed ranking, and `"less"` tests enrichment at the bottom.
#' @param gsea_score Column from fgsea output to return as the score matrix.
#' @param minSize Minimum pathway size passed to fgsea.
#' @param maxSize Maximum pathway size passed to fgsea.
#' @param nproc Number of fgsea workers.
#' @param ... Additional arguments passed to fgsea.
#'
#' @details `method = "ks"` is a backwards-compatible alias for
#'   `method = "ks_rank"`. For `method = "ks_rank"`, `method = "ks_score"`,
#'   and `method = "gsea"`, each ranked vector is
#'   built from both phenotype labels. Features in the requested `group_col`
#'   label are ranked by positive `-log10(p_value)`, and features in the
#'   contrasting label are ranked by negative `-log10(p_value)`. This places
#'   the most significant selected-label features at the top of the ranking and
#'   the most significant contrast-label features at the bottom. `ks_rank`
#'   compares the positions of each retained feature set within this ranked
#'   vector. `ks_score` compares the numeric ranking scores for retained
#'   features against the remaining features with a two-sample KS test.
#'
#'   Uni-directional signatures (no `group_label` contrast) are compared as a
#'   single, whole feature set rather than split by level. If every signature
#'   in both `sig_list1` and `sig_list2` is uni-directional, `method =
#'   "overlap"` returns one flat comparison instead of the
#'   `level1_vs_level1`/`level2_vs_level2` structure (see the Value section
#'   below). If the signatures being compared are a mix of uni- and
#'   bi-directional, each uni-directional signature's single feature set is
#'   compared against both levels of every bi-directional signature. For
#'   `method = "ks_rank"`, `method = "ks_score"`, and `method = "gsea"`, a
#'   uni-directional signature can only ever be a geneset (`sig_list1`),
#'   never a ranking (`sig_list2`), since ranking requires a two-group
#'   contrast; if `sig_list2` contains no bi-directional signature with a
#'   difexp table (e.g. it is entirely uni-directional), the comparison is
#'   not possible and this errors.
#'
#' @return A list with one element per label pairing, except when every
#'   signature in both `sig_list1` and `sig_list2` is uni-directional and
#'   `method = "overlap"`, in which case `comparisons` directly contains
#'   `jaccard`, `pvalue`, and `counts` (no `level1_vs_level1` nesting, and
#'   `label_order` is `NULL`). Otherwise, for `method = "overlap"` each
#'   element contains `jaccard`, `pvalue`, and `counts` matrices. `counts`
#'   entries are formatted as `"ov | n1 | n2"`. For `method = "ks_rank"`,
#'   `method = "ks_score"`, `method = "ks"`, and
#'   `method = "gsea"` each element contains `score` and `pvalue` matrices;
#'   columns for `sig_list2` signatures without a difexp table, or that are
#'   uni-directional, are entirely `NA`, since those signatures cannot serve
#'   as the ranking side.
#'
#' @examples
#' data(compare_signatures_example)
#'
#' overlap_res <- compare_omic_signatures(
#'   compare_signatures_example[1:2],
#'   method = "overlap",
#'   score_cutoff = log2(1.025),
#'   adj_p_cutoff = 0.01,
#'   min_features = 10
#' )
#' overlap_res$comparisons$level1_vs_level1$jaccard
#'
#' data(compare_label_pairing_example)
#'
#' toy_ks_score <- compare_omic_signatures(
#'   compare_label_pairing_example,
#'   method = "ks_score",
#'   label_pairing = list(
#'     signature_a = c("treated", "control"),
#'     signature_b = c("up", "down"),
#'     signature_c = c("resistant", "sensitive")
#'   ),
#'   min_features = 5
#' )
#' round(toy_ks_score$comparisons$level1_vs_level1$score, 3)
#'
#' @export
compare_omic_signatures <- function(
    sig_list1,
    sig_list2 = NULL,
    method = c("overlap", "ks_rank", "ks_score", "ks", "gsea"),
    background = NULL,
    score_cutoff = 0,
    adj_p_cutoff = 0.05,
    min_features = 5,
    max_feature = 500,
    label_pairing = NULL,
    label_pairing2 = NULL,
    feature_col = "feature_name",
    score_col = "score",
    adj_p_col = "adj_p",
    p_value_col = "p_value",
    group_col = "group_label",
    adjust = FALSE,
    p_adjust_method = "BH",
    alternative = "greater",
    gsea_score = "NES",
    minSize = 1,
    maxSize = Inf,
    nproc = 0,
    ...) {
  ## Normalize user options before doing any object-specific work.
  method <- match.arg(method)
  method <- .cos_normalize_method(method)
  p_adjust_method <- match.arg(p_adjust_method, stats::p.adjust.methods)
  .cos_check_cutoffs(score_cutoff, adj_p_cutoff, min_features, max_feature)

  ## Convert collections to lists and decide whether this is a self-comparison.
  sig_list1 <- .cos_as_signature_list(sig_list1)
  compare_self <- is.null(sig_list2)
  if (compare_self) {
    sig_list2 <- sig_list1
  } else {
    sig_list2 <- .cos_as_signature_list(sig_list2)
  }

  sig_list1 <- .cos_check_signature_list(sig_list1, "sig_list1")
  sig_list2 <- .cos_check_signature_list(sig_list2, "sig_list2")
  .cos_stop_if_categorical(sig_list1, "sig_list1")
  .cos_stop_if_categorical(sig_list2, "sig_list2")

  ## Signature names must unambiguously identify one signature. Self-comparisons
  ## reuse sig_list1 as sig_list2, so cross-list checks only apply when the
  ## caller supplied a distinct sig_list2 - downstream code (and the heatmap
  ## helper) tells self- from cross-comparisons apart by matching row/column
  ## names, which silently mismatches results if names collide across lists.
  .cos_check_unique_names(sig_list1, "sig_list1")
  if (!compare_self) {
    .cos_check_unique_names(sig_list2, "sig_list2")
    .cos_check_disjoint_names(sig_list1, sig_list2)
  }

  ## If every signature in both lists is uni-directional, there's no
  ## group_label contrast anywhere to pair on: run one flat overlap
  ## comparison instead of the level1_vs_level1/level2_vs_level2 structure
  ## that only makes sense when at least one signature has group_label
  ## levels to position bi-directional signatures against.
  all_uni <- all(vapply(sig_list1, .cos_is_uni, logical(1))) &&
    all(vapply(sig_list2, .cos_is_uni, logical(1)))
  if (method == "overlap" && all_uni) {
    if (is.null(background)) {
      background <- .cos_default_background(sig_list1, sig_list2, feature_col)
    }
    background <- unique(stats::na.omit(as.character(background)))
    labels1 <- stats::setNames(rep(NA_character_, length(sig_list1)), names(sig_list1))
    labels2 <- stats::setNames(rep(NA_character_, length(sig_list2)), names(sig_list2))
    comparisons <- .cos_compare_overlap(
      sig_list1, sig_list2, labels1, labels2, background,
      feature_col, score_col, adj_p_col, group_col,
      score_cutoff, adj_p_cutoff, min_features, max_feature, compare_self,
      adjust, p_adjust_method, alternative
    )
    return(list(method = method, comparisons = comparisons, label_order = NULL, background = background))
  }

  ## KS/GSEA rank sig_list1's feature sets against sig_list2's difexp-derived
  ## ranking; sig_list2 needs difexp regardless of self- vs cross-comparison,
  ## while sig_list1 never does (.cos_signature_features supports both).
  if (method %in% c("ks_rank", "ks_score", "gsea")) {
    .cos_check_ranking_capable(sig_list2, method)
  }

  ## Resolve the per-signature factor levels that should be compared.
  if (compare_self && is.null(label_pairing2)) {
    label_pairing2 <- label_pairing
  }
  label_order1 <- .cos_signature_label_order(sig_list1, group_col, label_pairing, "label_pairing")
  label_order2 <- .cos_signature_label_order(
    sig_list2, group_col, label_pairing2, "label_pairing2",
    ## Self-comparisons re-run this over the same sig_list1/label_pairing;
    ## label_order1 already warned about any mismatch, so don't do it twice.
    warn_on_mismatch = !(compare_self && identical(label_pairing, label_pairing2))
  )
  if (ncol(label_order1) != ncol(label_order2)) {
    stop("sig_list1 and sig_list2 label pairings must have the same length.")
  }
  label_pairs <- data.frame(
    label_pos1 = seq_len(ncol(label_order1)),
    label_pos2 = seq_len(ncol(label_order2))
  )
  ## Build a feature universe for overlap tests when one is not supplied.
  if (method == "overlap" && is.null(background)) {
    background <- .cos_default_background(sig_list1, sig_list2, feature_col)
  }
  if (!is.null(background)) {
    background <- unique(stats::na.omit(as.character(background)))
  }
  res <- stats::setNames(
    vector("list", nrow(label_pairs)),
    paste0("level", label_pairs$label_pos1, "_vs_level", label_pairs$label_pos2)
  )
  ## Run the selected comparison independently for each label pairing.
  for (k in seq_len(nrow(label_pairs))) {
    labels1 <- label_order1[, label_pairs$label_pos1[k]]
    labels2 <- label_order2[, label_pairs$label_pos2[k]]

    if (method == "overlap") {
      res[[k]] <- .cos_compare_overlap(
        sig_list1, sig_list2, labels1, labels2, background,
        feature_col, score_col, adj_p_col, group_col,
        score_cutoff, adj_p_cutoff, min_features, max_feature, compare_self,
        adjust, p_adjust_method, alternative
      )
    } else if (method %in% c("ks_rank", "ks_score")) {
      res[[k]] <- .cos_compare_ks(
        sig_list1, sig_list2, labels1, labels2,
        feature_col, score_col, adj_p_col, p_value_col, group_col,
        score_cutoff, adj_p_cutoff, min_features, max_feature,
        adjust, p_adjust_method, alternative, method
      )
    } else {
      res[[k]] <- .cos_compare_gsea(
        sig_list1, sig_list2, labels1, labels2,
        feature_col, score_col, adj_p_col, p_value_col, group_col,
        score_cutoff, adj_p_cutoff, min_features, max_feature,
        adjust, p_adjust_method, gsea_score, minSize, maxSize, nproc, ...
      )
    }
  }

  list(
    method = method,
    comparisons = res,
    label_order = if (compare_self) {
      list(sig_list1 = label_order1)
    } else {
      list(sig_list1 = label_order1, sig_list2 = label_order2)
    },
    background = background
  )
}

.cos_compare_overlap <- function(sig_list1, sig_list2, labels1, labels2, background,
                                 feature_col, score_col, adj_p_col, group_col,
                                 score_cutoff, adj_p_cutoff, min_features, max_feature,
                                 compare_self,
                                 adjust, p_adjust_method, alternative) {
  ## Convert each signature to a label-specific feature set.
  set_list1 <- lapply(seq_along(sig_list1), function(i) {
    .cos_signature_features(
      sig_list1[[i]], labels1[[i]], feature_col, score_col, adj_p_col, group_col,
      score_cutoff, adj_p_cutoff, min_features, max_feature
    )
  })
  names(set_list1) <- names(sig_list1)
  set_list2 <- lapply(seq_along(sig_list2), function(i) {
    .cos_signature_features(
      sig_list2[[i]], labels2[[i]], feature_col, score_col, adj_p_col, group_col,
      score_cutoff, adj_p_cutoff, min_features, max_feature
    )
  })
  names(set_list2) <- names(sig_list2)

  ## Drop signatures that can't reach min_features even after backfill, so the
  ## self-comparison diagonal below is never hardcoded over a degenerate
  ## (near-)empty feature set.
  dropped <- .cos_drop_small_sets(set_list1, set_list2, min_features, compare_self)
  set_list1 <- dropped$set_list1
  set_list2 <- dropped$set_list2
  if (length(set_list1) == 0 || length(set_list2) == 0) {
    stop("No signatures retained at least min_features = ", min_features,
         " features for method = 'overlap'.")
  }

  jaccard <- pvalue <- matrix(NA_real_, length(set_list1), length(set_list2),
                              dimnames = list(names(set_list1), names(set_list2)))
  counts <- matrix(NA_character_, length(set_list1), length(set_list2),
                   dimnames = list(names(set_list1), names(set_list2)))

  ## Fill pairwise overlap, enrichment p-value, and count summary matrices.
  for (i in seq_along(set_list1)) {
    for (j in seq_along(set_list2)) {
      if (compare_self && j < i) next
      x <- intersect(set_list1[[i]], background)
      y <- intersect(set_list2[[j]], background)
      jaccard[i, j] <- .cos_jaccard(x, y)
      pvalue[i, j] <- .cos_fisher_overlap(x, y, background, alternative)
      counts[i, j] <- paste(length(intersect(x, y)), length(x), length(y), sep = " | ")
    }
  }

  ## Mirror self-comparisons so callers receive full symmetric matrices.
  if (compare_self) {
    jaccard <- .cos_fill_symmetric(jaccard, diag_value = 1)
    pvalue <- .cos_fill_symmetric(pvalue, diag_value = 0)
    counts <- .cos_fill_symmetric(counts, diag_value = diag(counts))
  }
  pvalue <- .cos_adjust_matrix(pvalue, adjust, p_adjust_method, compare_self)

  list(jaccard = jaccard, pvalue = pvalue, counts = counts)
}

.cos_drop_small_sets <- function(set_list1, set_list2, min_features, compare_self) {
  ## Exclude signatures whose retained feature set is too small for a
  ## meaningful overlap test, instead of letting them silently through with a
  ## near-empty or empty set.
  sizes1 <- vapply(set_list1, length, integer(1))
  sizes2 <- vapply(set_list2, length, integer(1))
  small1 <- sizes1 < min_features
  small2 <- sizes2 < min_features

  if (compare_self) {
    ## sig_list1 and sig_list2 are the same signatures; report once.
    small <- small1 | small2
    if (any(small)) {
      warning(
        "Excluding signature(s) with fewer than min_features = ", min_features,
        " retained features: ", paste(names(set_list1)[small], collapse = ", ")
      )
    }
    return(list(set_list1 = set_list1[!small], set_list2 = set_list2[!small]))
  }

  if (any(small1)) {
    warning(
      "sig_list1: excluding signature(s) with fewer than min_features = ", min_features,
      " retained features: ", paste(names(set_list1)[small1], collapse = ", ")
    )
  }
  if (any(small2)) {
    warning(
      "sig_list2: excluding signature(s) with fewer than min_features = ", min_features,
      " retained features: ", paste(names(set_list2)[small2], collapse = ", ")
    )
  }
  list(set_list1 = set_list1[!small1], set_list2 = set_list2[!small2])
}

.cos_compare_ks <- function(sig_list1, sig_list2, labels1, labels2,
                            feature_col, score_col, adj_p_col, p_value_col, group_col,
                            score_cutoff, adj_p_cutoff, min_features, max_feature,
                            adjust, p_adjust_method, alternative, method) {
  score <- pvalue <- matrix(NA_real_, length(sig_list1), length(sig_list2),
                            dimnames = list(names(sig_list1), names(sig_list2)))

  ## Precompute each ranking vector once (it only depends on j); sig_list2
  ## elements without a difexp table, or that are uni-directional (no
  ## group_label contrast to rank from), can't rank at all and stay NA below.
  ranking <- lapply(seq_along(sig_list2), function(j) {
    if (is.null(sig_list2[[j]]$difexp) || .cos_is_uni(sig_list2[[j]])) return(NULL)
    .cos_difexp_scores(sig_list2[[j]], labels2[[j]], feature_col, score_col, p_value_col, group_col)
  })

  ## Compare each retained feature set against each ranked difexp vector.
  for (i in seq_along(sig_list1)) {
    geneset <- .cos_signature_features(
      sig_list1[[i]], labels1[[i]], feature_col, score_col, adj_p_col, group_col,
      score_cutoff, adj_p_cutoff, min_features, max_feature
    )
    for (j in seq_along(sig_list2)) {
      if (is.null(ranking[[j]])) next
      tmp <- .cos_ks_test(geneset, ranking[[j]], alternative, method)
      score[i, j] <- tmp["score"]
      pvalue[i, j] <- tmp["pvalue"]
    }
  }
  pvalue <- .cos_adjust_matrix(pvalue, adjust, p_adjust_method, compare_self = FALSE)

  list(score = score, pvalue = pvalue)
}

.cos_normalize_method <- function(method) {
  ## Preserve legacy calls while reporting the explicit KS flavor downstream.
  if (identical(method, "ks")) return("ks_rank")
  method
}

.cos_compare_gsea <- function(sig_list1, sig_list2, labels1, labels2,
                              feature_col, score_col, adj_p_col, p_value_col, group_col,
                              score_cutoff, adj_p_cutoff, min_features, max_feature,
                              adjust, p_adjust_method, gsea_score,
                              minSize, maxSize, nproc, ...) {
  if (!requireNamespace("fgsea", quietly = TRUE)) {
    stop("Package 'fgsea' is required for method = 'gsea'.")
  }

  score <- pvalue <- matrix(NA_real_, length(sig_list1), length(sig_list2),
                            dimnames = list(names(sig_list1), names(sig_list2)))

  ## Precompute each ranking vector once (it only depends on j); sig_list2
  ## elements without a difexp table, or that are uni-directional (no
  ## group_label contrast to rank from), can't rank at all and stay NA below.
  ranking <- lapply(seq_along(sig_list2), function(j) {
    if (is.null(sig_list2[[j]]$difexp) || .cos_is_uni(sig_list2[[j]])) return(NULL)
    .cos_difexp_scores(sig_list2[[j]], labels2[[j]], feature_col, score_col, p_value_col, group_col)
  })

  ## Run one fgsea test for every feature-set and ranked-vector pair.
  for (i in seq_along(sig_list1)) {
    geneset <- .cos_signature_features(
      sig_list1[[i]], labels1[[i]], feature_col, score_col, adj_p_col, group_col,
      score_cutoff, adj_p_cutoff, min_features, max_feature
    )
    for (j in seq_along(sig_list2)) {
      if (is.null(ranking[[j]])) next
      tmp <- .cos_fgsea(geneset, ranking[[j]], gsea_score, minSize, maxSize, nproc, ...)
      score[i, j] <- tmp["score"]
      pvalue[i, j] <- tmp["pvalue"]
    }
  }
  pvalue <- .cos_adjust_matrix(pvalue, adjust, p_adjust_method, compare_self = FALSE)

  list(score = score, pvalue = pvalue)
}

.cos_as_signature_list <- function(sig_list) {
  if (methods::is(sig_list, "OmicSignatureCollection")) {
    return(sig_list$OmicSigList)
  }
  sig_list
}

.cos_check_signature_list <- function(sig_list, arg_name) {
  ## Enforce the public API contract and fill missing names from metadata.
  if (!is.list(sig_list) || length(sig_list) == 0) {
    stop(arg_name, " must be a non-empty list or OmicSignatureCollection.")
  }
  bad <- !vapply(sig_list, function(x) methods::is(x, "OmicSignature"), logical(1))
  if (any(bad)) {
    bad_names <- names(sig_list)[bad]
    if (is.null(bad_names) || any(bad_names == "")) bad_names <- which(bad)
    stop(arg_name, " contains elements that are not OmicSignature objects: ",
         paste(bad_names, collapse = ", "))
  }
  if (is.null(names(sig_list)) || any(names(sig_list) == "")) {
    names(sig_list) <- vapply(sig_list, .cos_signature_name, character(1))
  }
  sig_list
}

.cos_check_unique_names <- function(sig_list, arg_name) {
  ## Duplicate names make matrix row/column lookups by name ambiguous.
  dup <- unique(names(sig_list)[duplicated(names(sig_list))])
  if (length(dup) > 0) {
    stop(arg_name, " contains duplicate signature names: ", paste(dup, collapse = ", "))
  }
  invisible(TRUE)
}

.cos_check_disjoint_names <- function(sig_list1, sig_list2) {
  ## A shared name across two distinct lists is indistinguishable, downstream,
  ## from a self-comparison; require disjoint names so callers can never hit
  ## that ambiguity instead of silently producing a mismatched result.
  shared <- intersect(names(sig_list1), names(sig_list2))
  if (length(shared) > 0) {
    stop(
      "sig_list1 and sig_list2 must not share signature names when both are ",
      "supplied: ", paste(shared, collapse = ", ")
    )
  }
  invisible(TRUE)
}

.cos_ranking_capable <- function(sig_list2) {
  ## Only a bi-directional signature with a difexp table can build a ranked
  ## vector (.cos_difexp_scores() ranks one group_label level against the
  ## other): a signature-only object can never rank, and neither can a
  ## uni-directional one, which has no group_label contrast to rank from.
  vapply(sig_list2, function(sig) !is.null(sig$difexp) && !.cos_is_uni(sig), logical(1))
}

.cos_check_ranking_capable <- function(sig_list2, method) {
  can_rank <- .cos_ranking_capable(sig_list2)
  if (!any(can_rank)) {
    stop(
      "No signatures in sig_list2 are bi-directional with a difexp table, ",
      "required as the ranking side for method = '", method, "'."
    )
  }
  if (any(!can_rank)) {
    warning(
      "Excluding signature(s) from the ranking side (sig_list2) for method = '",
      method, "' because they are uni-directional or have no difexp table: ",
      paste(names(sig_list2)[!can_rank], collapse = ", "),
      ". They remain available as the geneset side (sig_list1)."
    )
  }
  invisible(can_rank)
}

.cos_check_cutoffs <- function(score_cutoff, adj_p_cutoff, min_features, max_feature) {
  ## Validate feature filtering thresholds before expensive comparisons begin.
  if (!is.numeric(score_cutoff) || length(score_cutoff) != 1 || is.na(score_cutoff) || score_cutoff < 0) {
    stop("score_cutoff must be a single non-negative numeric value.")
  }
  if (!is.numeric(adj_p_cutoff) || length(adj_p_cutoff) != 1 || is.na(adj_p_cutoff) ||
      adj_p_cutoff < 0 || adj_p_cutoff > 1) {
    stop("adj_p_cutoff must be a single numeric value in [0, 1].")
  }
  if (!is.numeric(min_features) || length(min_features) != 1 || is.na(min_features) || min_features < 3) {
    stop("min_features must be a single numeric value >= 3.")
  }
  if (!is.numeric(max_feature) || length(max_feature) != 1 || is.na(max_feature) || max_feature < 1) {
    stop("max_feature must be a single positive numeric value.")
  }
  if (min_features > max_feature) {
    stop("min_features must be <= max_feature.")
  }
  invisible(TRUE)
}

.cos_stop_if_categorical <- function(sig_list, arg_name) {
  ## Exclude categorical signatures until comparison semantics are defined.
  direction_type <- vapply(sig_list, function(sig) {
    type <- sig$metadata$direction_type
    if (is.null(type)) return(NA_character_)
    as.character(type)[1]
  }, character(1))
  if (any(direction_type == "categorical", na.rm = TRUE)) {
    stop("Categorical signatures are not implemented yet: ",
         paste(names(sig_list)[direction_type == "categorical"], collapse = ", "))
  }
  invisible(TRUE)
}

.cos_is_uni <- function(sig) {
  ## Uni-directional signatures have no group_label contrast: no "level" to
  ## pair on, so they're compared as a single, whole feature set instead of
  ## being split by group_label like bi-directional signatures.
  identical(sig$metadata$direction_type, "uni-directional")
}

.cos_signature_name <- function(sig) {
  nm <- sig$metadata$signature_name
  if (is.null(nm) || is.na(nm) || nm == "") {
    nm <- "signature"
  }
  as.character(nm)[1]
}

.cos_signature_label_order <- function(sig_list, group_col, label_pairing, arg_name,
                                        warn_on_mismatch = TRUE) {
  ## Use user-specified label pairings where supplied, otherwise factor levels.
  if (!is.null(label_pairing)) {
    if (!is.list(label_pairing) || is.null(names(label_pairing)) || any(names(label_pairing) == "")) {
      stop(arg_name, " must be a named list.")
    }
    unknown <- setdiff(names(label_pairing), names(sig_list))
    if (length(unknown) > 0) {
      stop(arg_name, " contains names not found in the signature list: ", paste(unknown, collapse = ", "))
    }
  }

  auto_paired <- stats::setNames(logical(length(sig_list)), names(sig_list))

  label_order <- lapply(names(sig_list), function(sig_name) {
    sig <- sig_list[[sig_name]]

    if (.cos_is_uni(sig)) {
      ## No group_label contrast to pair on; a fixed-length NA placeholder
      ## keeps label_order rectangular alongside bi-directional signatures'
      ## real 2-level rows, without participating in level-position pairing
      ## or the mismatch check below.
      if (!is.null(label_pairing) && sig_name %in% names(label_pairing)) {
        stop(arg_name, "[['", sig_name, "']] was supplied, but '", sig_name,
             "' is uni-directional and has no group_label levels to pair.")
      }
      return(c(NA_character_, NA_character_))
    }

    levels <- .cos_group_label_levels(sig, group_col)
    if (!is.null(label_pairing) && sig_name %in% names(label_pairing)) {
      levels <- as.character(label_pairing[[sig_name]])
      if (length(levels) != 2) {
        stop(arg_name, "[['", sig_name, "']] must be a character vector of length 2.")
      }
      available <- .cos_group_label_levels(sig, group_col)
      if (!all(levels %in% available)) {
        stop(arg_name, "[['", sig_name, "']] contains labels not found in ", sig_name, ": ",
             paste(setdiff(levels, available), collapse = ", "))
      }
    } else {
      auto_paired[[sig_name]] <<- TRUE
    }
    levels
  })

  label_order <- do.call(rbind, label_order)
  rownames(label_order) <- names(sig_list)
  colnames(label_order) <- paste0("level", seq_len(ncol(label_order)))

  ## Signatures whose order wasn't pinned by the caller are paired purely by
  ## factor-level position. Different signatures using different label
  ## vocabularies (e.g. different drug names) in the same position is
  ## expected and fine; the real hazard is a label that switches position
  ## across signatures (e.g. "DMSO" is level1 for one signature but level2
  ## for another), since that silently compares mismatched conditions.
  if (warn_on_mismatch && sum(auto_paired) > 1) {
    auto_order <- label_order[auto_paired, , drop = FALSE]
    label_positions <- list()
    for (pos in seq_len(ncol(auto_order))) {
      for (lbl in unique(auto_order[, pos])) {
        label_positions[[lbl]] <- union(label_positions[[lbl]], pos)
      }
    }
    conflicting_labels <- names(label_positions)[lengths(label_positions) > 1]
    if (length(conflicting_labels) > 0) {
      conflicting_rows <- rownames(auto_order)[
        apply(auto_order, 1, function(r) any(r %in% conflicting_labels))
      ]
      msg <- paste0(
        arg_name, ": label(s) ", paste(sprintf("'%s'", conflicting_labels), collapse = ", "),
        " appear at different '", group_col, "' level positions across signatures, so ",
        "level-position pairing (level1 vs level1, level2 vs level2, ...) may not align ",
        "matching conditions across signatures:\n",
        paste(sprintf("  %s: %s", conflicting_rows,
                       apply(auto_order[conflicting_rows, , drop = FALSE], 1, paste, collapse = " vs ")),
              collapse = "\n"),
        "\nSupply ", arg_name, " to pin explicit level pairings if these should be compared."
      )
      warning(structure(
        class = c("cos_label_order_mismatch", "warning", "condition"),
        list(message = msg, call = NULL)
      ))
    }
  }

  label_order
}

.cos_group_label_levels <- function(sig, group_col) {
  ## Determine the two bi-directional levels to compare. Prefer difexp's
  ## levels when difexp is present: it's the table KS/GSEA ranking and
  ## difexp-backed overlap filtering actually read from, and
  ## standardizeSigDF() can drop factor levels from `signature` that are
  ## absent after signature-level filtering (e.g. an all-"up" signature table
  ## backed by a legitimately bi-directional difexp table).
  sigdf <- sig$signature
  .cos_require_col(group_col, sigdf)
  if (!is.factor(sigdf[[group_col]])) {
    stop("Column '", group_col, "' must be a factor in signature '", .cos_signature_name(sig), "'.")
  }

  if (!is.null(sig$difexp)) {
    .cos_require_col(group_col, sig$difexp)
    if (!is.factor(sig$difexp[[group_col]])) {
      stop("Column '", group_col, "' must be a factor in difexp for signature '",
           .cos_signature_name(sig), "'.")
    }
    levels <- levels(sig$difexp[[group_col]])
  } else {
    levels <- levels(sigdf[[group_col]])
  }

  if (length(levels) != 2) {
    stop("Column '", group_col, "' must have exactly 2 factor levels in signature '",
         .cos_signature_name(sig), "'.")
  }
  levels
}

.cos_signature_features <- function(sig, label, feature_col, score_col, adj_p_col, group_col,
                                    score_cutoff, adj_p_cutoff, min_features, max_feature) {
  ## Uni-directional signatures have no group_label contrast: use the whole
  ## signature/difexp table as a single feature set instead of filtering to
  ## one group_label value.
  label <- if (.cos_is_uni(sig)) NULL else label

  ## Prefer difexp so caller cutoffs can override stored signature cutoffs.
  if (!is.null(sig$difexp)) {
    df <- .cos_constrained_signature_df(
      sig$difexp, label, feature_col, score_col, adj_p_col, group_col,
      score_cutoff, adj_p_cutoff, min_features, max_feature
    )
    return(unique(stats::na.omit(as.character(df[[feature_col]]))))
  }

  full_df <- sig$signature
  if (!is.null(label)) {
    full_df <- full_df %>% dplyr::filter(as.character(.data[[group_col]]) == label)
  }
  full_df <- full_df %>%
    .cos_order_signature_rows(score_col, adj_p_col) %>%
    dplyr::distinct(.data[[feature_col]], .keep_all = TRUE)

  ## Without a difexp table, score_cutoff/adj_p_cutoff can only be honored if
  ## the signature table itself carries the relevant column; warn rather than
  ## silently skipping the filter the caller asked for.
  has_score <- score_col %in% colnames(full_df)
  has_adj_p <- adj_p_col %in% colnames(full_df)
  unmet <- character()

  selected <- full_df
  if (has_score) {
    selected <- selected %>% dplyr::filter(abs(.data[[score_col]]) >= score_cutoff)
  } else if (score_cutoff > 0) {
    unmet <- c(unmet, paste0("score_cutoff (no '", score_col, "' column)"))
  }

  if (has_adj_p) {
    selected <- selected %>% dplyr::filter(!is.na(.data[[adj_p_col]]), .data[[adj_p_col]] <= adj_p_cutoff)
  } else if (adj_p_cutoff < 1) {
    unmet <- c(unmet, paste0("adj_p_cutoff (no '", adj_p_col, "' column)"))
  }

  if (length(unmet) > 0) {
    warning(
      "Signature '", .cos_signature_name(sig), "' has no difexp table; ",
      paste(unmet, collapse = " and "), " could not be applied to its signature table."
    )
  }

  ## Backfill with the strongest remaining features if the cutoffs (or their
  ## absence) left too few rows, mirroring .cos_constrained_signature_df().
  if (nrow(selected) < min_features) {
    add <- full_df %>%
      dplyr::filter(!.data[[feature_col]] %in% selected[[feature_col]]) %>%
      dplyr::slice_head(n = min_features - nrow(selected))
    selected <- dplyr::bind_rows(selected, add)
  }

  selected %>%
    dplyr::slice_head(n = max_feature) %>%
    dplyr::pull(dplyr::all_of(feature_col)) %>%
    as.character() %>%
    stats::na.omit() %>%
    unique()
}

.cos_difexp_scores <- function(sig, label, feature_col, score_col, p_value_col, group_col) {
  ## Rank from most significant selected-label features to contrast features.
  if (is.null(sig$difexp)) {
    stop("Signature '", .cos_signature_name(sig), "' does not contain a difexp table.")
  }
  .cos_require_col(feature_col, sig$difexp)
  .cos_require_col(score_col, sig$difexp)
  .cos_require_col(p_value_col, sig$difexp)
  .cos_require_col(group_col, sig$difexp)

  levels <- .cos_group_label_levels(sig, group_col)
  contrast_label <- setdiff(levels, label)
  if (length(contrast_label) != 1) {
    stop("Could not identify one contrasting label for '", label, "'.")
  }

  ranked_sig <- sig$difexp %>%
    dplyr::select(dplyr::all_of(c(feature_col, score_col, p_value_col, group_col))) %>%
    dplyr::filter(
      !is.na(.data[[feature_col]]),
      .data[[feature_col]] != "",
      !is.na(.data[[score_col]]),
      !is.na(.data[[p_value_col]]),
      .data[[p_value_col]] >= 0,
      as.character(.data[[group_col]]) %in% c(label, contrast_label)
    ) %>%
    dplyr::mutate(
      .p_rank = -log10(pmax(.data[[p_value_col]], .Machine$double.eps)),
      .rank_score = ifelse(
        as.character(.data[[group_col]]) == label,
        .data$.p_rank,
        -.data$.p_rank
      ),
      .abs_score = abs(.data[[score_col]])
    ) %>%
    dplyr::arrange(dplyr::desc(.data$.rank_score), dplyr::desc(.data$.p_rank), dplyr::desc(.data$.abs_score)) %>%
    dplyr::distinct(.data[[feature_col]], .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(c(feature_col, ".rank_score"))) %>%
    tibble::deframe()
}

.cos_constrained_signature_df <- function(df, label, feature_col, score_col, adj_p_col, group_col,
                                          score_cutoff, adj_p_cutoff, min_features, max_feature) {
  ## Apply score and p-value filters within one label-specific difexp table.
  ## label = NULL means "no group_label contrast" (a uni-directional
  ## signature): use every row instead of filtering to one group_label value.
  .cos_require_col(feature_col, df)
  .cos_require_col(score_col, df)
  .cos_require_col(adj_p_col, df)

  if (!is.null(label)) {
    .cos_require_col(group_col, df)
    df <- df %>% dplyr::filter(as.character(.data[[group_col]]) == label)
  }

  df <- df %>%
    dplyr::filter(
      !is.na(.data[[feature_col]]),
      .data[[feature_col]] != ""
    ) %>%
    .cos_order_signature_rows(score_col, adj_p_col) %>%
    dplyr::distinct(.data[[feature_col]], .keep_all = TRUE)

  selected <- df %>%
    dplyr::filter(
      abs(.data[[score_col]]) >= score_cutoff,
      !is.na(.data[[adj_p_col]]),
      .data[[adj_p_col]] <= adj_p_cutoff
    )

  ## Backfill with strongest features if strict cutoffs return too few rows.
  if (nrow(selected) < min_features) {
    add <- df %>%
      dplyr::filter(!.data[[feature_col]] %in% selected[[feature_col]]) %>%
      dplyr::slice_head(n = min_features - nrow(selected))
    selected <- dplyr::bind_rows(selected, add)
  }

  selected %>%
    dplyr::slice_head(n = max_feature)
}

.cos_order_signature_rows <- function(df, score_col, adj_p_col) {
  df %>%
    dplyr::mutate(
      .adj_order = if (adj_p_col %in% colnames(df)) .data[[adj_p_col]] else 1,
      .score_order = if (score_col %in% colnames(df)) abs(.data[[score_col]]) else 0
    ) %>%
    dplyr::arrange(.data$.adj_order, dplyr::desc(.data$.score_order)) %>%
    dplyr::select(-dplyr::all_of(c(".adj_order", ".score_order")))
}

.cos_default_background <- function(sig_list1, sig_list2, feature_col) {
  ## Use every observed feature as the default overlap universe.
  unique(c(
    unlist(lapply(sig_list1, .cos_all_features, feature_col = feature_col), use.names = FALSE),
    unlist(lapply(sig_list2, .cos_all_features, feature_col = feature_col), use.names = FALSE)
  ))
}

.cos_all_features <- function(sig, feature_col) {
  ## Collect features from both stored signatures and difexp tables.
  out <- character()
  if (feature_col %in% colnames(sig$signature)) {
    out <- c(out, as.character(sig$signature[[feature_col]]))
  }
  if (!is.null(sig$difexp) && feature_col %in% colnames(sig$difexp)) {
    out <- c(out, as.character(sig$difexp[[feature_col]]))
  }
  unique(stats::na.omit(out))
}

.cos_jaccard <- function(x, y) {
  denom <- length(union(x, y))
  if (denom == 0) return(NA_real_)
  length(intersect(x, y)) / denom
}

.cos_fisher_overlap <- function(x, y, background, alternative) {
  ## Build a 2x2 contingency table inside the fixed feature universe.
  x <- intersect(unique(x), background)
  y <- intersect(unique(y), background)
  n11 <- length(intersect(x, y))
  n10 <- length(setdiff(x, y))
  n01 <- length(setdiff(y, x))
  n00 <- length(background) - n11 - n10 - n01
  if (n00 < 0) stop("Invalid overlap table; check background.")
  stats::fisher.test(matrix(c(n11, n10, n01, n00), nrow = 2), alternative = alternative)$p.value
}

.cos_ks_test <- function(geneset, stats_vec, alternative, method) {
  ## Dispatch to the requested KS semantics.
  if (identical(method, "ks_score")) {
    return(.cos_ks_score_test(geneset, stats_vec, alternative))
  }
  .cos_ks_rank_test(geneset, stats_vec, alternative)
}

.cos_ks_rank_test <- function(geneset, stats_vec, alternative) {
  ## Test where geneset members fall in the ranked stats vector.
  geneset <- intersect(unique(geneset), names(stats_vec))
  positions <- stats::na.omit(match(geneset, names(stats_vec)))
  n_stats <- length(stats_vec)
  n_geneset <- length(positions)
  if (n_geneset < 1 || n_stats < 2) {
    return(c(score = NA_real_, pvalue = NA_real_))
  }
  ## Compute the signed running enrichment statistic used by hypeR.
  positions <- sort(positions)
  hit_step <- 1 / n_geneset
  miss_step <- 1 / n_stats
  steps <- sort(c(positions - 1, positions))
  steps <- steps[c(TRUE, diff(steps) != 0)]
  hits_seen <- rep(0, length(steps))
  hits_seen[match(positions, steps)] <- seq_len(n_geneset)
  empty_steps <- which(hits_seen == 0)[-1]
  hits_seen[empty_steps] <- hits_seen[empty_steps - 1]
  running_score <- hits_seen * hit_step - steps * miss_step
  score <- running_score[which.max(abs(running_score))]

  ## Map ranking semantics to ks.test tails: top-ranked hits have lower ranks.
  ks_alternative <- switch(
    alternative,
    greater = "less",
    less = "greater",
    two.sided = "two.sided",
    stop("'alternative' must be one of 'greater', 'less', or 'two.sided'.")
  )
  tmp <- suppressWarnings(stats::ks.test(seq_len(n_stats), positions, alternative = ks_alternative))
  c(score = unname(score), pvalue = tmp$p.value)
}

.cos_ks_score_test <- function(geneset, stats_vec, alternative) {
  ## Compare geneset score values against all non-geneset score values.
  geneset <- intersect(unique(geneset), names(stats_vec))
  outside <- setdiff(names(stats_vec), geneset)
  if (length(geneset) < 1 || length(outside) < 1) {
    return(c(score = NA_real_, pvalue = NA_real_))
  }
  geneset_scores <- stats_vec[geneset]
  outside_scores <- stats_vec[outside]

  ## Map top-ranking enrichment to the KS tail where geneset scores are larger.
  ks_alternative <- switch(
    alternative,
    greater = "less",
    less = "greater",
    two.sided = "two.sided",
    stop("'alternative' must be one of 'greater', 'less', or 'two.sided'.")
  )
  tmp <- suppressWarnings(stats::ks.test(geneset_scores, outside_scores, alternative = ks_alternative))
  score_sign <- sign(mean(geneset_scores, na.rm = TRUE) - mean(outside_scores, na.rm = TRUE))
  if (is.na(score_sign) || score_sign == 0) score_sign <- 1
  c(score = unname(tmp$statistic) * score_sign, pvalue = tmp$p.value)
}

.cos_fgsea <- function(geneset, stats_vec, gsea_score, minSize, maxSize, nproc, ...) {
  ## Run fgsea on one retained feature set against one ranked vector.
  geneset <- intersect(unique(geneset), names(stats_vec))
  if (length(geneset) < minSize || length(stats_vec) < 2) {
    return(c(score = NA_real_, pvalue = NA_real_))
  }

  fg <- fgsea::fgsea(
    pathways = list(signature = geneset),
    stats = stats_vec,
    minSize = minSize,
    maxSize = maxSize,
    nproc = nproc,
    ...
  )
  if (!nrow(fg)) return(c(score = NA_real_, pvalue = NA_real_))
  if (!gsea_score %in% colnames(fg)) {
    stop("fgsea result does not contain column '", gsea_score, "'.")
  }
  c(score = fg[[gsea_score]][1], pvalue = fg$pval[1])
}

.cos_adjust_matrix <- function(pvalue, adjust, p_adjust_method, compare_self) {
  ## Optionally correct p-values while preserving self-comparison symmetry.
  if (!adjust) return(pvalue)
  if (compare_self && nrow(pvalue) == ncol(pvalue)) {
    idx <- upper.tri(pvalue, diag = FALSE)
    pvalue[idx] <- stats::p.adjust(pvalue[idx], method = p_adjust_method)
    pvalue[lower.tri(pvalue)] <- t(pvalue)[lower.tri(pvalue)]
    return(pvalue)
  }
  pvalue[] <- stats::p.adjust(as.vector(pvalue), method = p_adjust_method)
  pvalue
}

.cos_fill_symmetric <- function(mx, diag_value = NA_real_) {
  mx[lower.tri(mx)] <- t(mx)[lower.tri(mx)]
  diag(mx) <- diag_value
  mx
}

.cos_require_col <- function(col, df) {
  if (!length(col) || !col[1] %in% colnames(df)) {
    stop("Column '", col[1], "' not found.")
  }
  col[1]
}
