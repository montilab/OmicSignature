#######################################################################
## function: COMPARE OMIC SIGNATURES
##
## Pairwise comparison of OmicSignature objects by overlap, KS, or GSEA.
#######################################################################

#' Compare OmicSignature objects
#'
#' Compare signatures within one list, or between two lists, using set overlap,
#' a two-sample Kolmogorov-Smirnov test, or fgsea.
#'
#' @param sig_list1 List of OmicSignature objects or an OmicSignatureCollection.
#' @param sig_list2 Optional second list of OmicSignature objects or collection.
#' @param method Comparison method.
#' @param background Optional background feature vector for overlap tests.
#' @param score_cutoff Minimum absolute score to include in a signature.
#' @param adj_p_cutoff Maximum adjusted p-value to include in a signature.
#' @param min_features Minimum number of features retained per label-specific
#'   signature.
#' @param max_feature Maximum number of features retained per label-specific
#'   signature.
#' @param label_pairing Optional named list giving the two group-label levels
#'   to use for each signature in `sig_list1`.
#' @param label_pairing2 Optional named list giving the two group-label levels
#'   to use for each signature in `sig_list2`.
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
#' @details For `method = "ks"` and `method = "gsea"`, each ranked vector is
#'   built from both phenotype labels. Features in the requested `group_col`
#'   label are ranked by positive `-log10(p_value)`, and features in the
#'   contrasting label are ranked by negative `-log10(p_value)`. This places
#'   the most significant selected-label features at the top of the ranking and
#'   the most significant contrast-label features at the bottom. KS compares the
#'   positions of each retained feature set within this ranked vector.
#'
#' @return A list with one element per label pairing. For `method = "overlap"`
#'   each element contains `jaccard`, `pvalue`, and `counts` matrices. `counts`
#'   entries are formatted as `"ov | n1 | n2"`. For `method = "ks"` and
#'   `method = "gsea"` each element contains `score` and `pvalue` matrices.
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
#' @export
compare_omic_signatures <- function(
    sig_list1,
    sig_list2 = NULL,
    method = c("overlap", "ks", "gsea"),
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

  ## Resolve the per-signature factor levels that should be compared.
  if (compare_self && is.null(label_pairing2)) {
    label_pairing2 <- label_pairing
  }
  label_order1 <- .cos_signature_label_order(sig_list1, group_col, label_pairing, "label_pairing")
  label_order2 <- .cos_signature_label_order(sig_list2, group_col, label_pairing2, "label_pairing2")
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
    } else if (method == "ks") {
      res[[k]] <- .cos_compare_ks(
        sig_list1, sig_list2, labels1, labels2,
        feature_col, score_col, adj_p_col, p_value_col, group_col,
        score_cutoff, adj_p_cutoff, min_features, max_feature,
        adjust, p_adjust_method, alternative
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

.cos_compare_ks <- function(sig_list1, sig_list2, labels1, labels2,
                            feature_col, score_col, adj_p_col, p_value_col, group_col,
                            score_cutoff, adj_p_cutoff, min_features, max_feature,
                            adjust, p_adjust_method, alternative) {
  score <- pvalue <- matrix(NA_real_, length(sig_list1), length(sig_list2),
                            dimnames = list(names(sig_list1), names(sig_list2)))

  ## Compare each retained feature set against each ranked difexp vector.
  for (i in seq_along(sig_list1)) {
    geneset <- .cos_signature_features(
      sig_list1[[i]], labels1[[i]], feature_col, score_col, adj_p_col, group_col,
      score_cutoff, adj_p_cutoff, min_features, max_feature
    )
    for (j in seq_along(sig_list2)) {
      stats_j <- .cos_difexp_scores(sig_list2[[j]], labels2[[j]], feature_col, score_col, p_value_col, group_col)
      tmp <- .cos_ks_test(geneset, stats_j, alternative)
      score[i, j] <- tmp["score"]
      pvalue[i, j] <- tmp["pvalue"]
    }
  }
  pvalue <- .cos_adjust_matrix(pvalue, adjust, p_adjust_method, compare_self = FALSE)

  list(score = score, pvalue = pvalue)
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

  ## Run one fgsea test for every feature-set and ranked-vector pair.
  for (i in seq_along(sig_list1)) {
    geneset <- .cos_signature_features(
      sig_list1[[i]], labels1[[i]], feature_col, score_col, adj_p_col, group_col,
      score_cutoff, adj_p_cutoff, min_features, max_feature
    )
    for (j in seq_along(sig_list2)) {
      stats_j <- .cos_difexp_scores(sig_list2[[j]], labels2[[j]], feature_col, score_col, p_value_col, group_col)
      tmp <- .cos_fgsea(geneset, stats_j, gsea_score, minSize, maxSize, nproc, ...)
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

.cos_check_cutoffs <- function(score_cutoff, adj_p_cutoff, min_features, max_feature) {
  ## Validate feature filtering thresholds before expensive comparisons begin.
  if (!is.numeric(score_cutoff) || length(score_cutoff) != 1 || is.na(score_cutoff) || score_cutoff < 0) {
    stop("score_cutoff must be a single non-negative numeric value.")
  }
  if (!is.numeric(adj_p_cutoff) || length(adj_p_cutoff) != 1 || is.na(adj_p_cutoff) ||
      adj_p_cutoff < 0 || adj_p_cutoff > 1) {
    stop("adj_p_cutoff must be a single numeric value in [0, 1].")
  }
  if (!is.numeric(min_features) || length(min_features) != 1 || is.na(min_features) || min_features < 0) {
    stop("min_features must be a single non-negative numeric value.")
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

.cos_signature_name <- function(sig) {
  nm <- sig$metadata$signature_name
  if (is.null(nm) || is.na(nm) || nm == "") {
    nm <- "signature"
  }
  as.character(nm)[1]
}

.cos_signature_label_order <- function(sig_list, group_col, label_pairing, arg_name) {
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

  label_order <- lapply(names(sig_list), function(sig_name) {
    sig <- sig_list[[sig_name]]
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
    }
    levels
  })

  label_order <- do.call(rbind, label_order)
  rownames(label_order) <- names(sig_list)
  colnames(label_order) <- paste0("level", seq_len(ncol(label_order)))
  label_order
}

.cos_group_label_levels <- function(sig, group_col) {
  ## Read the two bi-directional levels from the signature table.
  sigdf <- sig$signature
  .cos_require_col(group_col, sigdf)
  if (!is.factor(sigdf[[group_col]])) {
    stop("Column '", group_col, "' must be a factor in signature '", .cos_signature_name(sig), "'.")
  }
  levels <- levels(sigdf[[group_col]])
  if (length(levels) != 2) {
    stop("Column '", group_col, "' must have exactly 2 factor levels in signature '",
         .cos_signature_name(sig), "'.")
  }
  if (!is.null(sig$difexp)) {
    .cos_require_col(group_col, sig$difexp)
    if (!is.factor(sig$difexp[[group_col]])) {
      stop("Column '", group_col, "' must be a factor in difexp for signature '",
           .cos_signature_name(sig), "'.")
    }
  }
  levels
}

.cos_signature_features <- function(sig, label, feature_col, score_col, adj_p_col, group_col,
                                    score_cutoff, adj_p_cutoff, min_features, max_feature) {
  ## Prefer difexp so caller cutoffs can override stored signature cutoffs.
  if (!is.null(sig$difexp)) {
    df <- .cos_constrained_signature_df(
      sig$difexp, label, feature_col, score_col, adj_p_col, group_col,
      score_cutoff, adj_p_cutoff, min_features, max_feature
    )
    return(unique(stats::na.omit(as.character(df[[feature_col]]))))
  }

  df <- sig$signature %>%
    dplyr::filter(as.character(.data[[group_col]]) == label)
  if (score_col %in% colnames(df)) {
    df <- df %>%
      dplyr::filter(abs(.data[[score_col]]) >= score_cutoff) %>%
      .cos_order_signature_rows(score_col, adj_p_col)
  }
  df %>%
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
      .p_rank = -log10(pmax(.data[[p_value_col]], .Machine$double.xmin)),
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
  .cos_require_col(feature_col, df)
  .cos_require_col(score_col, df)
  .cos_require_col(adj_p_col, df)
  .cos_require_col(group_col, df)

  df <- df %>%
    dplyr::filter(
      as.character(.data[[group_col]]) == label,
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

.cos_ks_test <- function(geneset, stats_vec, alternative) {
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
