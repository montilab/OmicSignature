#######################################################################
## function: SIGNATURE SIMILARITY HEATMAP
#######################################################################

#' Plot signature similarity heatmaps
#'
#' Plot one or more ComplexHeatmap heatmaps from the object returned by
#' `compare_omic_signatures()`. Overlap comparisons can show Jaccard
#' similarity or p-value-based similarity on a `-log10(p-value)` scale.
#' Asymmetric KS and GSEA comparisons can show score or `-log10(p-value)`
#' matrices. For these comparisons, `mode = "combined"` draws split cells: the
#' top-right triangle shows `level2_vs_level2` and the bottom-left triangle
#' shows `level1_vs_level1`. `mode = "separate"`
#' draws one full heatmap per level, and `mode = "split"` is invalid. Legends
#' for level-specific displays use abbreviated labels such as `"lev1_vs_lev1"`.
#'
#' @param comparison Output from `compare_omic_signatures()`.
#' @param measure One of `"jaccard"`, `"score"`, or `"pvalue"`.
#' @param mode One of `"separate"`, `"combined"`, or `"split"`.
#' @param annotation Optional ComplexHeatmap::HeatmapAnnotation object.
#' @param annotation_side Where to place annotation: `"column"` or `"row"`.
#' @param triangle Triangle to display for symmetric overlap heatmaps in
#'   `"separate"` and `"combined"` modes.
#' @param cluster_method Hierarchical clustering method passed to hclust or cba.
#' @param col_fun Color function for separate heatmaps.
#' @param pos_col_fun Color function for first-level triangles in combined mode.
#' @param neg_col_fun Color function for second-level triangles in combined mode.
#' @param combined_triangle_threshold Maximum matrix size for split triangles
#'   inside each cell in combined mode.
#' @param draw Logical; draw the heatmap before returning it.
#' @param ... Additional arguments passed to ComplexHeatmap::Heatmap().
#'
#' @return A ComplexHeatmap Heatmap or HeatmapList object, invisibly if
#'   `draw = TRUE`.
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
#'
#' if (requireNamespace("ComplexHeatmap", quietly = TRUE) &&
#'     requireNamespace("circlize", quietly = TRUE)) {
#'   signature_similarity_heatmap(overlap_res, draw = FALSE)
#' }
#'
#' ks_res <- compare_omic_signatures(
#'   compare_signatures_example[1:2],
#'   method = "ks_rank",
#'   adj_p_cutoff = 0.01,
#'   min_features = 10
#' )
#'
#' if (requireNamespace("ComplexHeatmap", quietly = TRUE) &&
#'     requireNamespace("circlize", quietly = TRUE)) {
#'   signature_similarity_heatmap(
#'     ks_res,
#'     measure = "score",
#'     mode = "combined",
#'     draw = FALSE
#'   )
#' }
#'
#' @export
signature_similarity_heatmap <- function(
    comparison,
    measure = c("jaccard", "score", "pvalue"),
    mode = c("separate", "combined", "split"),
    annotation = NULL,
    annotation_side = c("column", "row"),
    triangle = c("upper", "lower"),
    cluster_method = "ward.D",
    col_fun = NULL,
    pos_col_fun = NULL,
    neg_col_fun = NULL,
    combined_triangle_threshold = 50,
    draw = TRUE,
    ...)
{
  ## Normalize plotting options and verify optional graphics dependencies.
  measure <- match.arg(measure)
  mode <- match.arg(mode)
  annotation_side <- match.arg(annotation_side)
  triangle <- match.arg(triangle)

  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    stop("Package 'ComplexHeatmap' is required.")
  }
  if (!requireNamespace("circlize", quietly = TRUE)) {
    stop("Package 'circlize' is required.")
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required.")
  }
  if (!is.numeric(combined_triangle_threshold) ||
      length(combined_triangle_threshold) != 1 ||
      is.na(combined_triangle_threshold) ||
      combined_triangle_threshold < 0) {
    stop("combined_triangle_threshold must be a single non-negative number.")
  }

  ## Convert comparison output into one or two plottable similarity matrices.
  sim <- .ssh_similarity_from_comparison(comparison, measure)
  sim_names <- attr(sim, "comparison_names")
  legend_names <- .ssh_abbreviate_comparison_names(sim_names)
  comparison_method <- attr(sim, "comparison_method")
  is_rank_based <- comparison_method %in% c("ks", "ks_rank", "ks_score", "gsea")
  if (mode %in% c("combined", "split") && is.null(sim$negative)) {
    stop(mode, " mode requires two similarity matrices.")
  }
  if (is_rank_based && mode == "split") {
    stop("split mode is not supported for rank-based comparisons.")
  }

  ## Choose default color scales from the observed similarity range.
  value_range <- range(unlist(sim, use.names = FALSE), na.rm = TRUE)
  if (!all(is.finite(value_range))) value_range <- c(0, 1)
  max_abs_value <- max(abs(value_range))
  if (!is.finite(max_abs_value) || max_abs_value <= 0) max_abs_value <- 1
  max_value <- max(value_range)
  if (!is.finite(max_value) || max_value <= 0) max_value <- 1
  if (is.null(col_fun)) {
    col_fun <- if (value_range[1] < 0) {
      circlize::colorRamp2(
        c(-max_abs_value, 0, max_abs_value),
        c("#2166ac", "white", "#b2182b")
      )
    } else {
      circlize::colorRamp2(c(0, max_value), c("white", "#b2182b"))
    }
  }
  if (is.null(pos_col_fun)) {
    pos_col_fun <- col_fun
  }
  if (is.null(neg_col_fun)) {
    neg_col_fun <- col_fun
  }

  heatmap_args <- list(...)
  if (mode %in% c("separate", "combined")) {
    if (!"column_names_side" %in% names(heatmap_args) && triangle == "upper") {
      heatmap_args$column_names_side <- "top"
    }
    if (!"row_names_side" %in% names(heatmap_args) && triangle == "lower") {
      heatmap_args$row_names_side <- "left"
    }
  }

  ## Use shared row/column ordering so separate positive/negative maps align.
  order_basis <- if (is.null(sim$negative)) sim$positive else (sim$positive + sim$negative) / 2
  row_order <- rownames(order_basis)[.ssh_similarity_order(order_basis, margin = "row", method = cluster_method)]
  column_order <- colnames(order_basis)[.ssh_similarity_order(order_basis, margin = "column", method = cluster_method)]
  is_self_similarity <- identical(rownames(order_basis), colnames(order_basis))

  mask_redundant_triangle <- function(mat) {
    ## Hide the redundant half of symmetric self-comparison matrices.
    if (is_rank_based || !is_self_similarity) return(mat)
    row_pos <- match(rownames(mat), row_order)
    col_pos <- match(colnames(mat), column_order)
    displayed_lower <- outer(row_pos, col_pos, `>`)
    displayed_upper <- outer(row_pos, col_pos, `<`)
    if (triangle == "upper") {
      mat[displayed_lower] <- NA_real_
    } else {
      mat[displayed_upper] <- NA_real_
    }
    mat
  }
  is_visible_triangle_cell <- function(i, j) {
    ## Restrict custom triangle drawing to the requested matrix half.
    if (is_rank_based || !is_self_similarity) return(TRUE)
    row_pos <- match(rownames(order_basis)[i], row_order)
    col_pos <- match(colnames(order_basis)[j], column_order)
    if (row_pos == col_pos) return(TRUE)
    if (triangle == "upper") return(row_pos < col_pos)
    row_pos > col_pos
  }

  annotation_args <- .ssh_annotation_args(annotation, annotation_side)
  common_args <- c(
    list(
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      row_order = row_order,
      column_order = column_order
    ),
    annotation_args,
    heatmap_args
  )
  custom_heatmap_legends <- list()

  if (mode == "separate") {
    ## Draw one heatmap per comparison level.
    positive_args <- .ssh_set_legend_title(common_args, legend_names[1])
    positive_args$column_title <- sim_names[1]
    positive_ht <- do.call(
      ComplexHeatmap::Heatmap,
      c(
        list(
          matrix = mask_redundant_triangle(sim$positive),
          name = legend_names[1],
          col = col_fun,
          na_col = "#FFFFFF00"
        ),
        positive_args
      )
    )
    ht <- positive_ht
    if (!is.null(sim$negative)) {
      negative_args <- .ssh_set_legend_title(common_args, legend_names[2])
      if (annotation_side == "column") negative_args$top_annotation <- NULL
      if (annotation_side == "row") negative_args$left_annotation <- NULL
      negative_args$column_title <- sim_names[2]
      negative_ht <- do.call(
        ComplexHeatmap::Heatmap,
        c(
          list(
            matrix = mask_redundant_triangle(sim$negative),
            name = legend_names[2],
            col = col_fun,
            na_col = "#FFFFFF00"
          ),
          negative_args
        )
      )
      ht <- positive_ht + negative_ht
    }
  } else if (mode == "split") {
    if (!is_self_similarity) {
      stop("split mode requires square self-comparison matrices.")
    }
    ## Combine two matrices by assigning each to one triangle.
    split_mat <- sim$positive
    split_mat[lower.tri(split_mat)] <- sim$negative[lower.tri(sim$negative)]
    ht <- do.call(
      ComplexHeatmap::Heatmap,
      c(
        list(
          matrix = split_mat,
          name = measure,
          col = col_fun
        ),
        common_args
      )
    )
  } else {
    use_triangle_cells <- nrow(sim$positive) <= combined_triangle_threshold &&
      ncol(sim$positive) <= combined_triangle_threshold

    if (use_triangle_cells) {
      ## Draw two colored triangles inside each visible heatmap cell.
      combined_mat <- sim$positive
      combined_mat[] <- NA_real_
      custom_heatmap_legends <- .ssh_combined_legends(
        sim, measure, legend_names, pos_col_fun, neg_col_fun
      )
      ht <- do.call(
        ComplexHeatmap::Heatmap,
        c(
          list(
            matrix = combined_mat,
            name = measure,
            col = circlize::colorRamp2(c(0, max_value), c("white", "white")),
            na_col = "#FFFFFF00",
            show_heatmap_legend = FALSE,
            rect_gp = grid::gpar(type = "none"),
            cell_fun = function(j, i, x, y, width, height, fill) {
              if (!is_visible_triangle_cell(i, j)) return(NULL)
              top_right_value <- if (is_rank_based) {
                sim$negative[i, j]
              } else {
                sim$positive[i, j]
              }
              bottom_left_value <- if (is_rank_based) {
                sim$positive[i, j]
              } else {
                sim$negative[i, j]
              }
              grid::grid.polygon(
                x = grid::unit.c(x - width / 2, x + width / 2, x + width / 2),
                y = grid::unit.c(y + height / 2, y + height / 2, y - height / 2),
                gp = grid::gpar(col = NA, fill = neg_col_fun(top_right_value))
              )
              grid::grid.polygon(
                x = grid::unit.c(x - width / 2, x - width / 2, x + width / 2),
                y = grid::unit.c(y + height / 2, y - height / 2, y - height / 2),
                gp = grid::gpar(col = NA, fill = pos_col_fun(bottom_left_value))
              )
              grid::grid.rect(
                x = x,
                y = y,
                width = width,
                height = height,
                gp = grid::gpar(col = "#d9d9d9", fill = NA, lwd = 0.3)
              )
            }
          ),
          common_args
        )
      )
    } else {
      ## Fall back to the average similarity for large combined heatmaps.
      average_mat <- mask_redundant_triangle((sim$positive + sim$negative) / 2)
      ht <- do.call(
        ComplexHeatmap::Heatmap,
        c(
          list(
            matrix = average_mat,
            name = measure,
            col = col_fun,
            na_col = "#FFFFFF00"
          ),
          common_args
        )
      )
    }
  }

  if (length(custom_heatmap_legends) > 0) {
    attr(ht, "heatmap_legend_list") <- custom_heatmap_legends
  }
  if (draw) {
    draw_args <- list(object = ht)
    if (length(custom_heatmap_legends) > 0) {
      draw_args$heatmap_legend_list <- custom_heatmap_legends
    }
    return(invisible(do.call(ComplexHeatmap::draw, draw_args)))
  }
  ht
}

.ssh_similarity_from_comparison <- function(comparison, measure) {
  ## Extract Jaccard, score, or transformed p-value matrices.
  if (!is.list(comparison) || is.null(comparison$comparisons)) {
    stop("comparison must be an object returned by compare_omic_signatures().")
  }
  comparison_method <- comparison$method
  if (is.null(comparison_method)) comparison_method <- "overlap"
  if (measure == "jaccard" && comparison_method != "overlap") {
    stop("measure = 'jaccard' is only available for overlap comparisons.")
  }
  if (measure == "score" && comparison_method == "overlap") {
    stop("measure = 'score' is only available for rank-based comparisons.")
  }
  matrices <- lapply(comparison$comparisons, function(x) {
    if (measure == "jaccard") {
      if (is.null(x$jaccard)) stop("Comparison object does not contain jaccard matrices.")
      return(x$jaccard)
    }
    if (measure == "score") {
      if (is.null(x$score)) stop("Comparison object does not contain score matrices.")
      return(x$score)
    }
    if (is.null(x$pvalue)) stop("Comparison object does not contain pvalue matrices.")
    -log10(pmax(x$pvalue, .Machine$double.eps, na.rm = FALSE))
  })
  matrices <- .ssh_validate_similarity_matrices(matrices)
  names(matrices) <- names(comparison$comparisons)
  if (length(matrices) == 1) {
    out <- list(positive = matrices[[1]])
    attr(out, "comparison_names") <- names(matrices)[1]
    attr(out, "comparison_method") <- comparison_method
    return(out)
  }
  out <- list(positive = matrices[[1]], negative = matrices[[2]])
  attr(out, "comparison_names") <- names(matrices)[seq_along(out)]
  attr(out, "comparison_method") <- comparison_method
  out
}

.ssh_validate_similarity_matrix <- function(x, arg_name) {
  ## Ensure each heatmap input is a numeric matrix with dimnames.
  if (!is.matrix(x) || !is.numeric(x)) {
    stop(arg_name, " must be a numeric matrix.")
  }
  if (is.null(rownames(x)) || is.null(colnames(x))) {
    stop(arg_name, " must have row and column names.")
  }
  x
}

.ssh_validate_similarity_matrices <- function(matrices) {
  ## Validate all level matrices and keep cross-list dimensions aligned.
  matrices <- lapply(seq_along(matrices), function(i) {
    .ssh_validate_similarity_matrix(matrices[[i]], paste0("comparison$comparisons[[", i, "]]"))
  })
  reference_dim <- dim(matrices[[1]])
  reference_dimnames <- dimnames(matrices[[1]])
  for (i in seq_along(matrices)[-1]) {
    if (!identical(dim(matrices[[i]]), reference_dim) ||
        !identical(dimnames(matrices[[i]]), reference_dimnames)) {
      stop("All comparison matrices must have identical dimensions and dimnames.")
    }
  }
  matrices
}

.ssh_similarity_order <- function(similarity, margin = c("row", "column"), method = "ward.D") {
  ## Cluster rows or columns from their similarity profiles.
  margin <- match.arg(margin)
  profile <- if (margin == "row") similarity else t(similarity)
  profile[is.na(profile)] <- 0
  if (nrow(profile) < 2) return(seq_len(nrow(profile)))
  value_range <- range(profile, na.rm = TRUE)
  if (!all(is.finite(value_range)) || diff(value_range) == 0) {
    profile[] <- 0
  } else {
    profile <- (profile - value_range[1]) / diff(value_range)
  }
  d <- stats::dist(profile)
  hc <- stats::hclust(d, method = method)
  if (requireNamespace("cba", quietly = TRUE) &&
      exists("hcopt", envir = asNamespace("cba"), inherits = FALSE)) {
    hcopt <- get("hcopt", envir = asNamespace("cba"), inherits = FALSE)
    hc <- hcopt(d, HC = hc, method = method)
  }
  hc$order
}

.ssh_annotation_args <- function(annotation, annotation_side) {
  ## Map a user-supplied annotation object to the requested heatmap side.
  if (is.null(annotation)) return(list())
  if (!methods::is(annotation, "HeatmapAnnotation")) {
    stop("annotation must be a ComplexHeatmap::HeatmapAnnotation object.")
  }
  if (annotation_side == "column") {
    return(list(top_annotation = annotation))
  }
  list(left_annotation = annotation)
}

.ssh_abbreviate_comparison_names <- function(x) {
  ## Shorten level names for compact heatmap legend titles.
  gsub("level", "lev", x, fixed = TRUE)
}

.ssh_set_legend_title <- function(args, title) {
  ## Respect user legend parameters while filling a default title.
  legend_param <- args$heatmap_legend_param
  if (is.null(legend_param)) legend_param <- list()
  if (is.null(legend_param$title)) legend_param$title <- title
  args$heatmap_legend_param <- legend_param
  args
}

.ssh_measure_legend_title <- function(measure) {
  ## Use a descriptive title when only one color scale is displayed.
  if (measure == "pvalue") return("-log10(pvalue)")
  measure
}

.ssh_legend_at <- function(values) {
  ## Build stable numeric legend ticks from finite matrix values.
  values <- values[is.finite(values)]
  if (!length(values)) return(c(0, 1))
  value_range <- range(values, na.rm = TRUE)
  if (diff(value_range) == 0) return(value_range[1])
  pretty(value_range, n = 5)
}

.ssh_color_legend <- function(values, col_fun, title) {
  ## Create a ComplexHeatmap color legend for custom-drawn cells.
  at <- .ssh_legend_at(values)
  mapped_colors <- try(col_fun(at), silent = TRUE)
  if (inherits(mapped_colors, "try-error") || length(mapped_colors) != length(at)) {
    mapped_colors <- vapply(at, col_fun, character(1))
    return(ComplexHeatmap::Legend(
      labels = at,
      legend_gp = grid::gpar(fill = mapped_colors),
      title = title
    ))
  }
  ComplexHeatmap::Legend(
    at = at,
    col_fun = col_fun,
    title = title
  )
}

.ssh_combined_legends <- function(sim, measure, legend_names, pos_col_fun, neg_col_fun) {
  ## Add one legend for shared scales, or one per triangle-specific scale.
  if (identical(pos_col_fun, neg_col_fun)) {
    return(list(.ssh_color_legend(
      unlist(sim, use.names = FALSE),
      pos_col_fun,
      .ssh_measure_legend_title(measure)
    )))
  }
  list(
    .ssh_color_legend(sim$negative, neg_col_fun, legend_names[2]),
    .ssh_color_legend(sim$positive, pos_col_fun, legend_names[1])
  )
}
