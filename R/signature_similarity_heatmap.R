#######################################################################
## function: SIGNATURE SIMILARITY HEATMAP
#######################################################################

#' Plot signature similarity heatmaps
#'
#' Plot one or more ComplexHeatmap heatmaps from the object returned by
#' `compare_omics_signatures()`. Similarity can be shown as Jaccard similarity
#' or as p-value-based similarity on a `-log10(p-value)` scale.
#'
#' @param comparison Output from `compare_omics_signatures(method = "overlap")`.
#' @param measure One of `"jaccard"` or `"pvalue"`.
#' @param mode One of `"separate"`, `"combined"`, or `"split"`.
#' @param annotation Optional ComplexHeatmap::HeatmapAnnotation object.
#' @param annotation_side Where to place annotation: `"column"` or `"row"`.
#' @param triangle Triangle to display for symmetric heatmaps in `"separate"`
#'   and `"combined"` modes.
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
#' overlap_res <- compare_omics_signatures(
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
#' @export
signature_similarity_heatmap <- function(
    comparison,
    measure = c("jaccard", "pvalue"),
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
  if (mode %in% c("combined", "split") && is.null(sim$negative)) {
    stop(mode, " mode requires two similarity matrices.")
  }

  ## Choose default color scales from the observed similarity range.
  max_value <- max(unlist(sim, use.names = FALSE), na.rm = TRUE)
  if (!is.finite(max_value) || max_value <= 0) max_value <- 1
  if (is.null(col_fun)) {
    col_fun <- circlize::colorRamp2(c(0, max_value), c("white", "#b2182b"))
  }
  if (is.null(pos_col_fun)) {
    pos_col_fun <- circlize::colorRamp2(c(0, max_value), c("white", "#b2182b"))
  }
  if (is.null(neg_col_fun)) {
    neg_col_fun <- circlize::colorRamp2(c(0, max_value), c("white", "#2166ac"))
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

  ## Use one shared ordering so separate positive/negative maps align.
  order_basis <- if (is.null(sim$negative)) sim$positive else (sim$positive + sim$negative) / 2
  ord <- .ssh_similarity_order(order_basis, method = cluster_method)
  feature_order <- rownames(order_basis)[ord]

  mask_redundant_triangle <- function(mat) {
    ## Hide the redundant half of symmetric self-comparison matrices.
    row_pos <- match(rownames(mat), feature_order)
    col_pos <- match(colnames(mat), feature_order)
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
    row_pos <- match(rownames(order_basis)[i], feature_order)
    col_pos <- match(colnames(order_basis)[j], feature_order)
    if (row_pos == col_pos) return(TRUE)
    if (triangle == "upper") return(row_pos < col_pos)
    row_pos > col_pos
  }

  annotation_args <- .ssh_annotation_args(annotation, annotation_side)
  common_args <- c(
    list(
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      row_order = feature_order,
      column_order = feature_order
    ),
    annotation_args,
    heatmap_args
  )

  if (mode == "separate") {
    ## Draw one heatmap per comparison level.
    positive_args <- common_args
    positive_args$column_title <- sim_names[1]
    positive_ht <- do.call(
      ComplexHeatmap::Heatmap,
      c(
        list(
          matrix = mask_redundant_triangle(sim$positive),
          name = sim_names[1],
          col = col_fun,
          na_col = "#FFFFFF00"
        ),
        positive_args
      )
    )
    ht <- positive_ht
    if (!is.null(sim$negative)) {
      negative_args <- common_args
      if (annotation_side == "column") negative_args$top_annotation <- NULL
      if (annotation_side == "row") negative_args$left_annotation <- NULL
      negative_args$column_title <- sim_names[2]
      negative_ht <- do.call(
        ComplexHeatmap::Heatmap,
        c(
          list(
            matrix = mask_redundant_triangle(sim$negative),
            name = sim_names[2],
            col = col_fun,
            na_col = "#FFFFFF00"
          ),
          negative_args
        )
      )
      ht <- positive_ht + negative_ht
    }
  } else if (mode == "split") {
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
              pos_value <- sim$positive[i, j]
              neg_value <- sim$negative[i, j]
              grid::grid.polygon(
                x = grid::unit.c(x - width / 2, x + width / 2, x + width / 2),
                y = grid::unit.c(y + height / 2, y + height / 2, y - height / 2),
                gp = grid::gpar(col = NA, fill = pos_col_fun(pos_value))
              )
              grid::grid.polygon(
                x = grid::unit.c(x - width / 2, x - width / 2, x + width / 2),
                y = grid::unit.c(y + height / 2, y - height / 2, y - height / 2),
                gp = grid::gpar(col = NA, fill = neg_col_fun(neg_value))
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

  if (draw) {
    return(invisible(ComplexHeatmap::draw(ht)))
  }
  ht
}

.ssh_similarity_from_comparison <- function(comparison, measure) {
  ## Extract Jaccard or transformed p-value matrices from comparison output.
  if (!is.list(comparison) || is.null(comparison$comparisons)) {
    stop("comparison must be an object returned by compare_omics_signatures().")
  }
  matrices <- lapply(comparison$comparisons, function(x) {
    if (measure == "jaccard") {
      if (is.null(x$jaccard)) stop("Comparison object does not contain jaccard matrices.")
      return(x$jaccard)
    }
    if (is.null(x$pvalue)) stop("Comparison object does not contain pvalue matrices.")
    -log10(pmax(x$pvalue, .Machine$double.eps, na.rm = FALSE))
  })
  matrices <- lapply(seq_along(matrices), function(i) {
    .ssh_validate_similarity_matrix(matrices[[i]], paste0("comparison$comparisons[[", i, "]]"))
  })
  names(matrices) <- names(comparison$comparisons)
  if (length(matrices) == 1) {
    out <- list(positive = matrices[[1]])
    attr(out, "comparison_names") <- names(matrices)[1]
    return(out)
  }
  out <- list(positive = matrices[[1]], negative = matrices[[2]])
  attr(out, "comparison_names") <- names(matrices)[seq_along(out)]
  out
}

.ssh_validate_similarity_matrix <- function(x, arg_name) {
  ## Ensure heatmap inputs are square matrices with matching names.
  if (!is.matrix(x) || !is.numeric(x)) {
    stop(arg_name, " must be a numeric matrix.")
  }
  if (nrow(x) != ncol(x)) {
    stop(arg_name, " must be a square matrix.")
  }
  if (is.null(rownames(x)) || is.null(colnames(x))) {
    stop(arg_name, " must have row and column names.")
  }
  if (!identical(rownames(x), colnames(x))) {
    stop(arg_name, " must have identical row and column names in the same order.")
  }
  x
}

.ssh_similarity_order <- function(similarity, method = "ward.D") {
  ## Cluster on normalized distance derived from similarity values.
  similarity[is.na(similarity)] <- 0
  if (nrow(similarity) < 2) return(seq_len(nrow(similarity)))
  max_value <- max(similarity, na.rm = TRUE)
  if (!is.finite(max_value) || max_value <= 0) {
    similarity[] <- 0
  } else {
    similarity <- pmax(pmin(similarity / max_value, 1), 0)
  }
  d <- stats::as.dist(1 - similarity)
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
