# Plot signature similarity heatmaps

Plot one or more ComplexHeatmap heatmaps from the object returned by
\`compare_omic_signatures()\`. Overlap comparisons can show Jaccard
similarity or p-value-based similarity on a \`-log10(p-value)\` scale.
Rank-based comparisons can show score or \`-log10(p-value)\` matrices.
For rank-based KS and GSEA comparisons, \`mode = "combined"\` draws the
upper triangle with split cells: the top-right triangle shows
\`level2_vs_level2\` and the bottom-left triangle shows
\`level1_vs_level1\`. \`mode = "separate"\` draws one full heatmap per
level, and \`mode = "split"\` is invalid.

## Usage

``` r
signature_similarity_heatmap(
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
  ...
)
```

## Arguments

- comparison:

  Output from \`compare_omic_signatures()\`.

- measure:

  One of \`"jaccard"\`, \`"score"\`, or \`"pvalue"\`.

- mode:

  One of \`"separate"\`, \`"combined"\`, or \`"split"\`.

- annotation:

  Optional ComplexHeatmap::HeatmapAnnotation object.

- annotation_side:

  Where to place annotation: \`"column"\` or \`"row"\`.

- triangle:

  Triangle to display for symmetric overlap heatmaps in \`"separate"\`
  and \`"combined"\` modes.

- cluster_method:

  Hierarchical clustering method passed to hclust or cba.

- col_fun:

  Color function for separate heatmaps.

- pos_col_fun:

  Color function for first-level triangles in combined mode.

- neg_col_fun:

  Color function for second-level triangles in combined mode.

- combined_triangle_threshold:

  Maximum matrix size for split triangles inside each cell in combined
  mode.

- draw:

  Logical; draw the heatmap before returning it.

- ...:

  Additional arguments passed to ComplexHeatmap::Heatmap().

## Value

A ComplexHeatmap Heatmap or HeatmapList object, invisibly if \`draw =
TRUE\`.

## Examples

``` r
data(compare_signatures_example)

overlap_res <- compare_omic_signatures(
  compare_signatures_example[1:2],
  method = "overlap",
  score_cutoff = log2(1.025),
  adj_p_cutoff = 0.01,
  min_features = 10
)

if (requireNamespace("ComplexHeatmap", quietly = TRUE) &&
    requireNamespace("circlize", quietly = TRUE)) {
  signature_similarity_heatmap(overlap_res, draw = FALSE)
}


ks_res <- compare_omic_signatures(
  compare_signatures_example[1:2],
  method = "ks",
  adj_p_cutoff = 0.01,
  min_features = 10
)

if (requireNamespace("ComplexHeatmap", quietly = TRUE) &&
    requireNamespace("circlize", quietly = TRUE)) {
  signature_similarity_heatmap(
    ks_res,
    measure = "score",
    mode = "combined",
    draw = FALSE
  )
}
```
