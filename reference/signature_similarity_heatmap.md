# Plot signature similarity heatmaps

Plot one or more ComplexHeatmap heatmaps from the object returned by
\`compare_omics_signatures()\`. Similarity can be shown as Jaccard
similarity or as p-value-based similarity on a \`-log10(p-value)\`
scale.

## Usage

``` r
signature_similarity_heatmap(
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
  ...
)
```

## Arguments

- comparison:

  Output from \`compare_omics_signatures(method = "overlap")\`.

- measure:

  One of \`"jaccard"\` or \`"pvalue"\`.

- mode:

  One of \`"separate"\`, \`"combined"\`, or \`"split"\`.

- annotation:

  Optional ComplexHeatmap::HeatmapAnnotation object.

- annotation_side:

  Where to place annotation: \`"column"\` or \`"row"\`.

- triangle:

  Triangle to display for symmetric heatmaps in \`"separate"\` and
  \`"combined"\` modes.

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
