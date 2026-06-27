# Comparing OmicSignature Lists

This vignette outlines the workflow for comparing `OmicSignature`
objects and visualizing similarity among signatures. It is prepared for
later publication with the package website and uses small illustrative
snippets rather than bundled example signature files.

## Compare one list of signatures

Use
[`compare_omic_signatures()`](https://montilab.github.io/OmicSignature/reference/compare_omic_signatures.md)
with a named list of `OmicSignature` objects to compare every signature
against every other signature in the list. For bi-directional
signatures, the function compares the first factor level against the
first factor level and the second factor level against the second factor
level.

``` r

#library(OmicSignature)
devtools::load_all()

data(compare_signatures_example)
signature_list <- compare_signatures_example

overlap_res <- compare_omic_signatures(
  sig_list1 = signature_list,
  method = "overlap",
  score_cutoff = log2(1.25),
  adj_p_cutoff = 0.05,
  min_features = 25,
  max_feature = 500
)

names(overlap_res$comparisons)
overlap_res$comparisons$level1_vs_level1$jaccard
overlap_res$comparisons$level1_vs_level1$counts
```

The overlap method returns three matrices for each compared factor
level:

- `jaccard`: Jaccard similarity between retained feature sets.
- `pvalue`: Fisher exact test p-values for overlap enrichment.
- `counts`: overlap size, first signature size, and second signature
  size, formatted as `overlap | n1 | n2`.

## Compare two lists of signatures

Pass both `sig_list1` and `sig_list2` to compare signatures across
collections. This is useful when signatures come from different studies,
cohorts, platforms, or perturbation screens.

``` r

reference_signatures <- compare_signatures_example[1:2]
query_signatures <- compare_signatures_example[3:4]

cross_res <- compare_omic_signatures(
  sig_list1 = query_signatures,
  sig_list2 = reference_signatures,
  method = "overlap",
  score_cutoff = log2(1.25),
  adj_p_cutoff = 0.05,
  min_features = 25,
  max_feature = 500
)

cross_res$comparisons$level1_vs_level1$jaccard
```

When `background` is not supplied, the feature universe is inferred from
all available signature and differential-expression tables in both
inputs. Supplying a measured or assay-specific background is recommended
when it is available.

## Control label pairing

By default, level ordering follows the factor levels in each signature’s
`group_label` column. Use `label_pairing` and `label_pairing2` when
signatures need explicit matching.

``` r

data(compare_label_pairing_example)

paired_res <- compare_omic_signatures(
  sig_list1 = compare_label_pairing_example,
  method = "overlap",
  label_pairing = list(
    signature_a = c("treated", "control"),
    signature_b = c("up", "down"),
    signature_c = c("resistant", "sensitive")
  )
)

paired_res$comparisons$level1_vs_level1$jaccard
paired_res$comparisons$level2_vs_level2$jaccard
```

## KS and GSEA-style comparisons

The `ks` and `gsea` methods compare a retained feature set from one
signature against ranked differential-expression scores from another
signature. These methods require the compared `OmicSignature` objects to
retain their `difexp` tables. For each requested `group_label`, genes
are ranked from the largest `-log10(p_value)` in that label to the
largest `-log10(p_value)` in the contrasting label.

``` r

ks_res <- compare_omic_signatures(
  sig_list1 = signature_list,
  method = "ks",
  adj_p_cutoff = 0.05,
  min_features = 25,
  max_feature = 500
)

ks_res$comparisons$level1_vs_level1$score
ks_res$comparisons$level1_vs_level1$pvalue
```

GSEA requires the optional `fgsea` package.

``` r

gsea_res <- compare_omic_signatures(
  sig_list1 = signature_list,
  method = "gsea",
  adj_p_cutoff = 0.05,
  min_features = 25,
  max_feature = 500,
  minSize = 10,
  maxSize = 500
)
```

## Plot similarity heatmaps

Use
[`signature_similarity_heatmap()`](https://montilab.github.io/OmicSignature/reference/signature_similarity_heatmap.md)
to visualize square self-comparison output from
`compare_omic_signatures(method = "overlap")`. The heatmap function
requires the optional `ComplexHeatmap` and `circlize` packages.

``` r

signature_similarity_heatmap(
  overlap_res,
  measure = "jaccard",
  mode = "separate",
  triangle = "upper"
)

signature_similarity_heatmap(
  overlap_res,
  measure = "pvalue",
  mode = "combined",
  triangle = "upper"
)
```

For `measure = "pvalue"`, values are plotted as `-log10(pvalue)`. Larger
values therefore indicate stronger overlap enrichment.

Rank-based KS and GSEA outputs can be visualized with
`measure = "score"` or `measure = "pvalue"`. Because these matrices are
directional rather than symmetric, `mode = "combined"` draws only the
upper triangle and splits each cell between the two label levels.
`mode = "separate"` draws one full heatmap for each label level.

``` r

signature_similarity_heatmap(
  ks_res,
  measure = "score",
  mode = "combined",
  triangle = "upper"
)

signature_similarity_heatmap(
  ks_res,
  measure = "pvalue",
  mode = "separate"
)
```
