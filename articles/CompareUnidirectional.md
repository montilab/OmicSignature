# Comparing Uni-directional Signatures

This vignette demonstrates
[`compare_omic_signatures()`](https://montilab.github.io/OmicSignature/reference/compare_omic_signatures.md)
when every signature being compared is uni-directional: a single feature
set, with no `group_label` contrast at all. See
[`vignette("CompareSignatures")`](https://montilab.github.io/OmicSignature/articles/CompareSignatures.md)
for the general bi-directional workflow, and
[`vignette("CompareMixedDirection")`](https://montilab.github.io/OmicSignature/articles/CompareMixedDirection.md)
for a list that mixes uni- and bi-directional signatures.

## The example data

`compare_unidirectional_example` contains three simulated
uni-directional signatures sharing a 100-gene universe. `uni_x` and
`uni_y` share 5 of their 10 features each; `uni_z` shares nothing with
either.

``` r

library(OmicSignature)

data(compare_unidirectional_example)

## direction_type per signature
sapply(compare_unidirectional_example, \(x) x$metadata$direction_type)
#>             uni_x             uni_y             uni_z 
#> "uni-directional" "uni-directional" "uni-directional"
```

## Overlap: a flat comparison, with no level structure

Bi-directional signatures are compared level by level
(`level1_vs_level1`, `level2_vs_level2`, …), since `group_label` gives
each one two feature sets to pair up. Uni-directional signatures have no
such pairing to make, so when *every* signature in both `sig_list1` and
`sig_list2` is uni-directional,
[`compare_omic_signatures()`](https://montilab.github.io/OmicSignature/reference/compare_omic_signatures.md)
returns one flat comparison instead: `comparisons` directly holds
`jaccard`, `pvalue`, and `counts`, and `label_order` is `NULL`.

``` r

uni_res <- compare_omic_signatures(
  sig_list1 = compare_unidirectional_example,
  method = "overlap",
  min_features = 3
)

names(uni_res$comparisons)
#> [1] "jaccard" "pvalue"  "counts"
uni_res$label_order
#> NULL

uni_res$comparisons$jaccard
#>           uni_x     uni_y uni_z
#> uni_x 1.0000000 0.3333333     0
#> uni_y 0.3333333 1.0000000     0
#> uni_z 0.0000000 0.0000000     1
uni_res$comparisons$counts
#>       uni_x uni_y uni_z size
#> uni_x    10     5     0   10
#> uni_y     5    10     0   10
#> uni_z     0     0    10   10
#> size     10    10    10   NA
```

## KS and GSEA are not possible without a ranking

`ks_rank`, `ks_score`, and `gsea` need a two-group contrast to build a
ranked vector to test against, which no uni-directional signature can
provide. If none of `sig_list2` is bi-directional with a `difexp` table,
[`compare_omic_signatures()`](https://montilab.github.io/OmicSignature/reference/compare_omic_signatures.md)
cannot construct a ranking side at all, and errors accordingly:

``` r

compare_omic_signatures(compare_unidirectional_example, method = "ks_rank")
#> Error in `.cos_check_ranking_capable()`:
#> ! No signatures in sig_list2 are bi-directional with a difexp table, required as the ranking side for method = 'ks_rank'.
```
