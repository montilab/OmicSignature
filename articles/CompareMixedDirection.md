# Comparing Mixed Uni- and Bi-directional Signatures

This vignette demonstrates how
[`compare_omic_signatures()`](https://montilab.github.io/OmicSignature/reference/compare_omic_signatures.md)
handles a signature list that mixes bi-directional signatures (a
`group_label` factor with two levels, e.g. treated vs. control) with
uni-directional signatures (a single feature set, with no `group_label`
contrast at all). See
[`vignette("CompareSignatures")`](https://montilab.github.io/OmicSignature/articles/CompareSignatures.md)
for the general overlap/KS/GSEA workflow, and for how `label_pairing`
resolves mismatched `group_label` level order across bi-directional
signatures.

## The example data

`compare_mixed_direction_example` extends
`compare_label_pairing_example` (three simulated bi-directional
signatures, `signature_a`, `signature_b`, and `signature_c`) with one
additional uni-directional signature, `signature_d`. `signature_d`’s
features overlap strongly with `signature_a`’s `treated` label,
`signature_b`’s `up` label, and `signature_c`’s `resistant` label, and
not at all with any signature’s `control`/`down`/`sensitive` label.

``` r

library(OmicSignature)

data(compare_mixed_direction_example)

## direction_type per signature
sapply(compare_mixed_direction_example, \(x) x$metadata$direction_type)
#>       signature_a       signature_b       signature_c       signature_d 
#>  "bi-directional"  "bi-directional"  "bi-directional" "uni-directional"
```

## Overlap: a uni-directional signature is compared against both levels

Because `signature_d` has no `group_label` contrast,
[`compare_omic_signatures()`](https://montilab.github.io/OmicSignature/reference/compare_omic_signatures.md)
treats its whole feature set as a single set and compares it against
*both* levels of every bi-directional signature it’s compared to: the
same feature set appears, unchanged, in both the `level1_vs_level1` and
`level2_vs_level2` comparison matrices.

As in `compare_label_pairing_example`, `signature_b`’s `group_label`
levels are stored in a different order than `signature_a`’s and
`signature_c`’s, so `label_pairing` is used here too, to align matching
conditions:

``` r

mixed_res <- compare_omic_signatures(
  sig_list1 = compare_mixed_direction_example,
  method = "overlap",
  label_pairing = list(
    signature_a = c("treated", "control"),
    signature_b = c("up", "down"),
    signature_c = c("resistant", "sensitive")
  ),
  min_features = 3
)

## signature_d has no group_label levels, so label_pairing does not apply to
## it; its row is NA/NA in label_order, and it is compared unchanged against
## whichever level is being evaluated in each pass.
mixed_res$label_order
#> $sig_list1
#>             level1      level2     
#> signature_a "treated"   "control"  
#> signature_b "up"        "down"     
#> signature_c "resistant" "sensitive"
#> signature_d NA          NA

mixed_res$comparisons$level1_vs_level1$jaccard["signature_d", ]
#> signature_a signature_b signature_c signature_d 
#>   0.6000000   0.6000000   0.4545455   1.0000000
mixed_res$comparisons$level2_vs_level2$jaccard["signature_d", ]
#> signature_a signature_b signature_c signature_d 
#>           0           0           0           1
```

`signature_d` overlaps strongly (jaccard 0.45-0.6) with the
`treated`/`up`/`resistant` level (`level1`, after pairing) of each
bi-directional signature, and shares nothing (jaccard 0) with the
`control`/`down`/`sensitive` level (`level2`).

## KS and GSEA: a uni-directional signature can only be a geneset

`ks_rank`, `ks_score`, and `gsea` rank features using the two-group
contrast in a signature’s `difexp` table, so a uni-directional signature
can never serve as the ranking side (`sig_list2`), only as a geneset
(`sig_list1`).
[`compare_omic_signatures()`](https://montilab.github.io/OmicSignature/reference/compare_omic_signatures.md)
excludes it from the ranking side with a warning, and its column in the
result matrices is entirely `NA`, while it remains fully usable as a
geneset row.

``` r

ks_res <- compare_omic_signatures(
  sig_list1 = compare_mixed_direction_example,
  method = "ks_rank",
  label_pairing = list(
    signature_a = c("treated", "control"),
    signature_b = c("up", "down"),
    signature_c = c("resistant", "sensitive")
  ),
  min_features = 3
)
#> Warning in .cos_check_ranking_capable(sig_list2, method): Excluding
#> signature(s) from the ranking side (sig_list2) for method = 'ks_rank' because
#> they are uni-directional or have no difexp table: signature_d. They remain
#> available as the geneset side (sig_list1).

## signature_d as a ranking (column) is impossible: entirely NA.
ks_res$comparisons$level1_vs_level1$score[, "signature_d"]
#> signature_a signature_b signature_c signature_d 
#>          NA          NA          NA          NA

## signature_d as a geneset (row) against the other signatures' rankings works.
ks_res$comparisons$level1_vs_level1$score["signature_d", ]
#> signature_a signature_b signature_c signature_d 
#>   0.9400000   0.9400000   0.7833333          NA
```
