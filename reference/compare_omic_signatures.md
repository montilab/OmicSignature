# Compare OmicSignature objects

Compare signatures within one list, or between two lists, using set
overlap, rank-position KS, score-distribution KS, or fgsea.

## Usage

``` r
compare_omic_signatures(
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
  ...
)
```

## Arguments

- sig_list1:

  List of OmicSignature objects or an OmicSignatureCollection. Names
  must be unique within the list.

- sig_list2:

  Optional second list of OmicSignature objects or collection. Names
  must be unique within the list and must not overlap with `sig_list1`'s
  names, so that a cross-list comparison can never be mistaken for a
  self-comparison. For `method = "ks_rank"`, `method = "ks_score"`, and
  `method = "gsea"`, `sig_list2` is the ranking side and each element
  needs to be bi-directional with a difexp table; elements that aren't
  (including uni-directional signatures, which have no group_label
  contrast to rank from) are excluded from `sig_list2` (with a warning)
  but remain usable as `sig_list1` genesets, which never require difexp
  or a group_label contrast. If no element of `sig_list2` can rank, this
  errors instead.

- method:

  Comparison method.

- background:

  Optional background feature vector for overlap tests.

- score_cutoff:

  Minimum absolute score to include in a signature. For signatures
  without a difexp table, this can only be applied if the signature
  table itself has a `score_col` column; otherwise a warning is issued
  and the cutoff is skipped.

- adj_p_cutoff:

  Maximum adjusted p-value to include in a signature. For signatures
  without a difexp table, this can only be applied if the signature
  table itself has an `adj_p_col` column; otherwise a warning is issued
  and the cutoff is skipped.

- min_features:

  Minimum number of features retained per label-specific signature. Must
  be at least 3. For `method = "overlap"`, any signature/label that
  cannot reach `min_features` retained features (even after backfilling)
  is dropped from the comparison with a warning, rather than being
  scored against a near-empty or empty feature set.

- max_feature:

  Maximum number of features retained per label-specific signature.

- label_pairing:

  Optional named list giving the two group-label levels to use for each
  signature in \`sig_list1\`. Signatures not named in \`label_pairing\`
  fall back to their own \`group_label\` factor-level order; if two or
  more of those signatures disagree on that order (e.g. one signature's
  factor levels are \`c("control", "treated")\` and another's are
  \`c("treated", "control")\`), a warning is issued, since
  \`level1_vs_level1\`/ \`level2_vs_level2\` pairing is purely
  positional and may then compare mismatched conditions across
  signatures.

- label_pairing2:

  Optional named list giving the two group-label levels to use for each
  signature in \`sig_list2\`. Same fallback and warning behavior as
  \`label_pairing\`.

- feature_col:

  Column containing feature identifiers.

- score_col:

  Column containing scores in signature and difexp tables.

- adj_p_col:

  Column containing adjusted p-values in difexp tables.

- p_value_col:

  Column containing p-values used to rank difexp tables for KS and GSEA
  comparisons.

- group_col:

  Column containing phenotype group labels.

- adjust:

  Logical; adjust p-values within each returned comparison.

- p_adjust_method:

  Multiple-testing correction method.

- alternative:

  Alternative hypothesis for Fisher and KS tests. For KS, `"greater"`
  tests whether the feature set is enriched at the top of the signed
  ranking, and `"less"` tests enrichment at the bottom.

- gsea_score:

  Column from fgsea output to return as the score matrix.

- minSize:

  Minimum pathway size passed to fgsea.

- maxSize:

  Maximum pathway size passed to fgsea.

- nproc:

  Number of fgsea workers.

- ...:

  Additional arguments passed to fgsea.

## Details

\`method = "ks"\` is a backwards-compatible alias for \`method =
"ks_rank"\`. For \`method = "ks_rank"\`, \`method = "ks_score"\`, and
\`method = "gsea"\`, each ranked vector is built from both phenotype
labels. Features in the requested \`group_col\` label are ranked by
positive \`-log10(p_value)\`, and features in the contrasting label are
ranked by negative \`-log10(p_value)\`. This places the most significant
selected-label features at the top of the ranking and the most
significant contrast-label features at the bottom. \`ks_rank\` compares
the positions of each retained feature set within this ranked vector.
\`ks_score\` compares the numeric ranking scores for retained features
against the remaining features with a two-sample KS test.

Uni-directional signatures (no \`group_label\` contrast) are compared as
a single, whole feature set rather than split by level. If every
signature in both \`sig_list1\` and \`sig_list2\` is uni-directional,
\`method = "overlap"\` returns one flat comparison instead of the
\`level1_vs_level1\`/\`level2_vs_level2\` structure (see the Value
section below). If the signatures being compared are a mix of uni- and
bi-directional, each uni-directional signature's single feature set is
compared against both levels of every bi-directional signature. For
\`method = "ks_rank"\`, \`method = "ks_score"\`, and \`method =
"gsea"\`, a uni-directional signature can only ever be a geneset
(\`sig_list1\`), never a ranking (\`sig_list2\`), since ranking requires
a two-group contrast; if \`sig_list2\` contains no bi-directional
signature with a difexp table (e.g. it is entirely uni-directional), the
comparison is not possible and this errors.

## Value

A list with one element per label pairing, except when every signature
in both \`sig_list1\` and \`sig_list2\` is uni-directional and \`method
= "overlap"\`, in which case \`comparisons\` directly contains
\`jaccard\`, \`pvalue\`, and \`counts\` (no \`level1_vs_level1\`
nesting, and \`label_order\` is \`NULL\`). Otherwise, for \`method =
"overlap"\` each element contains \`jaccard\`, \`pvalue\`, and
\`counts\` matrices. \`counts\` entries are formatted as \`"ov \| n1 \|
n2"\`. For \`method = "ks_rank"\`, \`method = "ks_score"\`, \`method =
"ks"\`, and \`method = "gsea"\` each element contains \`score\` and
\`pvalue\` matrices; columns for \`sig_list2\` signatures without a
difexp table, or that are uni-directional, are entirely \`NA\`, since
those signatures cannot serve as the ranking side.

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
overlap_res$comparisons$level1_vs_level1$jaccard
#>             e7386_hsc3 e7386_cal27
#> e7386_hsc3   1.0000000   0.1271586
#> e7386_cal27  0.1271586   1.0000000

data(compare_label_pairing_example)

toy_ks_score <- compare_omic_signatures(
  compare_label_pairing_example,
  method = "ks_score",
  label_pairing = list(
    signature_a = c("treated", "control"),
    signature_b = c("up", "down"),
    signature_c = c("resistant", "sensitive")
  ),
  min_features = 5
)
round(toy_ks_score$comparisons$level1_vs_level1$score, 3)
#>             signature_a signature_b signature_c
#> signature_a         1.0         0.8       0.556
#> signature_b         0.8         1.0       0.556
#> signature_c         0.5         0.5       1.000
```
