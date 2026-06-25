# Compare OmicSignature objects

Compare signatures within one list, or between two lists, using set
overlap, a two-sample Kolmogorov-Smirnov test, or fgsea.

## Usage

``` r
compare_omics_signatures(
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

  List of OmicSignature objects or an OmicSignatureCollection.

- sig_list2:

  Optional second list of OmicSignature objects or collection.

- method:

  Comparison method.

- background:

  Optional background feature vector for overlap tests.

- score_cutoff:

  Minimum absolute score to include in a signature.

- adj_p_cutoff:

  Maximum adjusted p-value to include in a signature.

- min_features:

  Minimum number of features retained per label-specific signature.

- max_feature:

  Maximum number of features retained per label-specific signature.

- label_pairing:

  Optional named list giving the two group-label levels to use for each
  signature in \`sig_list1\`.

- label_pairing2:

  Optional named list giving the two group-label levels to use for each
  signature in \`sig_list2\`.

- feature_col:

  Column containing feature identifiers.

- score_col:

  Column containing scores in signature and difexp tables.

- adj_p_col:

  Column containing adjusted p-values in difexp tables.

- group_col:

  Column containing phenotype group labels.

- adjust:

  Logical; adjust p-values within each returned comparison.

- p_adjust_method:

  Multiple-testing correction method.

- alternative:

  Alternative hypothesis for Fisher and KS tests.

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

## Value

A list with one element per label pairing. For \`method = "overlap"\`
each element contains \`jaccard\`, \`pvalue\`, and \`counts\` matrices.
\`counts\` entries are formatted as \`"ov \| n1 \| n2"\`. For \`method =
"ks"\` and \`method = "gsea"\` each element contains \`score\` and
\`pvalue\` matrices.
