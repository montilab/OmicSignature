# create an OmicSignature object from differential expression matrix

updated 08/2025

## Usage

``` r
OmicSigFromDifexp(difexp, metadata, criteria = NULL)
```

## Arguments

- difexp:

  Differential expression matrix

- metadata:

  Metadata for the OmicSignature object. If \`criteria\` is \`NULL\`,
  the criterias to extract signatures will need to be provided in
  metadata. They can be specified in metadata fields as one or more of
  the followings: \`logfc_cutoff\`, \`score_cutoff\`, \`adj_p_cutoff\`,
  \`p_value_cutoff\`.

- criteria:

  A character string of R expressions used to extract signatures from
  difexp, e.g. "logfc \> 5; score \> 10", evaluated as R code (via
  \`rlang::parse_exprs()\`) against \`difexp\` - \`criteria\` must only
  ever come from a trusted source, not from untrusted/external input.
  Alternatively, they can be provided in metadata fields:
  list("logfc_cutoff" = 5, "score_cutoff" = 10). At least one of
  \`criteria\` or a metadata cutoff field is required; without any,
  every row of \`difexp\` would silently become the signature.

## Value

OmicSignature object
