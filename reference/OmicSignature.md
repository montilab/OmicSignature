# Create an OmicSignature object

Create an OmicSignature object

## Usage

``` r
OmicSignature(metadata, signature, difexp = NULL, print_message = FALSE)
```

## Arguments

- metadata:

  Required list. See \`createMetadata()\` for details.

- signature:

  Required vector or data frame. Data frames must include \`probe_id\`,
  \`feature_name\`, and, for non-uni-directional signatures,
  \`group_label\`. \`score\` is optional.

- difexp:

  Optional differential expression result data frame.

- print_message:

  Use \`TRUE\` to print validation messages.

## Value

An \`OmicSignature\` S4 object.
