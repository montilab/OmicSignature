# OmicSignature S4 class

Stores metadata, a signature data frame, and an optional differential
expression analysis result data frame.

## Usage

``` r
# S4 method for class 'OmicSignature'
show(object)
```

## Arguments

- object:

  An object to display.

## Slots

- `metadata`:

  A list describing the signature.

- `signature`:

  A data frame containing \`probe_id\`, \`feature_name\`, optional
  \`score\`, and optional \`group_label\`.

- `difexp`:

  A differential expression result data frame, or \`NULL\`.
