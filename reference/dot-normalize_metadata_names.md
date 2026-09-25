# Normalize deprecated metadata field names

Accepts a metadata list that may use the pre-1.4.0 field name
\`direction_type\` and returns the same list using the current name
\`type\`. Called from \`OmicSignature\`'s private \`checkMetadata()\`,
which every construction and assignment path routes through, so callers
do not need to normalize themselves.

## Usage

``` r
.normalize_metadata_names(metadata)
```

## Arguments

- metadata:

  a metadata list. Inputs that are not named lists are returned
  unchanged, leaving their validation to the caller.

## Value

the metadata list using \`type\`.
