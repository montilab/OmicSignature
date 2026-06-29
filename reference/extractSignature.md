# Extract a new signature

Extract a new signature

## Usage

``` r
extractSignature(x, conditions, bind = TRUE)

# S4 method for class 'OmicSignature'
extractSignature(x, conditions, bind = TRUE)

# S4 method for class 'OmicSignatureCollection'
extractSignature(x, conditions, bind = TRUE)
```

## Arguments

- x:

  An \`OmicSignature\` or \`OmicSignatureCollection\` object.

- conditions:

  Conditions used to filter differential expression results.

- bind:

  For collections, return a single bound data frame when \`TRUE\`;
  otherwise return a list.

## Value

A data frame, or a list of data frames for unbound collections.
