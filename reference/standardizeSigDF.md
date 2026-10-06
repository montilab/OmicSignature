# Standardize signature data frame

remove rows with an empty feature name, coerce score to numeric and
order by absolute score. Rows whose score is missing or cannot be parsed
are kept (with an NA score) and sorted last, so a uni-directional gene
list without scores keeps all its features. updated 10/2026.

## Usage

``` r
standardizeSigDF(sigdf)
```

## Arguments

- sigdf:

  signature dataframe

## Value

signature dataframe with empty feature names removed and ordered by
absolute score, NA scores last
