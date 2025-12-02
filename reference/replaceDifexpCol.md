# Standardize the column names of difexp

A helper function that rename some of the commonly used columns of a
differential expression dataframe or matrix. (updated 10/2024)

## Usage

``` r
replaceDifexpCol(x)
```

## Arguments

- x:

  A single character vector specifying the column names of the
  differential expression dataframe or matrix. Alternatively, the
  differential expression dataframe or matrix itself. If the latter, its
  column name will be modified and the matrix will be returned.

## Value

standardized column names or standardized matrix
