# search for a valid BRENDA ontology term to use for sample type (deprecated 10/2025)

Using BRENDA ontology version 2021.

## Usage

``` r
BRENDAExistName(x, file = BRENDA)
```

## Arguments

- x:

  A name to search for

- file:

  The BRENDA data frame, has columns ID and Name

## Value

TRUE or FALSE

## Examples

``` r
BRENDAExistName("blood plasma")
#> [1] TRUE
BRENDAExistName("random tissue")
#> [1] FALSE
```
