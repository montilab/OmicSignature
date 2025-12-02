# change a signature vector into a dataframe to be saved into OmicSignature object

updated 08/2025

## Usage

``` r
signatureVecToDF(input, group_labels = NULL)
```

## Arguments

- input:

  a character vector of the significant feature names, or a numeric
  vector of scores and named by feature names.

- group_labels:

  optional. a character vector of length of 2. when group_labels =
  c("Group1", "Group2"), it indicates the analysis is Group1 vs Group2,
  and positive scores indicate a higher value in Group1.

## Value

signature dataframe with columns "feature_name", along with "score" and
"group_label" if applicable.

## Examples

``` r
signatures <- c("gene1", "gene2", "gene3")
signatureVecToDF(signatures)
#>   probe_id feature_name
#> 1        1        gene1
#> 2        2        gene2
#> 3        3        gene3

signatures <- c(0.45, -3.21, 2.44)
names(signatures) <- c("gene1", "gene2", "gene3")
signatureVecToDF(signatures)
#>   probe_id feature_name score
#> 1        1        gene1  0.45
#> 2        2        gene2 -3.21
#> 3        3        gene3  2.44
signatureVecToDF(signatures, group_labels = c("Group1", "Group2"))
#>   probe_id feature_name score group_label
#> 1        1        gene1  0.45      Group1
#> 2        2        gene2 -3.21      Group2
#> 3        3        gene3  2.44      Group1
```
