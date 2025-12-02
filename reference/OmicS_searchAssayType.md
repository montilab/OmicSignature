# search for an assay type to use (deprecated 10/2025)

search for an assay type to use (deprecated 10/2025)

## Usage

``` r
OmicS_searchAssayType(x = "")
```

## Arguments

- x:

  a string to search for. if empty, will return all available assay
  types.

## Value

search result

## Examples

``` r
OmicS_searchAssayType()
#> [1] "transcriptomics"    "proteomics"         "metabolomics"      
#> [4] "methylomics"        "genetic_variations" "DNA_binding_sites" 
#> [7] "other"             
OmicS_searchAssayType("transcript")
#> [1] "transcriptomics"
```
