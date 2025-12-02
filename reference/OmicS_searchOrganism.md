# search for a pre-defined organism name to use (deprecated 10/2025)

search for a pre-defined organism name to use (deprecated 10/2025)

## Usage

``` r
OmicS_searchOrganism(x = "", organism = predefined_organisms)
```

## Arguments

- x:

  a string to search for (case-insensitive). if empty, will return all
  available organisms.

- organism:

  pre-defined organism character variable.

## Value

character of search result

## Examples

``` r
OmicS_searchOrganism()
#> [1] "unknown"                 "Homo sapiens"           
#> [3] "Mus musculus"            "Rattus norvegicus"      
#> [5] "Danio rerio"             "Heterocephalus glaber"  
#> [7] "Caenorhabditis elegans"  "Drosophila melanogaster"
#> [9] "Arabidopsis thaliana"   
OmicS_searchOrganism("homo")
#> [1] "Homo sapiens"
```
