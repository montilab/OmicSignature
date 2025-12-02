# search for a platform name to use (deprecated 10/2025)

search for a platform name to use (deprecated 10/2025)

## Usage

``` r
OmicS_searchPlatform(
  x = "",
  platforms = predefined_platforms,
  contain_all = TRUE
)
```

## Arguments

- x:

  a string or character vector to search for. input a character vector
  if searching for multiple terms. if empty, will return all available
  organisms.

- platforms:

  pre-defined platform information character variable.

- contain_all:

  if TRUE, results contain all search terms will be returned. if FALSE,
  results contain any of the given term will be returned.

## Value

character or dataframe of search result

## Examples

``` r
OmicS_searchPlatform()
#>  [1] "unknown"                                    
#>  [2] "DNA assay by ChIP-seq"                      
#>  [3] "DNA assay by ATAC-seq"                      
#>  [4] "genotyping by array"                        
#>  [5] "genotyping by whole-genome-sequencing"      
#>  [6] "transcriptomics by array"                   
#>  [7] "transcriptomics by bulk RNA-seq"            
#>  [8] "transcriptomics by long-read sequencing"    
#>  [9] "transcriptomics by single-cell RNA-seq"     
#> [10] "ribosome transcriptomics"                   
#> [11] "spatial transcriptomics"                    
#> [12] "single-cell spatial transcriptomics"        
#> [13] "proteomics by array"                        
#> [14] "proteomics by mass spectrometry"            
#> [15] "proteomics by NMR"                          
#> [16] "proteomics by antibody or aptamer"          
#> [17] "proteomics by fluorescence"                 
#> [18] "single-cell proteomics by mass spectrometry"
#> [19] "metabolomics by mass spectrometry"          
#> [20] "metabolomics by gas chromatography"         
#> [21] "metabolomics by liquid chromatography HPLC" 
#> [22] "metabolomics by NMR"                        
#> [23] "metabolomics by fluorescence"               
#> [24] "methylation by array"                       
#> [25] "methylation by bisulfite sequencing"        
#> [26] "methylation by immunoprecipitation"         
#> [27] "single-cell CITE-seq"                       
#> [28] "cell flow cytometry"                        
OmicS_searchPlatform("proteomics")
#> [1] "proteomics by array"                        
#> [2] "proteomics by mass spectrometry"            
#> [3] "proteomics by NMR"                          
#> [4] "proteomics by antibody or aptamer"          
#> [5] "proteomics by fluorescence"                 
#> [6] "single-cell proteomics by mass spectrometry"
OmicS_searchPlatform(c("transcript", "single-cell"), contain_all = TRUE)
#> [1] "transcriptomics by single-cell RNA-seq"
#> [2] "single-cell spatial transcriptomics"   
```
