# Sample Type & Platform Info

``` r

devtools::load_all(".")
```

    ## ℹ Loading OmicSignature

We use BRENDA tissue ontology to indicate the tissue or cell-line of a
signature. BRENDA tissue ontology version: **10/2021**. The OBO file was
downloaded at <https://www.brenda-enzymes.org/ontology.php>.  

To ensure accurate documentation of the technologies used in the
experiment, we provided a predefined list of experimental platforms.  

If you are not sure which BRENDA tissue ontology or platform name to
use, you can search for them.

## Search for sample type and platform

Multiple search terms are seperated by space ” “. The search is case
in-sensitive.  
Set `contain_all = TRUE` to show results include all search terms.  
Set `contain_all = FALSE` to show results include any of the search
terms.  

``` r

OmicS_searchSampleType("mcf cell", contain_all = TRUE)
```

    ##               ID              Name
    ## 95   BTO:0000093        MCF-7 cell
    ## 1939 BTO:0001939      MCF-10A cell
    ## 2270 BTO:0002270     MCF-7/2a cell
    ## 2544 BTO:0002544      MCF-12A cell
    ## 2545 BTO:0002545      MCF-12F cell
    ## 3900 BTO:0003900      MCF-10F cell
    ## 5124 BTO:0005127 MCF-10A neoT cell
    ## 5189 BTO:0005192    MCF-7/adr cell
    ## 5967 BTO:0005970   MCF-7/LCC9 cell
    ## 6395 BTO:0006398    MCF-10-2A cell

``` r

OmicS_searchSampleType("MCF-10 MCF-7", contain_all = FALSE)
```

    ##               ID              Name
    ## 95   BTO:0000093        MCF-7 cell
    ## 2270 BTO:0002270     MCF-7/2a cell
    ## 5189 BTO:0005192    MCF-7/adr cell
    ## 5967 BTO:0005970   MCF-7/LCC9 cell
    ## 1939 BTO:0001939      MCF-10A cell
    ## 3900 BTO:0003900      MCF-10F cell
    ## 5124 BTO:0005127 MCF-10A neoT cell
    ## 6395 BTO:0006398    MCF-10-2A cell

Search for a single word:

``` r

OmicS_searchPlatform("proteomics")
```

    ## [1] "proteomics by array"                        
    ## [2] "proteomics by mass spectrometry"            
    ## [3] "proteomics by NMR"                          
    ## [4] "proteomics by antibody or aptamer"          
    ## [5] "proteomics by fluorescence"                 
    ## [6] "single-cell proteomics by mass spectrometry"

Search for multiple words:

``` r

OmicS_searchPlatform(c("transcript", "single-cell"), contain_all = TRUE)
```

    ## [1] "transcriptomics by single-cell RNA-seq"
    ## [2] "single-cell spatial transcriptomics"

If `contain_all = FALSE`, results matching any of the key words will be
returned.  

``` r

OmicS_searchPlatform(c("transcript", "single-cell"), contain_all = FALSE)
```

    ## [1] "transcriptomics by single-cell RNA-seq"     
    ## [2] "single-cell spatial transcriptomics"        
    ## [3] "single-cell proteomics by mass spectrometry"
    ## [4] "single-cell CITE-seq"                       
    ## [5] "transcriptomics by array"                   
    ## [6] "transcriptomics by bulk RNA-seq"            
    ## [7] "transcriptomics by long-read sequencing"    
    ## [8] "ribosome transcriptomics"                   
    ## [9] "spatial transcriptomics"

## 
