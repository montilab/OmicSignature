# Create OmicSignatureCollection

``` r

devtools::load_all(".")
library(dplyr)
```

## Create an OmicSignatureCollection Object

This object contains several `OmicSignature` objects to facilitate
further analysis.  

An `OmicSignatureCollection` object contains two parts:  
- **metadata**  
- **OmicSigList**, a list of OmicSignature Objects  

### 1. Metadata for the Collection

The required fields for **metadata** are:  
“**collection_name**”, “**description**”.  
Additional optional fields can be added.  

``` r

ColMeta <- list(
  "collection_name" = "Example_Collection",
  "description" = "An example of signature collection",
  "organism" = "Mus Musculus",
  "author" = "me"
)
```

### 2. Prepare `OmicSignature` objects

Create or read OmicSignature objects.  
The following signatures are pseudo results for illustration purpose.  

``` r

OmicObj1 <- readJson(file.path(system.file("extdata", package = "OmicSignature"), "OmS_example_1.json"))
#>   [Success] OmicSignature object Experiment in liver created.
OmicObj2 <- readJson(file.path(system.file("extdata", package = "OmicSignature"), "OmS_example_2.json"))
#>   [Success] OmicSignature object Experiment in brain created.
OmicObj3 <- readJson(file.path(system.file("extdata", package = "OmicSignature"), "OmS_example_3.json"))
#>   [Success] OmicSignature object Experiment in heart created.
```

### 3. Create an `OmicSignatureCollection` object

Use `OmicSignatureCollection$new()` and provide metadata and a list of
`OmicSignature` objects:

``` r

OmicCol <- OmicSignatureCollection$new(
  OmicSigList = list(OmicObj1, OmicObj2, OmicObj3),
  metadata = ColMeta,
  print_message = FALSE
)

OmicCol
```

During the creation of `OmicSignatureCollection`, all input
`OmicSignature` objects will be re-created to make sure they pass all
the check functions.  

## 
