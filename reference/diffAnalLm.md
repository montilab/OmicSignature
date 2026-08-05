# differential expression analysis using limma package

updated 08/2020

## Usage

``` r
diffAnalLm(dat, ctrl_columns = c(2:4), trt_columns = c(5:7), id = "ID_REF")
```

## Arguments

- dat:

  dataframe or matrix containing expression data. rows should be
  features (e.g. genes) and columns should be samples.

- ctrl_columns:

  column names (character vector) or column numbers (numeric vector) of
  control samples. input type need to be consistant with trt_columns.

- trt_columns:

  column names (character vector) or column numbers (numeric vector) of
  treatment samples. input type need to be consistant with ctrl_columns.

- id:

  the id for the features, usually probe id. either the column name of
  the input dataframe contains the id, or character vector of the actual
  ids for all features.

## Value

dataframe of differential analysis
