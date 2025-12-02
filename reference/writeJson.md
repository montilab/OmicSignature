# write OmicSignature object into json txt format

To avoid confusion, in the written json text file, the column names in
signature dataframe and difexp dataframe will have prefix "sig\_" and
"difexp" added. This corresponds to readJson() function. updated 08/2025

## Usage

``` r
writeJson(OmicObj, file)
```

## Arguments

- OmicObj:

  A OmicSignature object

- file:

  file name to write
