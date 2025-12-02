# read an OmicSignature object from json txt file created by writeJson()

To avoid confusion, in the json text file, assume the column names in
signature dataframe and difexp dataframe have prefix "sig\_" and
"difexp". This corresponds to writeJson() function. updated 04/2024

## Usage

``` r
readJson(filename)
```

## Arguments

- filename:

  json file name to read in

## Value

OmicSignature object
