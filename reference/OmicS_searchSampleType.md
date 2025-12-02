# OmicS_searchSampleType

search for BRENDA tissue name updated 02/2024

## Usage

``` r
OmicS_searchSampleType(x, file = BRENDA, contain_all = TRUE)
```

## Arguments

- x:

  A string or character vector to search for (case-insensitive).
  Multiple search terms can be separated by space, or input them as a
  character vector.

- file:

  The BRENDA data frame, has columns ID and Name.

- contain_all:

  if TRUE, will only return the results contain all search terms. if
  FALSE, will return results contain any of the given pattern.

## Value

matrix including search result

## Examples

``` r
# search for results that contain all of "HEK" "293" and "T":
OmicS_searchSampleType("HEK 293 T", contain_all = TRUE)
#>               ID                      Name
#> 2181 BTO:0002181             HEK-293T cell
#> 3347 BTO:0003347    HEK-293 Tet-On 3G cell
#> 5022 BTO:0005025         HEK-293-TrkB cell
#> 5026 BTO:0005029            HEK-293FT cell
#> 5545 BTO:0005548            HEK-293ET cell
#> 6319 BTO:0006322 Jump-In-T-REx HEK293 cell
#> 6325 BTO:0006328          HEK-293T/17 cell
#> 6510 BTO:0006513          HEK-293-AT1 cell
OmicS_searchSampleType(c("HEK", "293", "T"), contain_all = TRUE)
#>               ID                      Name
#> 2181 BTO:0002181             HEK-293T cell
#> 3347 BTO:0003347    HEK-293 Tet-On 3G cell
#> 5022 BTO:0005025         HEK-293-TrkB cell
#> 5026 BTO:0005029            HEK-293FT cell
#> 5545 BTO:0005548            HEK-293ET cell
#> 6319 BTO:0006322 Jump-In-T-REx HEK293 cell
#> 6325 BTO:0006328          HEK-293T/17 cell
#> 6510 BTO:0006513          HEK-293-AT1 cell
OmicS_searchSampleType(c("HEK 293", "T"), contain_all = TRUE)
#>               ID                      Name
#> 2181 BTO:0002181             HEK-293T cell
#> 3347 BTO:0003347    HEK-293 Tet-On 3G cell
#> 5022 BTO:0005025         HEK-293-TrkB cell
#> 5026 BTO:0005029            HEK-293FT cell
#> 5545 BTO:0005548            HEK-293ET cell
#> 6319 BTO:0006322 Jump-In-T-REx HEK293 cell
#> 6325 BTO:0006328          HEK-293T/17 cell
#> 6510 BTO:0006513          HEK-293-AT1 cell

# search for results that contain any of "HEK", "SUM" or "HeLa":
OmicS_searchSampleType("HEK SUM HeLa", contain_all = FALSE)
#>               ID                                  Name
#> 568  BTO:0000567                             HeLa cell
#> 569  BTO:0000568                          HeLa-S3 cell
#> 2772 BTO:0002772                                 chela
#> 3180 BTO:0003180                          HeLa-80 cell
#> 3587 BTO:0003587                   HeLa-MAGI-CCR5 cell
#> 3588 BTO:0003588                        HeLa-MAGI cell
#> 3602 BTO:0003602             HeLa GFP-histone H2B cell
#> 5027 BTO:0005030                         HeLa-229 cell
#> 5035 BTO:0005038                HeLa-LTRHIV-1-Luc cell
#> 5996 BTO:0005999                           chelate leg
#> 6088 BTO:0006091                       HeLa-HL3T1 cell
#> 26   BTO:0000024                              abomasum
#> 274  BTO:0000272                     colon transversum
#> 350  BTO:0000348                                omasum
#> 363  BTO:0000361                    stratum granulosum
#> 437  BTO:0000435                      stratum spinosum
#> 616  BTO:0000615                       corpus callosum
#> 1713 BTO:0001713                                dorsum
#> 2018 BTO:0002018          corpus cavernosum clitoridis
#> 2019 BTO:0002019               corpus cavernosum penis
#> 3677 BTO:0003677                     chorion frondosum
#> 4134 BTO:0004136                         Kasumi-1 cell
#> 4423 BTO:0004425          stratum granulosum cerebelli
#> 4925 BTO:0004928                        SUM-159PT cell
#> 5384 BTO:0005387                        SUM-102PT cell
#> 5385 BTO:0005388                          SUM-149 cell
#> 5386 BTO:0005389                         SUM-44PE cell
#> 5387 BTO:0005390                         SUM-52PE cell
#> 5388 BTO:0005391                          SUM-225 cell
#> 5389 BTO:0005392                          SUM-229 cell
#> 5390 BTO:0005393                          SUM-190 cell
#> 5391 BTO:0005394                          SUM-185 cell
#> 5451 BTO:0005454                     corpus cavernosum
#> 5472 BTO:0005475                        SUM-149PT cell
#> 5473 BTO:0005476                        SUM-185PE cell
#> 5474 BTO:0005477                        SUM-190PT cell
#> 5475 BTO:0005478                       SUM-225CWN cell
#> 5476 BTO:0005479                        SUM-229PE cell
#> 5500 BTO:0005503       corpus cavernosum smooth muscle
#> 5929 BTO:0005932                              SUM cell
#> 5930 BTO:0005933                      SUM-1315MO2 cell
#> 6265 BTO:0006268 organum vasculosum laminae terminalis
#> 9    BTO:0000007                          HEK-293 cell
#> 2181 BTO:0002181                         HEK-293T cell
#> 2524 BTO:0002524                         HEK-293A cell
#> 2732 BTO:0002732                      HEK-293CymR cell
#> 2974 BTO:0002974                     HEK-293-EBNA cell
#> 3032 BTO:0003032                         HEK-293F cell
#> 3033 BTO:0003033                         HEK-293H cell
#> 3182 BTO:0003182                             NHEK cell
#> 3263 BTO:0003263                        HEK-AD293 cell
#> 3347 BTO:0003347                HEK-293 Tet-On 3G cell
#> 3468 BTO:0003468                        HEK-293B2 cell
#> 3615 BTO:0003615                HEK-293 PEAKrapid cell
#> 4073 BTO:0004075                       D1-HEK-293 cell
#> 4790 BTO:0004792                           RHEK-1 cell
#> 5022 BTO:0005025                     HEK-293-TrkB cell
#> 5026 BTO:0005029                        HEK-293FT cell
#> 5545 BTO:0005548                        HEK-293ET cell
#> 5836 BTO:0005839                       HEK-293VnR cell
#> 6094 BTO:0006097                             HEKa cell
#> 6319 BTO:0006322             Jump-In-T-REx HEK293 cell
#> 6325 BTO:0006328                      HEK-293T/17 cell
#> 6419 BTO:0006422                   HEK-293-APPswe cell
#> 6450 BTO:0006453                         HEK-293S cell
#> 6510 BTO:0006513                      HEK-293-AT1 cell
OmicS_searchSampleType(c("HEK", "SUM", "HeLa"), contain_all = FALSE)
#>               ID                                  Name
#> 568  BTO:0000567                             HeLa cell
#> 569  BTO:0000568                          HeLa-S3 cell
#> 2772 BTO:0002772                                 chela
#> 3180 BTO:0003180                          HeLa-80 cell
#> 3587 BTO:0003587                   HeLa-MAGI-CCR5 cell
#> 3588 BTO:0003588                        HeLa-MAGI cell
#> 3602 BTO:0003602             HeLa GFP-histone H2B cell
#> 5027 BTO:0005030                         HeLa-229 cell
#> 5035 BTO:0005038                HeLa-LTRHIV-1-Luc cell
#> 5996 BTO:0005999                           chelate leg
#> 6088 BTO:0006091                       HeLa-HL3T1 cell
#> 26   BTO:0000024                              abomasum
#> 274  BTO:0000272                     colon transversum
#> 350  BTO:0000348                                omasum
#> 363  BTO:0000361                    stratum granulosum
#> 437  BTO:0000435                      stratum spinosum
#> 616  BTO:0000615                       corpus callosum
#> 1713 BTO:0001713                                dorsum
#> 2018 BTO:0002018          corpus cavernosum clitoridis
#> 2019 BTO:0002019               corpus cavernosum penis
#> 3677 BTO:0003677                     chorion frondosum
#> 4134 BTO:0004136                         Kasumi-1 cell
#> 4423 BTO:0004425          stratum granulosum cerebelli
#> 4925 BTO:0004928                        SUM-159PT cell
#> 5384 BTO:0005387                        SUM-102PT cell
#> 5385 BTO:0005388                          SUM-149 cell
#> 5386 BTO:0005389                         SUM-44PE cell
#> 5387 BTO:0005390                         SUM-52PE cell
#> 5388 BTO:0005391                          SUM-225 cell
#> 5389 BTO:0005392                          SUM-229 cell
#> 5390 BTO:0005393                          SUM-190 cell
#> 5391 BTO:0005394                          SUM-185 cell
#> 5451 BTO:0005454                     corpus cavernosum
#> 5472 BTO:0005475                        SUM-149PT cell
#> 5473 BTO:0005476                        SUM-185PE cell
#> 5474 BTO:0005477                        SUM-190PT cell
#> 5475 BTO:0005478                       SUM-225CWN cell
#> 5476 BTO:0005479                        SUM-229PE cell
#> 5500 BTO:0005503       corpus cavernosum smooth muscle
#> 5929 BTO:0005932                              SUM cell
#> 5930 BTO:0005933                      SUM-1315MO2 cell
#> 6265 BTO:0006268 organum vasculosum laminae terminalis
#> 9    BTO:0000007                          HEK-293 cell
#> 2181 BTO:0002181                         HEK-293T cell
#> 2524 BTO:0002524                         HEK-293A cell
#> 2732 BTO:0002732                      HEK-293CymR cell
#> 2974 BTO:0002974                     HEK-293-EBNA cell
#> 3032 BTO:0003032                         HEK-293F cell
#> 3033 BTO:0003033                         HEK-293H cell
#> 3182 BTO:0003182                             NHEK cell
#> 3263 BTO:0003263                        HEK-AD293 cell
#> 3347 BTO:0003347                HEK-293 Tet-On 3G cell
#> 3468 BTO:0003468                        HEK-293B2 cell
#> 3615 BTO:0003615                HEK-293 PEAKrapid cell
#> 4073 BTO:0004075                       D1-HEK-293 cell
#> 4790 BTO:0004792                           RHEK-1 cell
#> 5022 BTO:0005025                     HEK-293-TrkB cell
#> 5026 BTO:0005029                        HEK-293FT cell
#> 5545 BTO:0005548                        HEK-293ET cell
#> 5836 BTO:0005839                       HEK-293VnR cell
#> 6094 BTO:0006097                             HEKa cell
#> 6319 BTO:0006322             Jump-In-T-REx HEK293 cell
#> 6325 BTO:0006328                      HEK-293T/17 cell
#> 6419 BTO:0006422                   HEK-293-APPswe cell
#> 6450 BTO:0006453                         HEK-293S cell
#> 6510 BTO:0006513                      HEK-293-AT1 cell
OmicS_searchSampleType(c("HEK SUM", "HeLa"), contain_all = FALSE)
#>               ID                                  Name
#> 568  BTO:0000567                             HeLa cell
#> 569  BTO:0000568                          HeLa-S3 cell
#> 2772 BTO:0002772                                 chela
#> 3180 BTO:0003180                          HeLa-80 cell
#> 3587 BTO:0003587                   HeLa-MAGI-CCR5 cell
#> 3588 BTO:0003588                        HeLa-MAGI cell
#> 3602 BTO:0003602             HeLa GFP-histone H2B cell
#> 5027 BTO:0005030                         HeLa-229 cell
#> 5035 BTO:0005038                HeLa-LTRHIV-1-Luc cell
#> 5996 BTO:0005999                           chelate leg
#> 6088 BTO:0006091                       HeLa-HL3T1 cell
#> 26   BTO:0000024                              abomasum
#> 274  BTO:0000272                     colon transversum
#> 350  BTO:0000348                                omasum
#> 363  BTO:0000361                    stratum granulosum
#> 437  BTO:0000435                      stratum spinosum
#> 616  BTO:0000615                       corpus callosum
#> 1713 BTO:0001713                                dorsum
#> 2018 BTO:0002018          corpus cavernosum clitoridis
#> 2019 BTO:0002019               corpus cavernosum penis
#> 3677 BTO:0003677                     chorion frondosum
#> 4134 BTO:0004136                         Kasumi-1 cell
#> 4423 BTO:0004425          stratum granulosum cerebelli
#> 4925 BTO:0004928                        SUM-159PT cell
#> 5384 BTO:0005387                        SUM-102PT cell
#> 5385 BTO:0005388                          SUM-149 cell
#> 5386 BTO:0005389                         SUM-44PE cell
#> 5387 BTO:0005390                         SUM-52PE cell
#> 5388 BTO:0005391                          SUM-225 cell
#> 5389 BTO:0005392                          SUM-229 cell
#> 5390 BTO:0005393                          SUM-190 cell
#> 5391 BTO:0005394                          SUM-185 cell
#> 5451 BTO:0005454                     corpus cavernosum
#> 5472 BTO:0005475                        SUM-149PT cell
#> 5473 BTO:0005476                        SUM-185PE cell
#> 5474 BTO:0005477                        SUM-190PT cell
#> 5475 BTO:0005478                       SUM-225CWN cell
#> 5476 BTO:0005479                        SUM-229PE cell
#> 5500 BTO:0005503       corpus cavernosum smooth muscle
#> 5929 BTO:0005932                              SUM cell
#> 5930 BTO:0005933                      SUM-1315MO2 cell
#> 6265 BTO:0006268 organum vasculosum laminae terminalis
#> 9    BTO:0000007                          HEK-293 cell
#> 2181 BTO:0002181                         HEK-293T cell
#> 2524 BTO:0002524                         HEK-293A cell
#> 2732 BTO:0002732                      HEK-293CymR cell
#> 2974 BTO:0002974                     HEK-293-EBNA cell
#> 3032 BTO:0003032                         HEK-293F cell
#> 3033 BTO:0003033                         HEK-293H cell
#> 3182 BTO:0003182                             NHEK cell
#> 3263 BTO:0003263                        HEK-AD293 cell
#> 3347 BTO:0003347                HEK-293 Tet-On 3G cell
#> 3468 BTO:0003468                        HEK-293B2 cell
#> 3615 BTO:0003615                HEK-293 PEAKrapid cell
#> 4073 BTO:0004075                       D1-HEK-293 cell
#> 4790 BTO:0004792                           RHEK-1 cell
#> 5022 BTO:0005025                     HEK-293-TrkB cell
#> 5026 BTO:0005029                        HEK-293FT cell
#> 5545 BTO:0005548                        HEK-293ET cell
#> 5836 BTO:0005839                       HEK-293VnR cell
#> 6094 BTO:0006097                             HEKa cell
#> 6319 BTO:0006322             Jump-In-T-REx HEK293 cell
#> 6325 BTO:0006328                      HEK-293T/17 cell
#> 6419 BTO:0006422                   HEK-293-APPswe cell
#> 6450 BTO:0006453                         HEK-293S cell
#> 6510 BTO:0006513                      HEK-293-AT1 cell
```
