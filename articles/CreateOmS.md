# Create OmicSignature

``` r

devtools::load_all(".")
library(dplyr)
```

## 1. Cheatsheet

An `OmicSignature` object contains three parts:  

- **metadata**, a list.  
  Required fields:  
  “**signature_name**”, “**organism**”, “**direction_type**”,
  “**assay_type**”, “**phenotype**”, “**author**”.  
  Recommended optional fields, if applicable:  
  “platform”, “sample_type”, “description”, “covariates”,
  “score_cutoff”, “adj_p_cutoff”.  
  All of the above information should be provided as a string (spaces
  and punctuation are allowed) or one numeric value, not as a
  multi-length vector.  

- **signature**, a data frame.  
  Required columns:  
  “**probe_id**” (unique identifier for each feature)  
  “**feature_name**” (e.g. ENSEMBL ID, Uniprot ID)  
  Required for bi-directional and categorical signatures:
  “**group_label**”  
  Recommended optional column, if applicable: “score”  

- **difexp** (optional), a data frame of differential expression
  analysis results.  
  Required columns:  
  “**probe_id**” (unique identifier for each feature)  
  “**feature_name**” (e.g. ENSEMBL ID, Uniprot ID)  
  “**score**”  
  Required for bi-directional and categorical signatures:
  “**group_label**”  
  At least one of the following: “**p_value**”, “**q_value**”, or
  “**adj_p**”.  
  Recommended optional column, if applicable: “gene_symbol”  

Create the object:  

    OmS <- OmicSignature$new(
      metadata = metadata,
      signature = signature,
      difexp = difexp
    )

## 2. Create an `OmicSignature` Object Step-by-Step

The example provided below is from an experiment evaluating the effects
of Myc reduced expression by comparing liver profiles of 24-month old
*Myc*$`^{+/+}`$*vs.* *Myc*$`^{+/-}`$ mice. This is a *bi-directional*
signature example, since it is a comparison between two groups, and
contains up and down regulated features (genes). For ease of exposition,
only the top 1000 genes are here included.  

### 2.1. Metadata

A list with the following required fields:  
“**signature_name**”, “**organism**”, “**direction_type**”,
“**assay_type**”, “**phenotype**”, “**author**”.  
  
To make collaboration easier, we recommend including your work email
address along with your name in the author field.  
  
Not required, but highly recommended fields:  
“**platform**”, “**sample_type**”, “**description**”, “**covariates**”,
“**score_cutoff**”, “**adj_p_cutoff**”, “**logfc_cutoff**”.  
  
Option 1: Create `metadata` by hand. This is not recommended, because
typos can occur.  

    metadata <- list(
      "signature_name" = Myc_reduce_mice_liver_24m,
      "organism" = "Mus musculus",
      "sample_type" = "liver",
      "phenotype" = "Myc_reduce",
      "direction_type" = "bi-directional",
      "assay_type" = "transcriptomics", 
      "platform" = "transcriptomics by array",
      "author" = "researcher@institute.edu"
    )

Option 2: Use
[`createMetadata()`](https://montilab.github.io/OmicSignature/reference/createMetadata.md)
(recommended).  
This function helps remind you of the built-in attributes. The full list
of current built-in attributes is shown
[here](https://montilab.github.io/OmicSignature/reference/createMetadata.html).  
You can also provide your own customized attributes using the “others”
field.  

``` r

metadata <- createMetadata(
  # required attributes:
  signature_name = "Myc_reduce_mice_liver_24m",
  organism = "Mus musculus",
  direction_type = "bi-directional",
  assay_type = "transcriptomics",
  phenotype = "Myc_reduce",
  author = "researcher@institute.edu",

  # optional and recommended attributes:
  covariates = "age, gender",
  description = "mice Myc haploinsufficient (Myc(+/-))",
  platform = "transcriptomics by array",
  sample_type = "liver", # recommended using BRENDA ontology

  # optional cut-off attributes:
  # specifying them can facilitate the extraction of signatures.
  logfc_cutoff = NULL,
  p_value_cutoff = NULL,
  adj_p_cutoff = 0.05,
  score_cutoff = 5,

  # other optional built-in attributes:
  keywords = "Myc, KO, longevity",
  PMID = "25619689",
  year = 2015,

  # example of other customized attributes:
  others = list("animal_strain" = "C57BL/6")
)
```

#### 2.1.1 “phenotype”

“‘phenotype’ is free text, usually a one- or two-word description of the
experimental condition or trait under study, such as a drug treatment, a
gene knockout, or a clinical characteristic. Examples include *age*,
*breast cancer*, and *drug X*.

Providing a detailed (free text) “description” is highly recommended.
For instance, it may include information about how the treatment was
administered and how each group was defined. The character limit of the
description is 65,535 (16-bit storage), space included.  

#### 2.1.2 “organism

“organism” is free text. We provide a pre-defined list of common
organisms, which you can search using
[`OmicS_searchOrganism()`](https://montilab.github.io/OmicSignature/reference/OmicS_searchOrganism.md).
Other entries are allowed, but please use standard naming conventions
(e.g., “Homo sapiens”, “Mus musculus”) to ensure consistency.  

``` r

OmicS_searchOrganism("homo")
#> [1] "Homo sapiens"
```

#### 2.1.3 “sample_type” and “platform”

While both “sample_type” and “platform” are free text, it is recommended
that predefined terms are used. Thus, “sample_type” should be a BRENDA
ontology term, and “platform” should be one of a set of predefined
platforms, if appropriate. You can search for predefined terms via the
`OmicS_searchSampleType` and `OmicS_searchPlatform` functions. See
[“Sample Type & Platform
Info”](https://montilab.github.io/OmicSignature/articles/SampleType.html)
for details.  

#### 2.1.4 “direction_type”

`direction_type` must be one of the following:  

- “uni-directional”. Only a list of significant feature names is
  available. Examples including “genes mutated in a disease” and
  “markers of a specific cell type”.  

- “bi-directional”. Significant features are derived from comparison of
  two groups, or a single continuous trait. Thus, the resulting
  significant features can be grouped into two categories. For example,
  when comparing treatment *vs.* control groups, some features will be
  higher and some will be lower in the treatment group. When the
  phenotype is a continuous trait, such as age, some features will
  increase with age, while others will decrease with age.  

- “categorical”. Used with multi-valued categorical phenotypes (e.g.,
  “A” *vs.* “B” *vs.* “C”), usually analyzed by ANOVA.  

#### 2.1.5 “assay_type”

`assay_type` is one of the following:  
- “transcriptomics” (e.g. RNA-seq, micro-array)  
- “proteomics”  
- “metabolomics”  
- “methylomics”  
- “genetic_variations” (e.g. SNP, GWAS)  
- “DNA_binding_sites” (e.g. ChIP-seq)  
- “other”  

You can also use
[`OmicS_searchAssayType()`](https://montilab.github.io/OmicSignature/reference/OmicS_searchAssayType.md)
to see the list above.  

### 2.2. Signature

**signature** is a dataframe with the columns **“probe_id”** and
**“feature_name”**. If the signature is bi-directional or categorical
(as specified in `direction_type` within `metadata`), an additional
column, **“group_label”**, is also required.  
An optional column **“score”** is highly recommended when applicable.  
  
“**probe_id**” is a unique identifier, usually a platform-specific
identifider (e.g., probe IDs for Affymetrix microarrays, or aptamer IDs
for SomaScan assays). If not provided, it will be automatically
generated.  
“**feature_name**” is a name that identifies each feature, examples
include ENSEMBL IDs for transcripts, UniProt IDs for proteins, and
Refmet IDs for metabolites. To better identify the features, it is
recommended to add an additional annotation column(s), e.g.,
“gene_symbol”. For metabolite features, it is recommended to include
multiple annotation columns if available, e.g., HMDB ID and InChI key.  
**“group_label”** is a factor column. This column indicates the
experimental group in which a feature is more highly expressed or more
significant. For example, if the analysis identifies genes
differentially expressed in *Treatment v.s. Control*, all gene features
with a positive score should be labeled “Treatment” in this column,
indicating their higher expression in the Treatment group. Features with
negative scores should be labeled “Control”. Similarly, if the analysis
concerns protein expression changes associated with BMI, protein
features with positive scores should be labeled “higher BMI”, while
those with negative scores should be labeled “lower BMI”.  
  
  
**Option 1**: Extract signature from a differential analysis results
table.  
In this example, we extract a bi-directional signature from a `difexp`
object (see next section) using the `score_cutoff` and `adj_p_cutoff`
specified in the metadata. A `difexp` object is a properly formatted
data frame reporting the results of a differential analysis (e.g., by
Limma).  

    #>   probe_id  logfc  mean  score p_value adj_p      b       feature_name
    #> 1 10345228 -0.167 7.106 -1.470   0.186 0.560 -5.866 ENSMUSG00000103746
    #> 2 10354534  0.041 4.351  0.520   0.620 0.870 -6.780 ENSMUSG00000060715
    #> 3 10354529 -0.175 4.955 -0.941   0.379 0.731 -6.458 ENSMUSG00000043629
    #> 4 10346337  0.025 8.621  0.188   0.857 0.962 -6.911 ENSMUSG00000038323
    #> 5 10353792 -0.025 6.063 -0.284   0.785 0.936 -6.885 ENSMUSG00000045815
    #> 6 10350848 -0.055 7.595 -0.630   0.549 0.836 -6.712 ENSMUSG00000049881

We can then extract the significant features as follows:

``` r

signature <- difexp %>%
  dplyr::filter(abs(score) > metadata$score_cutoff & adj_p < metadata$adj_p_cutoff) %>%
  dplyr::select(probe_id, feature_name, score) %>%
  dplyr::mutate(group_label = as.factor(ifelse(score > 0, "MYC Reduce", "WT")))
head(signature)
#>   probe_id       feature_name   score group_label
#> 1 10346882 ENSMUSG00000025964  -6.990          WT
#> 2 10353878 ENSMUSG00000067653  -7.867          WT
#> 3 10349648 ENSMUSG00000004552  14.762  MYC Reduce
#> 4 10355278 ENSMUSG00000062209   6.083  MYC Reduce
#> 5 10353192 ENSMUSG00000025932  10.487  MYC Reduce
#> 6 10345762 ENSMUSG00000026072 -13.543          WT
```

Function
[`standardizeSigDF()`](https://montilab.github.io/OmicSignature/reference/standardizeSigDF.md)
can help remove duplicated and empty names.

``` r

signature <- standardizeSigDF(signature)
head(signature)
#>   probe_id       feature_name   score group_label
#> 1 10349648 ENSMUSG00000004552  14.762  MYC Reduce
#> 2 10345762 ENSMUSG00000026072 -13.543          WT
#> 3 10353192 ENSMUSG00000025932  10.487  MYC Reduce
#> 4 10355259 ENSMUSG00000061816 -10.315          WT
#> 5 10351477 ENSMUSG00000102418   8.818  MYC Reduce
#> 6 10353878 ENSMUSG00000067653  -7.867          WT
```

**Option 2**: Manually create signature dataframe.  
For uni-directional signature:  

    signature <- data.frame("probe_id" = c(1, 2, 3), "feature_name" = c("gene1", "gene2", "gene3"))

For bi-directional signature:  

    signature <- data.frame(
      "probe_id" = c(1, 2, 3),
      "feature_name" = c("gene1", "gene2", "gene3"),
      "score" = c(0.45, -3.21, 2.44),
      "group_label" = c("Treatment", "Control", "Treatment")
    )

For multi-categorical signature:  

    signature <- data.frame(
      "probe_id" = c(1, 2, 3, 4),
      "feature_name" = c("gene1", "gene2", "gene3", "gene4"),
      "score" = c(0.45, -3.21, 2.44, -2.45),
      "group_label" = c("group1", "group1", "group2", "group3")
    )

### 2.3. Differential expression analysis results (difexp)

A differential expression dataframe is optional but **recommended** if
available. It facilitates downstream signature extraction, and signature
comparison by rank-based test.  

`difexp` is a dataframe with the following **required** columns:  
“**probe_id**”, “**feature_name**”, “**score**”, along with at least one
of the following: “**p_value**”, “**q_value**”, or “**adj_p**”. Same as
in the signature dataframe, **“group_label”** is also required when the
signature is bi-directional or categorical.  
Descriptions of probe_id, feature_name, and group_label were provided in
the signature section above.  
“**p_value**”, “**q_value**”, or “**adj_p**” refers to the p- or q-value
representing the significance of each feature.  
“**score**” is a numeric value that indicates the importance or
significance of a feature. Depending on how the signature was derived,
this can be the t-statistics, log-fold change, Z-score, or other summary
statistics.  
  
Here we use an example from the differential expression analysis using
the `limma` package.

``` r

# Version reading from a txt file
# difexp <- read.table(
#   file.path(
#     system.file("extdata", package = "OmicSignature"),
#     "difmatrix_Myc_mice_liver_24m.txt"
#   ),
#   header = TRUE, sep = "\t", stringsAsFactors = FALSE
# )
# Version reading from a binary file
difexp <- readRDS(
  file.path(
    system.file("extdata", package = "OmicSignature"),
    "difmatrix_Myc_mice_liver_24m.rds"
  )
)
head(difexp)
#>   Probe.ID  logFC AveExpr      t P.Value adj.P.Val      b            ensembl
#> 1 10345228 -0.167   7.106 -1.470   0.186     0.560 -5.866 ENSMUSG00000103746
#> 2 10354534  0.041   4.351  0.520   0.620     0.870 -6.780 ENSMUSG00000060715
#> 3 10354529 -0.175   4.955 -0.941   0.379     0.731 -6.458 ENSMUSG00000043629
#> 4 10346337  0.025   8.621  0.188   0.857     0.962 -6.911 ENSMUSG00000038323
#> 5 10353792 -0.025   6.063 -0.284   0.785     0.936 -6.885 ENSMUSG00000045815
#> 6 10350848 -0.055   7.595 -0.630   0.549     0.836 -6.712 ENSMUSG00000049881
#>     gene_symbol
#> 1 1700001G17Rik
#> 2 1700019A02Rik
#> 3 1700019D03Rik
#> 4 1700066M21Rik
#> 5 1700101I19Rik
#> 6 2810025M15Rik
```

Manually change the column names to match the requirement. The built-in
function
[`replaceDifexpCol()`](https://montilab.github.io/OmicSignature/reference/replaceDifexpCol.md)
is designed to replace *some* of the frequently used alternative column
names.  
If some required columns are not in the difexp, it will give you a
warning:  

``` r

colnames(difexp) <- replaceDifexpCol(colnames(difexp))
#> Warning in replaceDifexpCol(colnames(difexp)): Required column for
#> OmicSignature object difexp: feature_name, is not found in your input. This may
#> cause problem when creating your OmicSignature object.
```

Create the required columns “feature_name” and “group_label”:

``` r

difexp <- difexp %>%
  rename(feature_name = ensembl) %>%
  mutate(group_label = as.factor(ifelse(score > 0, "MYC Reduce", "WT")))
head(difexp)
#>   probe_id  logfc  mean  score p_value adj_p      b       feature_name
#> 1 10345228 -0.167 7.106 -1.470   0.186 0.560 -5.866 ENSMUSG00000103746
#> 2 10354534  0.041 4.351  0.520   0.620 0.870 -6.780 ENSMUSG00000060715
#> 3 10354529 -0.175 4.955 -0.941   0.379 0.731 -6.458 ENSMUSG00000043629
#> 4 10346337  0.025 8.621  0.188   0.857 0.962 -6.911 ENSMUSG00000038323
#> 5 10353792 -0.025 6.063 -0.284   0.785 0.936 -6.885 ENSMUSG00000045815
#> 6 10350848 -0.055 7.595 -0.630   0.549 0.836 -6.712 ENSMUSG00000049881
#>     gene_symbol group_label
#> 1 1700001G17Rik          WT
#> 2 1700019A02Rik  MYC Reduce
#> 3 1700019D03Rik          WT
#> 4 1700066M21Rik  MYC Reduce
#> 5 1700101I19Rik          WT
#> 6 2810025M15Rik          WT
```

### 2.4. Create the `OmicSignature` object

``` r

OmS <- OmicSignature$new(
  metadata = metadata,
  signature = signature,
  difexp = difexp
)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m created.
```

Set `print_message` = `TRUE` to see all the messages.  

``` r

OmS <- OmicSignature$new(
  metadata = metadata,
  signature = signature,
  difexp = difexp,
  print_message = TRUE
)
#>   -- Required attributes for metadata: signature_name, phenotype, organism, direction_type, assay_type --
#>   [Success] Metadata is saved. 
#>   [Success] Signature is valid. 
#>   [Success] difexp is valid. 
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m created.
```

See the created object information:

``` r

print(OmS)
#> Signature Object: 
#>   Metadata: 
#>     adj_p_cutoff = 0.05 
#>     assay_type = transcriptomics 
#>     author = researcher@institute.edu 
#>     covariates = age, gender 
#>     description = mice Myc haploinsufficient (Myc(+/-)) 
#>     direction_type = bi-directional 
#>     keywords = Myc, KO, longevity 
#>     organism = Mus musculus 
#>     others = C57BL/6 
#>     phenotype = Myc_reduce 
#>     platform = transcriptomics by array 
#>     PMID = 25619689 
#>     sample_type = liver 
#>     score_cutoff = 5 
#>     signature_name = Myc_reduce_mice_liver_24m 
#>     year = 2015 
#>   Metadata user defined fields: 
#>     animal_strain = C57BL/6 
#>   Signature: 
#>     MYC Reduce (5)
#>     WT (10)
#>   Differential Expression Data: 
#>     884 x 10
```

Use new criteria to extract significant features:  
(this does *not* change the `signature` saved in the object)

``` r

OmS$extractSignature("abs(score) > 10; adj_p < 0.01")
#>   probe_id       feature_name   score group_label
#> 1 10349648 ENSMUSG00000004552  14.762  MYC Reduce
#> 2 10345762 ENSMUSG00000026072 -13.543          WT
#> 3 10353192 ENSMUSG00000025932  10.487  MYC Reduce
#> 4 10355259 ENSMUSG00000061816 -10.315          WT
```

Besides saving and reading OmicSignature object in `.rds` format, you
can export the object as a text file in json format.  

    saveRDS(OmS, "Myc_reduce_mice_liver_24m_OmS.rds")
    writeJson(OmS, "Myc_reduce_mice_liver_24m_OmS.json")

See more in [“Functionalities of
OmicSignature”](https://montilab.github.io/OmicSignature/articles/FunOmS.html)
section.  

## 3. Create an `OmicSignature` from `difexp` and `metadata`

You can by-pass the generating signature process once you are an expert.
Simply provide cutoffs (e.g. `adj_p_cutoff` and `score_cutoff`) in the
`metadata`, make sure `difexp` has “adj_p”, “score” and “group_label”
columns, and use
[`OmicSigFromDifexp()`](https://montilab.github.io/OmicSignature/reference/OmicSigFromDifexp.md)
to automatically extract significant features and create the
`OmicSignature` object.  

``` r

OmS1 <- OmicSigFromDifexp(difexp, metadata)
#> -- criterias used to extract signatures:  abs(score) >= 5; adj_p <= 0.05 . 
#> 
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m created.
OmS1
#> Signature Object: 
#>   Metadata: 
#>     adj_p_cutoff = 0.05 
#>     assay_type = transcriptomics 
#>     author = researcher@institute.edu 
#>     covariates = age, gender 
#>     description = mice Myc haploinsufficient (Myc(+/-)) 
#>     direction_type = bi-directional 
#>     keywords = Myc, KO, longevity 
#>     organism = Mus musculus 
#>     others = C57BL/6 
#>     phenotype = Myc_reduce 
#>     platform = transcriptomics by array 
#>     PMID = 25619689 
#>     sample_type = liver 
#>     score_cutoff = 5 
#>     signature_name = Myc_reduce_mice_liver_24m 
#>     year = 2015 
#>   Metadata user defined fields: 
#>     animal_strain = C57BL/6 
#>   Signature: 
#>     MYC Reduce (5)
#>     WT (10)
#>   Differential Expression Data: 
#>     884 x 10
```

## 
