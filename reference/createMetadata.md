# template for creating a metadata list for an OmicSignature R6 object

updated 10/2025

## Usage

``` r
createMetadata(
  signature_name,
  organism,
  phenotype = "unknown",
  assay_type,
  covariates = NULL,
  platform = "unknown",
  direction_type,
  sample_type = NULL,
  signature_collection = NULL,
  author = NULL,
  year = NULL,
  PMID = NULL,
  keywords = NULL,
  description = NULL,
  category_num = NULL,
  logfc_cutoff = NULL,
  p_value_cutoff = NULL,
  adj_p_cutoff = NULL,
  score_cutoff = NULL,
  cutoff_description = NULL,
  others = NULL
)
```

## Arguments

- signature_name:

  required. name of the signature.

- organism:

  required. e.g. "Homo sapiens", "Mus musculus".

- phenotype:

  optional but highly recommended. input as a single string. e.g. "Gene
  KO", "Parkinson disease". Use "unknown" or NULL if not applicable.

- assay_type:

  required. must be one of the following: "transcriptomics",
  "proteomics", "metabolomics", "methylomics", "methylomics",
  "genetic_variations", "DNA_binding_sites", or "others". some of the
  common misspells, e.g. "gene", "protein" and "metab" will be changed
  automatically.

- covariates:

  optional. input covariates as a single string, and separate multiple
  covariates using comma, e.g. "age, gender".

- platform:

  optional but highly recommended. input as a single string. e.g.
  "transcriptomics by single-cell RNA-seq".

- direction_type:

  required. the direction information of the signature. "uni" or
  "uni-directional" if the signature is derived from one category. "bi"
  or "bi-directional" if the signature is derived from group A vs group
  B, or it contains "up" and "down" regulated features for a continuous
  phenotype. "categorical" if the signature is derived from comparisons
  between multiple groups, e.g. A vs B vs C.

- sample_type:

  optional but highly recommended. a cell line or tissue from BRENDA
  ontology.

- signature_collection:

  optional. the collection name that the signature belongs to.

- author:

  optional. the author's name.

- year:

  optional. a single-length numeric value. the year when the signature
  was created or published.

- PMID:

  optional. a single-length character value. the PubMed ID if the
  signature is from a published article.

- keywords:

  optional. key words for the signature. input as a multi-length
  character vector, e.g. c("longevity", "perturbation", "health").

- description:

  optional. free text to describe the signature. input as a
  single-length string. the character limit (including all spaces and
  symbols) is 65,535.

- category_num:

  required when direction_type = "categorical". numeric. a number
  indicates how many categories or class the signature contains.

- logfc_cutoff:

  optional. a single-length numeric value. log fold change cutoff used
  to generate the signature, if applicable.

- p_value_cutoff:

  optional. a single-length numeric value. p value cutoff used to
  generate the signature, if applicable.

- adj_p_cutoff:

  optional. a single-length numeric value. adjusted p-value, e.g. fdr,
  cutoff used to generate the signature, if applicable.

- score_cutoff:

  optional. a single-length numeric value. score cutoff used to generate
  the signature, if applicable.

- cutoff_description:

  optional. description of the cutoff, if applicable.

- others:

  provide additional user-defined metadata fields as a list. for
  example, others = list("animal_strain" = "C57BL/6", "lab" =
  "new_lab").

## Value

a metadata list to create an OmicSignature R6 object.
