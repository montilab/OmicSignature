% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createMetadata.R
\name{createMetadata}
\alias{createMetadata}
\title{template for creating a metadata list for an OmicSignature R6 object
updated 05/2021}
\usage{
createMetadata(
  signature_name,
  organism,
  phenotype = "unknown",
  covariates = "none",
  platform = "GPLXXXXX",
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
  ...
)
}
\arguments{
\item{signature_name}{required. name of the signature.}

\item{organism}{required. e.g. "Homo Sapiens", "Mus Musculus".}

\item{phenotype}{optional but highly recommended. e.g. "Gene KO", "Parkinson disease". Use "unknown" or NULL if not applicable.}

\item{covariates}{optional. e.g. a signature of aging may have covariate "gender".}

\item{platform}{optional but highly recommended. GEO platform name. e.g. "GPL11154" is for Illumina HiSeq 2000 Homo sapiens. Use "GPLXXXXX" or NULL if not available.}

\item{direction_type}{required. the direction information of the signature.
"uni" or "uni-directional" if signature has only one direction or no direction infomation.
"bi" or "bi-directional" if signature contains "up" and "down" regulated features.
"multi" or "multi-directional" if the signature contains more categories.}

\item{sample_type}{optional but highly recommended. a cell line or tissue from BRENDA ontology.}

\item{signature_collection}{optional. collection name that the signature belongs to.}

\item{author}{optional. the author name if the signature is from a published article.}

\item{year}{optional. the year when the signature was created or published.}

\item{PMID}{optional. the PubMed ID if the signature is from a published article.}

\item{keywords}{optional. key words for the signature. examples are "longevity", "perturbation". "drug".}

\item{description}{optional. free text to describe the signature.}

\item{category_num}{optional, specifically used for multi-directional signature to specify how many categories or class the signature contains.}

\item{logfc_cutoff}{optional. log fold change cutoff used to generate the signature, if applicable.}

\item{p_value_cutoff}{optional. p value cutoff used to generate the signature, if applicable.}

\item{adj_p_cutoff}{optional. adjusted p-value, e.g. fdr, cutoff used to generate the signature, if applicable.}

\item{score_cutoff}{optional. score cutoff used to generate the signature, if applicable.}

\item{cutoff_description}{optional. discription of the cutoff, if applicable.}

\item{...}{additional user-defined metadata fields.}
}
\value{
a metadata list to create an OmicSignature R6 object.
}
\description{
template for creating a metadata list for an OmicSignature R6 object
updated 05/2021
}
