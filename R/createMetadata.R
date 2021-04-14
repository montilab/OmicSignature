#' @title template for creating a metadata list for an OmicSignature R6 object
#' updated 09/2020
#' @importFrom dplyr recode %>%
#' @param signature_name name of the signature.
#' @param signature_collection optional. collection of which the signature belongs to.
#' @param organism organism. e.g. "Homo Sapiens", "Mus Musculus".
#' @param platform GEO platform. e.g. "GPL11154" is for Illumina HiSeq 2000 Homo sapiens, "GPL570" is for Affymetrix Human Genome U133 Plus 2.0 Array
#' @param phenotype the phenotype of the signature. e.g. "Gene KO", "Ketodiet", "Parkinson disease", "unknown".
#' @param covariates optional but highly recommended. any covariates used in the study. e.g. a signature of aging with covariate "gender".
#' @param direction_type the direction information of the signature.
#' "uni" or "uni-directional" if signature has only one direction or no direction infomation.
#' "bi" or "bi-directional" if signature contains "up" and "down" regulated features.
#' "multi" or "multi-directional" if the signature contains more categories.
#' @param sample_type optional but highly recommended. a cell line or tissue from BRENDA ontology.
#' @param author optional. the author name if the signature is from a published article.
#' @param year optional. the year when the signature was created or published.
#' @param PMID optional. the PubMed ID if the signature is from a published article.
#' @param keywords optional. key words for the signature. examples are "longevity", "perturbation". "drug".
#' @param description optional. free text to describe the signature.
#' @param category_num optional, specifically used for multi-directional signature to specify how many categories or class the signature contains.
#' @param logfc_cutoff optional. log fold change cutoff used to generate the signature, if applicable.
#' @param p_value_cutoff optional. p value cutoff used to generate the signature, if applicable.
#' @param adj_p_cutoff optional. adjusted p-value, e.g. fdr, cutoff used to generate the signature, if applicable.
#' @param score_cutoff optional. score cutoff used to generate the signature, if applicable.
#' @param cutoff_description optional. discription of the cutoff, if applicable.
#' @param ... additional user-defined metadata fields.
#' @return a metadata list to create an OmicSignature R6 object
#' @export
createMetadata <- function(signature_name, organism, phenotype = "unknown",
                           covariates = "none", platform, direction_type,
                           sample_type = NULL, signature_collection = NULL,
                           author = NULL, year = NULL, PMID = NULL,
                           keywords = NULL, description = NULL, category_num = NULL,
                           logfc_cutoff = NULL, p_value_cutoff = NULL,
                           adj_p_cutoff = NULL, score_cutoff = NULL,
                           cutoff_description = NULL, ...) {
  organism <- tools::toTitleCase(organism)

  # check direction type
  direction_type <- direction_type %>%
    tolower() %>%
    dplyr::recode(
      "bi" = "bi-directional",
      "uni" = "uni-directional",
      "multi" = "multi-directional"
    )
  if (!direction_type %in% c("bi-directional", "uni-directional", "multi-directional")) {
    stop("direction_type should be uni-directional, bi-directional or multi-directional.")
  }

  # check if sample_type is a valid BRENDA term
  if (!is.null(sample_type)) {
    tempSampleType <- try(BRENDACurrentName(sample_type), silent = TRUE)
    if (is(tempSampleType, "character")) {
      sample_type <- tempSampleType[2]
    } else {
      warning(paste("sample_type is not a valid BRENDA ontology term. Ignore this message if you intend to input other sample types, such as animal strains.",
        "  Otherwise, please consider using BRENDASearch() function to search for the correct BRENDA ontology term to use.",
        sep = "\n"
      ))
    }
  }

  # check if platform is a valid GPL platform
  platform <- toupper(platform)
  if (!platform %in% GEOplatform$Accession) {
    warning("platform in metadata is not a valid GEO platform accession. Please see `View(GEOplatform)`, or use GEOPlatformSearch() function to search for the correct platform id.")
  }

  # create metadata list
  result <- list(
    "signature_name" = signature_name,
    "signature_collection" = signature_collection,
    "organism" = organism,
    "platform" = platform,
    "direction_type" = direction_type,
    "phenotype" = phenotype,
    "sample_type" = sample_type,
    "covariates" = covariates,
    "author" = author, "year" = year, "PMID" = PMID,
    "keywords" = keywords,
    "description" = description,
    "logfc_cutoff" = logfc_cutoff,
    "p_value_cutoff" = p_value_cutoff,
    "adj_p_cutoff" = adj_p_cutoff,
    "score_cutoff" = score_cutoff,
    "cutoff_description" = cutoff_description
  )
  userDef <- list(...)
  result <- c(result, userDef)

  # remove empty entries
  result <- result[-which(sapply(result, is.null))]

  return(result)
}
