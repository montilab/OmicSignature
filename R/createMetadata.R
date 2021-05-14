#' @title template for creating a metadata list for an OmicSignature R6 object
#' updated 05/2021
#' @importFrom dplyr recode %>%
#' @param signature_name required. name of the signature.
#' @param signature_collection optional. collection name that the signature belongs to.
#' @param direction_type required. the direction information of the signature.
#' "uni" or "uni-directional" if signature has only one direction or no direction infomation.
#' "bi" or "bi-directional" if signature contains "up" and "down" regulated features.
#' "multi" or "multi-directional" if the signature contains more categories.
#' @param organism required. e.g. "Homo Sapiens", "Mus Musculus".
#' @param platform optional but highly recommended. GEO platform name. e.g. "GPL11154" is for Illumina HiSeq 2000 Homo sapiens. Use "GPLXXXXX" or NULL if not available.
#' @param phenotype optional but highly recommended. e.g. "Gene KO", "Parkinson disease". Use "unknown" or NULL if not applicable.
#' @param sample_type optional but highly recommended. a cell line or tissue from BRENDA ontology.
#' @param covariates optional. e.g. a signature of aging may have covariate "gender".
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
#' @return a metadata list to create an OmicSignature R6 object.
#' @export
createMetadata <- function(signature_name, organism, phenotype = "unknown",
                           covariates = "none", platform = "GPLXXXXX", direction_type,
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
      warning(paste("sample_type is not a valid BRENDA ontology term. Ignore this message if it's intentional. ",
        "  Otherwise, please consider using BRENDASearch() function to search for the correct BRENDA ontology term to use.",
        sep = "\n"
      ))
    }
  }

  # check if platform is a valid GPL platform
  platform <- toupper(platform)
  if (is.null(platform) | platform == "GPLXXXXX") {
    platform <- "GPLXXXXX"
    warning("Platform information unknown. Ignore this message if it's intentional. ")
  } else if (!platform %in% GEOplatform$Accession) {
    warning(paste("Input platform is not a valid GEO platform accession ID. Ignore this message if it's intentional. ",
      "  Otherwise, please see `View(GEOplatform)`, or use `GEOPlatformSearch()` to search for the correct id. ",
      sep = "\n"
    ))
  }

  # check phenotype
  if (is.null(phenotype) | phenotype == "unknown") {
    phenotype <- "unknown"
    warning("Phenotype information unknown. Ignore this message if it's intentional. ")
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
