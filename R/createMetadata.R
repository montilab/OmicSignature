#' @title template for creating a metadata list for an OmicSignature R6 object
#' @description updated 10/2025
#' @importFrom dplyr recode %>%
#' @param signature_name required. name of the signature.
#' @param signature_collection optional. the collection name that the signature belongs to.
#' @param direction_type required. the direction information of the signature.
#' "uni" or "uni-directional" if the signature is derived from one category.
#' "bi" or "bi-directional" if the signature is derived from group A vs group B, or it contains "up" and "down" regulated features for a continuous phenotype.
#' "categorical" if the signature is derived from comparisons between multiple groups, e.g. A vs B vs C.
#' @param assay_type required. must be one of the following: "transcriptomics", "proteomics", "metabolomics", "methylomics", "methylomics", "genetic_variations", "DNA_binding_sites", or "others". some of the common misspells, e.g. "gene", "protein" and "metab" will be changed automatically.
#' @param organism required. e.g. "Homo sapiens", "Mus musculus".
#' @param platform optional but highly recommended. input as a single string. e.g. "transcriptomics by single-cell RNA-seq".
#' @param phenotype optional but highly recommended. input as a single string. e.g. "Gene KO", "Parkinson disease". Use "unknown" or NULL if not applicable.
#' @param sample_type optional but highly recommended. a cell line or tissue from BRENDA ontology.
#' @param covariates optional. input covariates as a single string, and separate multiple covariates using comma, e.g. "age, gender".
#' @param author optional. the author's name.
#' @param year optional. a single-length numeric value. the year when the signature was created or published.
#' @param PMID optional. a single-length character value. the PubMed ID if the signature is from a published article.
#' @param keywords optional. key words for the signature. input as a multi-length character vector, e.g. c("longevity", "perturbation", "health").
#' @param description optional. free text to describe the signature. input as a single-length string. the character limit (including all spaces and symbols) is 65,535.
#' @param category_num required when direction_type = "categorical". numeric. a number indicates how many categories or class the signature contains.
#' @param logfc_cutoff optional. a single-length numeric value. log fold change cutoff used to generate the signature, if applicable.
#' @param p_value_cutoff optional. a single-length numeric value. p value cutoff used to generate the signature, if applicable.
#' @param adj_p_cutoff optional. a single-length numeric value. adjusted p-value, e.g. fdr, cutoff used to generate the signature, if applicable.
#' @param score_cutoff optional. a single-length numeric value. score cutoff used to generate the signature, if applicable.
#' @param cutoff_description optional. description of the cutoff, if applicable.
#' @param others provide additional user-defined metadata fields as a list. for example, others = list("animal_strain" = "C57BL/6", "lab" = "new_lab").
#' @return a metadata list to create an OmicSignature R6 object.
#' @export
createMetadata <- function(signature_name, organism, phenotype = "unknown", assay_type,
                           covariates = NULL, platform = "unknown", direction_type,
                           sample_type = NULL, signature_collection = NULL,
                           author = NULL, year = NULL, PMID = NULL,
                           keywords = NULL, description = NULL, category_num = NULL,
                           logfc_cutoff = NULL, p_value_cutoff = NULL,
                           adj_p_cutoff = NULL, score_cutoff = NULL,
                           cutoff_description = NULL, others = NULL) {
  # check sig direction type
  direction_type <- direction_type %>%
    tolower() %>%
    dplyr::case_match(
      .default = direction_type,
      "bi" ~ "bi-directional",
      "uni" ~ "uni-directional"
    )
  if (!direction_type %in% c("bi-directional", "uni-directional", "categorical")) {
    stop("direction_type should be uni-directional, bi-directional or categorical.")
  }

  # check assey type
  assay_type <- assay_type %>%
    tolower() %>%
    dplyr::case_match(
      .default = assay_type,
      "rna" ~ "transcriptomics",
      "gene" ~ "transcriptomics",
      "transcript" ~ "transcriptomics",
      "transcripts" ~ "transcriptomics",
      "transcriptomic" ~ "transcriptomics",
      "protein" ~ "proteomics",
      "proteomic" ~ "proteomics",
      "metab" ~ "metabolomics",
      "metabolite" ~ "metabolomics",
      "metabolites" ~ "metabolomics",
      "metabolome" ~ "metabolomics",
      "metabolomic" ~ "metabolomics",
      "methyl" ~ "methylomics",
      "methylation" ~ "methylomics",
      "methylomic" ~ "methylomics",
      "snp" ~ "genetic_variations",
      "snps" ~ "genetic_variations",
      "genetic" ~ "genetic_variations",
      "genetics" ~ "genetic_variations",
      "genetic_variation" ~ "genetic_variations",
      "chip-seq" ~ "DNA_binding_sites",
      "chip seq" ~ "DNA_binding_sites",
      "dna binding" ~ "DNA_binding_sites",
      "dna_binding_site" ~ "DNA_binding_sites"
    )
  if (!assay_type %in% predefined_assaytypes) {
    warning("assay_type is not one of the commonly used assay_type terms. Ignore this message if intentional.\n")
  }

  # check if sample_type is a valid BRENDA term
  if (!is.null(sample_type)) {
    if (!BRENDAExistName(sample_type)) {
      warning(paste0(
        "sample_type is not a valid BRENDA ontology term. Ignore this message if intentional. \n",
        "Use OmicS_searchSampleType() to search for the correct BRENDA ontology term to use. \n"
      ))
    }
  }

  # check if platform is valid
  if (is.null(platform) | platform == "unknown") {
    platform <- "unknown"
    warning("Platform information unknown. Ignore this message if intentional. ")
  } else if (!platform %in% predefined_platforms) {
    warning(paste0(
      "Input platform is not in the pre-defined list. Ignore this message if intentional. \n",
      "Use `OmicS_searchPlatform()` to search for the correct accession ID to use. \n"
    ))
  }

  # check if organism is valid
  if (is.null(organism) | organism == "unknown") {
    organism <- "unknown"
    warning("Organism information unknown. Ignore this message if intentional. ")
  } else if (!organism %in% predefined_organisms) {
    warning(paste0(
      "Input organism is not in the pre-defined list. Ignore this message if intentional. \n",
      "Use `OmicS_searchOrganism()` to search for the correct accession ID to use. \n"
    ))
  }

  # check phenotype
  if (is.null(phenotype) | phenotype == "unknown") {
    phenotype <- "unknown"
    warning("Phenotype information unknown. Ignore this message if intentional. ")
  }

  # check others, i.e. user defined fields
  if (!is.null(others) & !is.list(others)) {
    stop("\"others\" must be a list. see description for details.")
  }

  # create metadata list
  result <- list(
    "signature_name" = signature_name,
    "signature_collection" = signature_collection,
    "direction_type" = direction_type,
    "assay_type" = assay_type,
    "organism" = organism,
    "platform" = platform,
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
    "cutoff_description" = cutoff_description,
    "others" = others
  )
  if (direction_type == "categorical") {
    if (!is.null(category_num)) {
      result$category_num <- category_num
    } else {
      stop("Error: signature is categorical but category_num is not specified. \n")
    }
  }

  # remove empty entries
  result <- result[-which(sapply(result, is.null))]

  return(result)
}
