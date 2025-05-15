#' @title template for creating a metadata list for an OmicSignature R6 object
#' @description updated 02/2024
#' @importFrom dplyr recode %>%
#' @param signature_name required. name of the signature.
#' @param signature_collection optional. collection name that the signature belongs to.
#' @param direction_type required. the direction information of the signature.
#' "uni" or "uni-directional" if signature has only one direction or no direction infomation.
#' "bi" or "bi-directional" if signature contains "up" and "down" regulated features.
#' "categorical" if the signature contains more categories.
#' @param assay_type required. e.g. "transcriptomics", "proteomics", "metabolomics", "methylomics", "methylomics", "genetic_variations", "DNA_binding_sites". some common misspell, e.g. "gene", "protein", "metab" will be changed automatically.
#' @param organism required. e.g. "Homo sapiens", "Mus musculus".
#' @param platform optional but highly recommended.
#' @param phenotype optional but highly recommended. e.g. "Gene KO", "Parkinson disease". Use "unknown" or NULL if not applicable.
#' @param sample_type optional but highly recommended. a cell line or tissue from BRENDA ontology.
#' @param covariates optional. e.g. "gender", "age".
#' @param author optional. the author name.
#' @param year optional. the year when the signature was created or published.
#' @param PMID optional. the PubMed ID if the signature is from a published article.
#' @param keywords optional. key words for the signature. examples are "longevity", "perturbation". "drug".
#' @param description optional. free text to describe the signature.
#' @param category_num required when direction_type = "categorical". numeric. indicates how many categories or class the signature contains.
#' @param logfc_cutoff optional. log fold change cutoff used to generate the signature, if applicable.
#' @param p_value_cutoff optional. p value cutoff used to generate the signature, if applicable.
#' @param adj_p_cutoff optional. adjusted p-value, e.g. fdr, cutoff used to generate the signature, if applicable.
#' @param score_cutoff optional. score cutoff used to generate the signature, if applicable.
#' @param cutoff_description optional. discription of the cutoff, if applicable.
#' @param others provide additional user-defined metadata fields as a list. for example, others = list("animal_strain" = "C57BL/6", "lab" = "new_lab").
#' @return a metadata list to create an OmicSignature R6 object.
#' @export
createMetadata <- function(signature_name, organism, phenotype = "unknown", assay_type,
                           covariates = "none", platform = "unknown", direction_type,
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
      "RNA" ~ "transcriptomics",
      "transcript" ~ "transcriptomics",
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
      "dna" ~ "genetic_variations",
      "DNA" ~ "genetic_variations",
      "SNP" ~ "genetic_variations",
      "genetic" ~ "genetic_variations",
      "genetics" ~ "genetic_variations",
      "genetic_variation" ~ "genetic_variations",
      "chip-seq" ~ "DNA_binding_sites",
      "ChIP-seq" ~ "DNA_binding_sites",
      "DNA binding" ~ "DNA_binding_sites",
      "DNA_binding_site" ~ "DNA_binding_sites"
    )
  if (!assay_type %in% c("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "DNA_binding_sites")) {
    assay_type <- "others"
    warning(paste0(
      "assay_type is not one of the following: transcriptomics, proteomics, metabolomics, methylomics, genetic_variations, DNA_binding_sites. \n",
      "Set it to be \"others\". \n"
    ))
  }

  # check if sample_type is a valid BRENDA term
  if (!is.null(sample_type)) {
    if (!BRENDAExistName(sample_type)) {
      warning(paste0(
        "sample_type is not a valid BRENDA ontology term. Ignore this message if intentional. \n",
        "Use searchSampleType() to search for the correct BRENDA ontology term to use. \n"
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
      "Use `searchPlatform()` to search for the correct accession ID to use. \n"
    ))
  }

  # check if organism is valid
  if (is.null(organism) | organism == "unknown") {
    organism <- "unknown"
    warning("Organism information unknown. Ignore this message if intentional. ")
  } else if (!organism %in% predefined_organisms) {
    warning(paste0(
      "Input organism is not in the pre-defined list. Ignore this message if intentional. \n",
      "Use `searchOrganism()` to search for the correct accession ID to use. \n"
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
