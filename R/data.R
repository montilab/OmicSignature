#' @title BRENDA tissue ontology obo file. using version 2021/10
#' @description BTO:0000000 is set to be "unknown" 
#' @export
BRENDA <- readRDS(file.path(system.file("extdata", package = "OmicSignature"), "BRENDA_2021.rds"))

#' @title GEO platform names. updated 02/2024
#' @description GPLXXXXX is set to be "unknown" 
#' @export
GEOplatform <- readRDS(file.path(system.file("extdata", package = "OmicSignature"), "GEOplatform_2024.rds"))
