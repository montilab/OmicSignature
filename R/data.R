#' @title BRENDA tissue ontology obo file. using version 2021/10
#' @description BTO:0000000 is set to be "unknown"
#' @export
BRENDA <- readRDS(file.path(system.file("extdata", package = "OmicSignature"), "BRENDA_2021.rds"))

#' @title predefined platform list
#' @description a list of predefined platform names 
#' @export
# GEOplatform <- readRDS(file.path(system.file("extdata", package = "OmicSignature"), "GEOplatform_2024.rds"))
predefined_platforms <- readLines(file.path(system.file("extdata", package = "OmicSignature"), "predefined_platforms.txt"))

#' @title predefined organism list
#' @description a list of predefined organism names 
#' @export
predefined_organisms <- readLines(file.path(system.file("extdata", package = "OmicSignature"), "predefined_organisms.txt"))

#' @title predefined assay type list
#' @description a list of predefined assay type names 
#' @export
predefined_assaytypes <- readLines(file.path(system.file("extdata", package = "OmicSignature"), "predefined_assaytypes.txt"))
