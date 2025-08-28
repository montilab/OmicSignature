#### OmicSigObj ####

#' @title OmicSignature R6 object
#' @description a R6 object to store signatures generated from experiments. In cluding metadata, signature, and an optional differential expression analysis result dataframe.
#' updated 08/2025
#' @importFrom R6 R6Class
#' @importFrom dplyr filter select mutate relocate arrange distinct recode bind_rows across everything %>%
#' @importFrom jsonlite toJSON fromJSON
#' @export
OmicSignature <-
  R6::R6Class(
    "OmicSignature",
    #### public of OmS ####
    public = list(
      #' @description
      #' Create a new OmicSignature object
      #' @param metadata required. a list. See `createMetadata` for more information
      #' @param signature required. a vector, or a dataframe including columns: "probe_id", "feature_name" and "group_label", and an optional column "score"
      #' @param difexp optional
      #' @param print_message use TRUE if want to see all messages printed
      #' @export
      initialize = function(metadata, signature, difexp = NULL, print_message = FALSE) {
        ## if probe_id is not provided, setup numeric counter as probe_id
        if (!is.null(difexp)) {
          if (!"probe_id" %in% colnames(difexp)) {
            difexp <- difexp %>% dplyr::mutate(probe_id = paste0("feature_", seq(nrow(difexp))), .before = everything())
          }
          signature$probe_id <- NULL
          signature <- merge(
            x = difexp[, c("probe_id", "feature_name")],
            y = signature, by = "feature_name", all.x = FALSE, all.y = TRUE
          )
          signature <- signature %>% dplyr::relocate(probe_id, .before = everything())
        } else {
          signature <- signature %>%
            dplyr::mutate(probe_id = seq(paste0("feature_", nrow(signature))), .before = everything())
        }

        private$.metadata <- private$checkMetadata(metadata, v = print_message)
        private$.signature <- private$checkSignature(signature, signatureType = metadata$direction_type, v = print_message)
        if (!is.null(difexp)) {
          difexp <- private$checkDifexp(difexp, v = print_message)
          private$.difexp <- difexp
          if (!all(signature$probe_id %in% difexp$probe_id)) {
            stop("Some probe_id in the signature are not included in the probe_id in the difexp.")
          }
          if (!all(signature$feature_name %in% difexp$feature_name)) {
            stop("Some feature_name in the signature are not included in the feature_name in the difexp.")
          }
        }
        cat(paste("  [Success] OmicSignature object", private$.metadata$signature_name, "created.\n"))
      },
      #' @description
      #' Print an OmicSignature object
      #' @export
      print = function() {
        cat("Signature Object: \n")
        cat("  Metadata: \n")
        sh <- mapply(function(k, v) {
          cat("   ", k, "=", paste(v, collapse = ", "), "\n")
        }, names(private$.metadata), private$.metadata)
        if (!is.null(private$.metadata$others)) {
          cat("  Metadata user defined fields: \n")
          sh <- mapply(function(k, v) {
            cat("   ", k, "=", paste(v, collapse = ", "), "\n")
          }, names(private$.metadata$others), private$.metadata$others)
        }
        cat("  Signature: \n")
        if (private$.metadata$direction_type %in% c("bi-directional", "categorical")) {
          sh <- mapply(
            function(k, v) {
              cat("    ", k, " (", v, ")", "\n", sep = "")
            }, names(summary(private$.signature$group_label)),
            summary(private$.signature$group_label)
          )
        } else {
          cat("    Length (", nrow(private$.signature), ")\n")
        }
        cat("  Differential Expression Data: \n")
        cat("    ", nrow(private$.difexp), " x ", ncol(private$.difexp), "\n", sep = "")
        invisible(self)
      },
      #' @param conditions conditions for new signatures
      #' @return a dataframe of new signatures
      #' @export
      extractSignature = function(conditions) {
        if (is.null(private$.difexp)) {
          stop("Error: Difexp data frame not found.")
        }
        v <- rlang::parse_exprs(conditions)

        direction_type <- private$.metadata$direction_type
        difexp <- private$.difexp
        res <- difexp %>% dplyr::filter(!!!v)

        if ("score" %in% colnames(difexp)) {
          if (direction_type == "uni-directional") {
            res <- res %>%
              dplyr::select(probe_id, feature_name, score) %>%
              dplyr::filter(score != "") %>%
              dplyr::arrange(desc(abs(score)))
          } else if (direction_type == "bi-directional") {
            res <- res %>%
              dplyr::select(probe_id, feature_name, score, group_label) %>%
              dplyr::filter(score != "") %>%
              dplyr::arrange(desc(abs(score)))
          } else if (direction_type == "categorical") {
            res <- res %>%
              dplyr::select(probe_id, feature_name, score, group_label) %>%
              dplyr::filter(score != "", ) %>%
              dplyr::arrange(desc(abs(score)))
          }
        } else {
          if (direction_type == "uni-directional") {
            res <- res %>%
              dplyr::select(probe_id, feature_name)
          } else {
            res <- res %>%
              dplyr::filter(!!!v) %>%
              dplyr::select(probe_id, feature_name, group_label)
          }
        }

        res <- res %>%
          dplyr::filter(feature_name != "", complete.cases(across(everything()))) %>%
          dplyr::distinct(feature_name, .keep_all = TRUE) 

        return(res)
      }
    ),

    #### active of OmS ####
    active = list(
      #' @field metadata a list to describe the metadata
      metadata = function(value, print_message = FALSE) {
        if (missing(value)) {
          private$.metadata
        } else {
          private$.metadata <- private$checkMetadata(value, print_message)
        }
      },
      #' @field signature a dataframe contains probe_id, feature_name, score (optional) and group_label (optional)
      signature = function(value, print_message = FALSE) {
        if (missing(value)) {
          private$.signature
        } else {
          private$.signature <- private$checkSignature(value, print_message)
        }
      },
      #' @field difexp a dataframe for differential expression result
      difexp = function(value, print_message = FALSE) {
        if (missing(value)) {
          private$.difexp
        } else {
          private$.difexp <- private$checkDifexp(value, print_message)
        }
      },
      #' @field removeDifexp a function to remove difexp from the object
      removeDifexp = function(check = "no") {
        if (tolower(check) == "yes") {
          private$.difexp <- NULL
          cat("difexp has been deleted.\n")
        } else {
          cat("use removeDifexp(\"yes\") to delete difexp. \n")
        }
      }
    ),

    #### private of OmS ####
    private = list(
      .metadata = NULL,
      .signature = NULL,
      .difexp = NULL,
      verbose = function(v, ...) {
        if (v) cat(...)
      },
      checkDifexp = function(difexp, v = FALSE) {
        if (is.null(difexp)) stop("Please use build-in function $removeDifexp.")
        if (is(difexp, "OmicSignature")) difexp <- difexp$difexp
        if (is(difexp, "matrix")) difexp <- as.data.frame(difexp)
        if (!is(difexp, "data.frame")) stop("difexp must be a data frame. ")

        ## check if it's empty:
        if (nrow(difexp) == 0) stop("difexp is empty. ")

        ## check column names:
        difexpColRequired <- c("probe_id", "feature_name", "score", "group_label")
        ## require any of p_value, q_value, or adj_p
        exist_p_columns <- intersect(colnames(difexp), c("p_value", "q_value", "adj_p"))
        if (length(exist_p_columns) > 0) {
          difexpColRequired <- c(difexpColRequired, exist_p_columns)
        } else {
          stop("difexp requires at least one of the following columns: p_value, q_value, adj_p.")
        }
        difexpColMissing <- setdiff(difexpColRequired, colnames(difexp))
        if (length(difexpColMissing) > 0) {
          stop("Differential expression result (difexp) does not contain required column(s): ",
            paste(difexpColMissing, collapse = ", "), ".",
            sep = ""
          )
        }

        ## check column type:
        ## "logfc", "score", "p_value", "adj_p" should be numerical
        for (difexpColNumeric in c("logfc", "score", "p_value", "adj_p", "q_value", "aveexpr", "mean")) {
          if (difexpColNumeric %in% colnames(difexp)) {
            if (!is(difexp[, difexpColNumeric], "numeric")) {
              stop(paste("difexp:", difexpColNumeric, "is not numeric."))
            }
          }
        }
        ## "feature_name" should be character
        if ("feature_name" %in% colnames(difexp)) {
          if (!is(difexp$feature_name, "character")) {
            stop("difexp: feature_name is not character.")
          }
        }

        ## "group_label" should be factor
        if ("group_label" %in% colnames(difexp)) {
          if (!is(difexp$group_label, "factor")) {
            stop("difexp: group_label is not factor.")
          }
        }
        private$verbose(v, "  [Success] difexp is valid. \n")
        return(difexp)
      },
      checkMetadata = function(metadata, v = FALSE) {
        stopifnot(is(metadata, "list"))

        # check required metadata fields
        metadataRequired <- c("signature_name", "phenotype", "organism", "direction_type", "assay_type")
        metadataMissing <- setdiff(metadataRequired, names(metadata))
        private$verbose(v, paste("  --Required attributes for metadata: ",
          paste(metadataRequired, collapse = ", "), " --\n",
          sep = ""
        ))
        if (length(metadataMissing) != 0) {
          stop("Metadata does not contain attribute(s): ",
            paste(metadataMissing, collapse = ", "), ".",
            sep = ""
          )
        }

        # check direction_type
        if (!metadata$direction_type %in% c("uni-directional", "bi-directional", "categorical")) {
          stop("direction_type must be uni-directional, bi-directional, or categorical. ")
        }

        # check assay_type
        if (!metadata$assay_type %in% c("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "DNA_binding_sites")) {
          metadata$assay_type <- "other"
          warning("assay_type is not one of the commonly used term: transcriptomics, proteomics, metabolomics, methylomics, genetic_variations, DNA_binding_sites. Set it to be \"other\". ")
        }

        # check covariates
        if (is.null(metadata$covariates)) metadata$covariates <- "none"

        # check phenotype
        if (is.null(metadata$phenotype)) {
          metadata$phenotype <- "unknown"
          warning("Phenotype information unknown. ")
        }

        # check if sample_type is a valid BRENDA term
        if (is.null(metadata$sample_type)) metadata$sample_type <- "unknown"
        if (!BRENDAExistName(metadata$sample_type)) {
          metadata$sample_type <- "unknown"
          warning("sample_type is not a valid BRENDA ontology term. Set to be unknown.")
        }

        # check if organism is in pre-defined list
        if (is.null(metadata$organism)) metadata$organism <- "unknown"
        if (!metadata$organism %in% predefined_organisms) {
          warning("Organism is not in the predefined list. Ignore this message if intentional.")
        }

        # check if platform is in pre-defined list
        if (is.null(metadata$platform)) metadata$platform <- "unknown"
        if (!metadata$platform %in% predefined_platforms) {
          warning("Platform is not in the predefined list. Ignore this message if intentional.")
        }

        private$verbose(v, "  [Success] Metadata is saved. \n")
        metadata <- metadata[order(names(metadata))]
        return(metadata)
      },
      checkSignature = function(input, signatureType = NULL, category_num = 0, v = FALSE) {
        ## category_num is used for categorical signature

        if (is(input, "OmicSignature")) {
          signature <- input$signature
          signatureType <- input$metadata$direction_type
          if (signatureType == "categorical") {
            if (is.null(input$metadata$category_num)) {
              stop("Signature is specified as categorical, but category_num not specified. This is the number of categories or group analyzed. ")
            }
          }
        } else if (is.vector(input)) {
          if (signatureType == "bi-directional") {
            signature <- signatureVecToDF(input, bi_directional = TRUE)
          } else if (signatureType == "uni-directional") {
            signature <- signatureVecToDF(input, bi_directional = FALSE)
          } else {
            stop("Please provide signature as a dataframe. ")
          }
        } else if (is.data.frame(input)) {
          signature <- input
        } else {
          stop("Signature is not valid.")
        }
        remove(input)

        ## starting this point, signature should be a dataframe
        if (nrow(signature) == 0) {
          stop("Signature is empty.")
        }

        ## check signatureType:
        if (is.null(signatureType)) {
          stop("Signature type not specified. It needs to be uni-directional, bi-directional or categorical.")
        }
        if (!signatureType %in% c("uni-directional", "bi-directional", "categorical")) {
          stop("Signature type not specified. It needs to be uni-directional, bi-directional or categorical.")
        }

        ## standardize content and column names:
        signature <- standardizeSigDF(signature)

        ## check column names
        signatureColRequired <- c("probe_id", "feature_name")
        if (signatureType != "uni-directional") {
          signatureColRequired <- c(signatureColRequired, "group_label")
        }
        signatureColMissing <- setdiff(signatureColRequired, colnames(signature))
        if (length(signatureColMissing) > 0) {
          stop("Signature must contain the following columns: ", paste(signatureColRequired, collapse = ", "))
        }

        ## check column types:
        if (!is(signature$feature_name, "character")) {
          stop("signature: feature_name is not character.")
        }

        if (signatureType != "uni-directional") {
          if (!is(signature$group_label, "factor")) {
            stop("signature: group_label is not factor.")
          }
        }
        private$verbose(v, "  [Success] Signature is valid. \n")
        return(signature)
      }
    )
  )

#### OmicSigCollection ####
#' @title OmicSignatureCollection R6 object
#' @description a R6 object to store a collection of OmicSignature objects.
#' In cluding metadata, OmicSigList which is a list of OmicSignature object.
#' @importFrom R6 R6Class
#' @importFrom dplyr filter arrange mutate %>%
#' @export
OmicSignatureCollection <- R6Class(
  "OmicSignatureCollection",

  #### public of OmSC ####
  public = list(
    #' @description
    #' Create an OmicSignatureCollection object
    #' @param metadata required, must be a list
    #' @param OmicSigList required, a list of OmicSignature R6 objects
    #' @param print_message use TRUE if want to see all messages printed
    #' @export
    initialize = function(metadata, OmicSigList, print_message = FALSE) {
      private$.metadata <- private$checkCollectionMetadata(metadata, v = print_message)
      private$.OmicSigList <- private$checkCollectionOmicSigList(OmicSigList, v = print_message)
      # a list of OmicSig Obj, contains everything
      names(private$.OmicSigList) <- mapply(function(k) {
        k$metadata$signature_name
      }, private$.OmicSigList)
      cat(paste(
        "  [Success] OmicSignature Collection",
        private$.metadata$collection_name, "created.\n"
      ))
    },
    #' @description
    #' Print an OmicSignatureCollection object
    #' @export
    print = function() {
      cat("Signature Collection: \n")
      cat("  Metadata: \n")
      sh <- mapply(function(k, v) {
        cat("   ", k, "=", v, "\n")
      }, names(private$.metadata), private$.metadata)
      cat("  OmicSignature Objects: \n")
      cat("   ", paste(names(private$.OmicSigList), collapse = "\n    "))
      cat("\n  Available difexp columns: \n")
      sh <- mapply(function(k) {
        cat("   ", k$metadata$signature_name, " (", paste(
          {
            if (is.null(k$difexp)) {
              "* no difexp found *"
            } else {
              colnames(k$difexp)
            }
          },
          collapse = ", "
        ), ") \n")
      }, private$.OmicSigList)
      invisible(self)
    },
    #' @param conditions conditions for new signatures
    #' @param bind use TRUE to return all results in a single dataframe. Otherwise, will return a list contains the result of each OmicSignature individually
    #' @return a dataframe or a list of new signatures
    #' @export
    extractSignature = function(conditions, bind = TRUE) {
      sigDF <- lapply(private$.OmicSigList, function(x) {
        try_temp <- try(x$extractSignature(conditions), silent = TRUE)
        if (is(try_temp, "try-error")) {
          cat("  Warning: ", x$metadata$signature_name, "does not have a valid difexp data frame. \n")
          try_temp <- NULL
        }
        return(try_temp)
      })
      if (bind) {
        sigDF <- dplyr::bind_rows(sigDF, .id = "sig_name")
        if ("score" %in% colnames(sigDF)) sigDF <- sigDF %>% dplyr::arrange(desc(abs(score)))
      }
      return(sigDF)
    },
    #' @param only_shared use TRUE to only print the shared metadata fields in the OmicSignatures
    #' @return a dataframe of the summary of the metadata
    #' @export
    metadataSummary = function(only_shared = TRUE) {
      if (only_shared == TRUE) {
        col <- Reduce(intersect, sapply(private$.OmicSigList, function(x) {
          names(x$metadata)
        }, simplify = F))
      } else {
        col <- Reduce(union, sapply(private$.OmicSigList, function(x) {
          names(x$metadata)
        }, simplify = F))
      }
      res <- sapply(private$.OmicSigList, function(x) {
        x$metadata[col]
      }, simplify = T)
      rownames(res) <- col
      colnames(res) <- sapply(private$.OmicSigList, function(x) {
        x$metadata$signature_name
      }, simplify = T)
      return(res)
    }
  ),

  #### active of OmSC ####
  active = list(
    #' @field metadata a list to describe the metadata
    metadata = function(value, print_message = FALSE) {
      if (missing(value)) {
        private$.metadata
      } else {
        private$.metadata <- private$checkCollectionMetadata(value, print_message)
      }
    },
    #' @field OmicSigList a list of OmicSignature object(s)
    OmicSigList = function(value, print_message = FALSE) {
      if (missing(value)) {
        private$.OmicSigList
      } else {
        private$.OmicSigList <- private$checkCollectionOmicSigList(value, print_message)
      }
    }
  ),

  #### private of OmSC ####
  private = list(
    .metadata = NULL,
    .OmicSigList = NULL,
    verbose = function(v, ...) {
      if (v) cat(...)
    },
    checkCollectionMetadata = function(metadata, v = FALSE) {
      ## metadata should be a list with required attributes
      if (class(metadata)[1] == "OmicSignatureCollection") {
        metadata <- metadata$metadata
      }
      stopifnot(is(metadata, "list"))
      metadataRequired <- c(
        "collection_name", "description"
      )
      private$verbose(v, paste("  --Required attributes for metadata: ",
        paste(metadataRequired, collapse = ", "), " --\n",
        sep = ""
      ))
      metadataMissing <- setdiff(metadataRequired, names(metadata))
      if (length(metadataMissing) != 0) {
        stop(
          "Metadata for this Collection does not contain attribute(s): ",
          paste(metadataMissing, collapse = ", "),
          ". Please check your input."
        )
      }
      metadata <- metadata[order(names(metadata))]
      private$verbose(v, "  [Success] Metadata is saved. \n")
      return(metadata)
    },
    checkCollectionOmicSigList = function(OmicSigList, v = FALSE) {
      ## OmicSigList should be a list of OmicSig object
      if (class(OmicSigList)[1] == "OmicSignatureCollection") {
        OmicSigList <- OmicSigList$OmicSigList
      }
      stopifnot(is(OmicSigList, "list"))
      stopifnot(length(OmicSigList) != 0)

      for (OmicObj in OmicSigList) {
        if (class(OmicObj)[1] != "OmicSignature") {
          stop("Some elements in OmicSigList are not OmicSignature objects.")
        }
      }
      return(OmicSigList)
    }
  )
)
