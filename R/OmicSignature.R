#### OmicSigObj ####

#' @title OmicSignature R6 object
#' @description a R6 object to store signatures generated from experiments. In cluding metadata, signature, and an optional differential expression analysis result matrix.
#' updated 04/2024
#' @importFrom R6 R6Class
#' @importFrom dplyr filter select mutate arrange distinct recode %>%
#' @importFrom jsonlite toJSON fromJSON
#' @export
OmicSignature <-
  R6::R6Class(
    "OmicSignature",
    #### public of OmicSig ####
    public = list(
      #' @description
      #' Create a new OmicSignature object
      #' @param metadata required. must be a list. See `createMetadata` for more information
      #' @param signature required. must be a vector, or a dataframe with column "id", "symbol" and "direction", and optional column "score"
      #' @param difexp optional
      #' @param print_message use TRUE if want to see all messages printed
      #' @export
      initialize = function(metadata, signature, difexp = NULL, print_message = FALSE) {
        private$.metadata <- private$checkMetadata(metadata, v = print_message)
        private$.signature <- private$checkSignature(signature, signatureType = metadata$direction_type, v = print_message)
        if (!is.null(difexp)) {
          difexp <- private$checkDifexp(difexp, v = print_message)
          private$.difexp <- difexp
          if (!all(signature$id %in% difexp$id)) {
            stop(paste(
              "Some features in signature$id are not included in difexp$id. Examples:",
              paste(head(setdiff(signature$id, difexp$id)), collapse = " ")
            ))
          }
          if (!all(signature$symbol %in% difexp$symbol)) {
            stop(paste(
              "Some features in signature$symbol are not included in difexp$symbol. Examples:",
              paste(head(setdiff(signature$symbol, difexp$symbol)), collapse = " ")
            ))
          }
        }
        cat(paste(
          "  [Success] OmicSignature object",
          private$.metadata$signature_name, "created.\n"
        ))
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
        sh <- mapply(
          function(k, v) {
            cat("    ", k, " (", v, ")", "\n", sep = "")
          }, names(summary(private$.signature$direction)),
          summary(private$.signature$direction)
        )
        cat("  Differential Expression Data: \n")
        cat("    ", nrow(private$.difexp), " x ", ncol(private$.difexp), "\n", sep = "")
        invisible(self)
      },
      #' @param conditions conditions for new signatures
      #' @return a dataframe of new signatures
      #' @export
      extractSignature = function(conditions) {
        if (is.null(private$.difexp)) {
          stop("Error: Difexp matrix not found in OmicSignature object.")
        }
        v <- rlang::parse_exprs(conditions)
        res <- private$.difexp %>%
          dplyr::filter(!!!v) %>%
          dplyr::filter(symbol != "" & score != "") %>%
          dplyr::select(id, symbol, score) %>%
          dplyr::mutate(direction = ifelse(score < 0, "-", "+")) %>%
          dplyr::arrange(desc(abs(score))) %>%
          dplyr::distinct(symbol, .keep_all = TRUE)
        res <- res[complete.cases(res), ]
        return(res)
      }
    ),

    #### active of OmicSig ####
    active = list(
      #' @field metadata a list to describe the metadata
      metadata = function(value, print_message = FALSE) {
        if (missing(value)) {
          private$.metadata
        } else {
          private$.metadata <- private$checkMetadata(value, print_message)
        }
      },
      #' @field signature a dataframe contains id, symbol, score (optional) and direction (optional)
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
      #' @field deleteDifexp a function to delete difexp from the object
      deleteDifexp = function(x) {
        private$.difexp <- NULL
        cat("difexp has been deleted.\n")
      }
    ),

    #### private of OmicSig ####
    private = list(
      .metadata = NULL,
      .signature = NULL,
      .difexp = NULL,
      verbose = function(v, ...) {
        if (v) cat(...)
      },
      checkDifexp = function(difexp, v = FALSE) {
        if (is.null(difexp)) {
          stop("You are trying to assign difexp to NULL. To prevent false operation, please use build-in function $deleteDifexp.")
        }
        if (is(difexp, "OmicSignature")) {
          difexp <- difexp$difexp
        }
        stopifnot(is(difexp, "data.frame"))

        ## check if it's empty:
        if (nrow(difexp) == 0) {
          stop("difexp is empty. ")
        }

        ## check column names:
        ## if id is not included, then setup numeric counter as id
        if (!("id" %in% colnames(difexp))) {
          difexp$id <- c(1:nrow(difexp))
        }
        ## require any of adj_p, p_value, or q_value
        exist_p_columns <- intersect(colnames(difexp), c("adj_p", "p_value", "q_value"))
        if (length(exist_p_columns) > 0) {
          difexpColRequired <- c("id", "symbol", "score", exist_p_columns)
        } else {
          stop("Columns in difexp need to contain at least one of the following: p_value, q_value, adj_p.")
        }

        difexpColMissing <- setdiff(difexpColRequired, colnames(difexp))
        difexpColAdditional <- setdiff(colnames(difexp), difexpColRequired)

        if (length(difexpColMissing) > 0) {
          stop("Differential Matrix (difexp) does not contain required column(s): ",
            paste(difexpColMissing, collapse = ", "), ".",
            sep = ""
          )
        }
        if (length(difexpColAdditional) > 0) {
          private$verbose(v, paste("  difexp: additional columns found: ",
            paste(difexpColAdditional, collapse = ", "), ". \n",
            sep = ""
          ))
        }

        ## check column type:
        ## "logfc","score","p_value","adj_p" should be numerical
        for (difexpColNumeric in c("logfc", "score", "p_value", "adj_p", "q_value", "aveexpr")) {
          if (difexpColNumeric %in% colnames(difexp)) {
            if (!is(difexp[, difexpColNumeric], "numeric")) {
              stop(paste("difexp:", difexpColNumeric, "is not numeric."))
            }
          }
        }
        ## "symbol" should be character
        if ("symbol" %in% colnames(difexp)) {
          if (!is(difexp$symbol, "character") && !is(difexp$symbol, "factor")) {
            stop("difexp: symbol is not character.")
          }
        }
        private$verbose(v, "  [Success] difexp matrix is valid. \n")
        return(difexp)
      },
      checkMetadata = function(metadata, v = FALSE) {
        stopifnot(is(metadata, "list"))

        # check required metadata fields
        metadataRequired <- c(
          "signature_name", "organism", "direction_type", "assay_type"
        )
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
        if (!metadata$direction_type %in% c("uni-directional", "bi-directional", "multiple")) {
          stop("direction_type must be uni-directional, bi-directional, or multiple. ")
        }

        # check assay_type
        if (!metadata$assay_type %in% c("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "DNA_binding_sites")) {
          metadata$assay_type <- "others"
          warning("assay_type is not one of the commonly used term: transcriptomics, proteomics, metabolomics, methylomics, genetic_variations, DNA_binding_sites. Set it to be \"others\". ")
        }

        # check covariates
        if (is.null(metadata$covariates)) {
          metadata$covariates <- "none"
        }

        # check phenotype
        if (is.null(metadata$phenotype)) {
          metadata$phenotype <- "unknown"
          warning("Phenotype information unknown. ")
        }

        # check if sample_type is a valid BRENDA term
        if (is.null(metadata$sample_type)) {
          metadata$sample_type <- "unknown"
        }
        if (!BRENDAExistName(metadata$sample_type) | metadata$sample_type == "unknown") {
          warning(paste(
            "sample_type is missing or is not a valid BRENDA ontology term. Set to be unknown.",
            "If this is a mistake, use BRENDASearch() to search for the correct term to use.",
            sep = "\n"
          ))
        }

        # check if platform is a valid GPL platform
        if (is.null(metadata$platform)) {
          metadata$platform <- "GPLXXXXX"
        }
        if (!metadata$platform %in% GEOplatform$Accession | metadata$platform == "GPLXXXXX") {
          warning(paste(
            "platform is missing or not a valid GEO platform accession ID. Set to be GPLXXXXX.",
            "If this is a mistake, use `GEOSearch()` to search for the correct assession ID to use. ",
            sep = "\n"
          ))
        }
        private$verbose(v, "  [Success] Metadata is saved. \n")
        metadata <- metadata[order(names(metadata))]
        return(metadata)
      },
      checkSignature = function(input, signatureType = NULL, category_num = 0, v = FALSE) {
        ## category_num is used for multiple category signature

        if (is(input, "OmicSignature")) {
          signature <- input$signature
          signatureType <- input$metadata$direction_type
          if (signatureType == "multiple") {
            if (is.null(input$metadata$category_num)) {
              stop("Signature is specified as multiple, but category_num not specified. This is the number of categories or group analyzed. ")
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
          remove(input)
        } else if (is.data.frame(input)) {
          signature <- input
          remove(input)
        } else {
          stop(paste(
            "Signature not found in OmicSignature object",
            "or signature is not a vector or dataframe. Or, if you are trying to modify",
            "the signature, the new signature you are inputting is not valid."
          ))
        }

        ## starting this point, signature should be a dataframe
        if (nrow(signature) == 0) {
          stop("Signature is empty.")
        }

        ## standardize content and column names:
        signature <- standardizeSigDF(signature)
        signature <- replaceSigCol(signature)

        ## check columns:
        if (!c("symbol") %in% colnames(signature)) {
          stop("Signature dataframe does not contain \"symbol\" column.")
        }
        if (!c("id") %in% colnames(signature)) {
          signature <- signature %>%
            dplyr::mutate(id = symbol, .before = everything())
        }
        if (!c("score") %in% colnames(signature)) {
          signature$score <- NA
          warning("\"score\" is missing in the signature dataframe. Ignore this message if intentional. \n")
        }

        ## check if the direction match with signature type:
        if (is.null(signatureType)) {
          stop("Signature type not specified. It needs to be uni-directional, bi-directional or multiple.")
        }
        if (!signatureType %in% c("uni-directional", "bi-directional", "multiple")) {
          stop("Signature type not specified. It needs to be uni-directional, bi-directional or multiple.")
        }

        ## uni-directional and multiple:
        if (signatureType %in% c("uni-directional", "multiple")) {
          signature$direction <- NA
        }

        ## bi-directional:
        if (signatureType == "bi-directional") {
          if (!c("direction") %in% colnames(signature)) {
            stop("\"direction\" is missing in the bi-directional signature. \n")
          }

          ## change direction to + and - :
          signature$direction <- signature$direction %>%
            tolower() %>%
            dplyr::case_match(
              .default = signature$direction,
              "up" ~ "+", "increase" ~ "+", "more" ~ "+",
              "dn" ~ "-", "down" ~ "-", "decrease" ~ "-", "less" ~ "-"
            ) %>%
            as.factor()
          if (!all(as.character(unique(signature$direction)) %in% c("-", "+"))) {
            stop("Direction for bi-directional signature should be either \"-\" and \"+\".")
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

  #### public of OmicSigCol ####
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
      sigDF <- mapply(function(x) {
        try_temp <- try(x$extractSignature(conditions), silent = T)
        if (is(try_temp, "try-error")) {
          cat(paste(
            "  Warning: OmicSignature", x$metadata$signature_name,
            "does not have diffanal result matrix (difexp) or does not have the specified column. \n"
          ))
          try_temp <- NULL
        }
        return(try_temp)
      }, private$.OmicSigList)

      # 'sigDF' should be a matrix, each column is one signature, rows are score, symbol, direction
      # process it into a list; each signature is a dataframe as an element in the list
      if (is(sigDF, "matrix")) {
        colnames(sigDF) <- sapply(X = private$.OmicSigList, function(x) {
          x$metadata$signature_name
        })
        sigDF <- apply(sigDF, 2, function(x) {
          data.frame(matrix(unlist(x), nrow = length(x[[1]]), byrow = F), stringsAsFactors = F)
        })
      }

      # change the column names of the dataframes
      if (is(sigDF, "list")) {
        sigDF <- lapply(sigDF, function(x) {
          if (is(x, "data.frame") && nrow(x) > 0) {
            colnames(x) <- c("id", "symbol", "score", "direction")
            x <- x %>%
              dplyr::mutate(
                id = as.character(id),
                symbol = as.character(symbol),
                score = as.numeric(as.character(score)),
                direction = as.character(direction)
              ) %>%
              arrange(desc(abs(score)))
          } else {
            x <- NULL
          }
          return(x)
        })
      }

      # bind them into a single dataframe if asked to
      if (bind) {
        res <- bind_rows(sigDF, .id = "sig_name") # save names(sigDF) as a column sig_name
        if (is(res, "data.frame") && nrow(res) > 0) {
          colnames(res) <- c("sig_name", "id", "symbol", "score", "direction")
          res <- res %>%
            dplyr::mutate(
              id = as.character(id),
              symbol = as.character(symbol),
              score = as.numeric(as.character(score)),
              direction = as.character(direction)
            ) %>%
            arrange(desc(abs(score)))
        } else {
          res <- NULL
        }
        return(res)
      } else {
        return(sigDF)
      }
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

  #### active of OmicSigCol ####
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

  #### private of OmicSigCol ####
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
