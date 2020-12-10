# library(R6)
# library(dplyr)
# library(jsonlite)

#### OmicSigObj ####

#' @title OmicSignature R6 object
#' @description a R6 object to store signatures generated from experiments. In cluding metadata, signature, and an optional differential expression matrix.
#' @importFrom R6 R6Class
#' @importFrom dplyr filter pull %>%
#' @importFrom jsonlite toJSON fromJSON
#' @export
OmicSignature <-
  R6::R6Class(
    "OmicSignature",
    #### public of OmicSig ####
    public = list(
      #' @description
      #' Create a new OmicSignature object
      #' @param metadata required, must be a list
      #' @param signature required, must be a dataframe
      #' @param difexp optional
      #' @param print_message use TRUE if want to see all messages printed
      #' @export
      initialize = function(metadata, signature, difexp = NULL, print_message = FALSE) {
        if (!is.null(difexp)) {
          difexp <- private$checkDifexp(difexp, v = print_message)
        }
        private$.metadata <- private$checkMetadata(metadata, v = print_message)
        private$.signature <- private$checkSignature(signature, signatureType = metadata$direction_type, v = print_message)
        private$.difexp <- difexp
        cat(paste(
          "  [Success] OmicSignature object",
          private$.metadata$signature_name, "created.\n"
        ))
      },
      #' @description
      #' Print an OmicSignature object
      #' @export
      print = function(...) {
        cat("Signature Object: \n")
        cat("  Metadata: \n")
        sh <- mapply(function(k, v) {
          cat("   ", k, "=", paste(v, collapse = ", "), "\n")
        }, names(private$.metadata), private$.metadata)
        cat("  signature: \n")
        sh <- mapply(
          function(k, v) {
            cat("    ", k, " (", v, ")", "\n", sep = "")
          }, names(summary(private$.signature$signature_direction)),
          summary(private$.signature$signature_direction)
        )
        cat("  Differential Expression Data: \n")
        cat("    ", nrow(private$.difexp), " x ", ncol(private$.difexp), "\n", sep = "")
        invisible(self)
      },
      #' @param conditions conditions for new signatures
      #' @return a dataframe of new signatures
      #' @export
      extract.signature = function(conditions) {
        if (is.null(private$.difexp)) {
          stop("Error: Difexp matrix not found in OmicSignature object.")
        }
        v <- rlang::parse_exprs(conditions)
        res <- private$.difexp %>%
          dplyr::filter(!!!v) %>%
          dplyr::select(symbol, score) %>%
          dplyr::mutate(direction = ifelse(score < 0, "-", "+")) %>%
          dplyr::arrange(direction)
        res <- res[complete.cases(res), ]
        res <- res[res$symbol != "", ]
        res <- res[res$score != "", ]
        res <- res[order(abs(as.numeric(as.character(res$score))), decreasing = T), ]
        res <- distinct(res, res$symbol, .keep_all = T)[, c(1:3)]
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
      #' @field signature a dataframe contains symbol, score (optional) and direction (optional)
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
        if (is(difexp, "OmicSignature")) {
          difexp <- difexp$difexp
        }
        stopifnot(is(difexp, "data.frame"))

        ## check if it's empty:
        if (nrow(difexp) == 0) {
          stop("Differential Matrix (lv1 data) is empty. ")
        }

        ## check column names:
        ## if id is not included, then use probe_id as id
        if (!("id" %in% colnames(difexp)) && "probe_id" %in% colnames(difexp)) {
            colnames(difexp) <- colnames(difexp) %>%
            dplyr::recode("probe_id" = "id")
        }
        ## require only one of adj_p, p_value, and q_value
        if ("adj_p" %in% colnames(difexp)) {
          difexpColRequired <- c("id", "symbol", "score", "adj_p")
        } else if ("p_value" %in% colnames(difexp)) {
          difexpColRequired <- c("id", "symbol", "score", "p_value")
        } else if ("q_value" %in% colnames(difexp)) {
          difexpColRequired <- c("id", "symbol", "score", "q_value")
        }
        
        difexpColMissing <- setdiff(difexpColRequired, colnames(difexp))
        difexpColAdditional <- setdiff(colnames(difexp), difexpColRequired)

        if (length(difexpColMissing) > 0) {
          stop("Differential Matrix (lv1 data) does not contain required column(s): ",
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
            stop("difexp: signature symbol is not character.")
          }
        }
        private$verbose(v, "  [Success] difexp matrix is valid. \n")
        return(difexp)
      },
      checkMetadata = function(metadata, v = FALSE) {
        stopifnot(is(metadata, "list"))
        if (is.null(metadata$covariates)) {
          metadata$covariates <- "none"
        }
        if (is.null(metadata$phenotype)) {
          metadata$phenotype <- "unknown"
        }

        # check required metadata fields
        metadataRequired <- c(
          "signature_name", "organism", "platform",
          "direction_type", "phenotype", "covariates"
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

        # check if sample_type is a valid BRENDA term
        if (!is.null(metadata$sample_type)) {
          tempSampleType <- try(SigRepoR::BRENDACurrentName(metadata$sample_type), silent = T)
          if (is(tempSampleType, "character")) {
            metadata$sample_type <- tempSampleType[2]
          } else {
            warning(paste(
              "sample_type in metadata is not a valid BRENDA ontology term.",
              "Please consider using BRENDASearch() function to 
            	search for the correct BRENDA ontology term to use.",
              sep = "\n"
            ))
          }
        }

        # check if platform is a valid GPL platform
        if (!metadata$platform %in% GEOplatform$Accession) {
          warning(paste(
            "platform in metadata is not a valid GEO platform accession.",
            "Please consider using GEOSearch() function to 
            	search for the correct platform assession to use.",
            sep = "\n"
          ))
        }
        private$verbose(v, "  [Success] Metadata is saved. \n")
        metadata <- metadata[order(names(metadata))]
        return(metadata)
      },
      checkSignature = function(omicObj, signatureType = NULL, category_num = 0, v = FALSE) {
        ## category_num is used for multi-directional signature
        ## read the signature, and check if it is a dataframe:
        if (is(omicObj, "OmicSignature")) {
          signature <- omicObj$signature
          signatureType <- omicObj$metadata$direction_type
          if (signatureType == "multi-directional") {
            if (!is.null(omicObj$metadata$category_num)) {
              categoryNum <- omicObj$metadata$category_num
            } else {
              stop("Signature is specified as multi-directional, but sample number not found.")
            }
          }
        }
        else if (is(omicObj, "data.frame")) {
          signature <- omicObj
          remove(omicObj)

          ## change possible column names to standard column names:
          colnames(signature) <- colnames(signature) %>%
            tolower() %>%
            dplyr::recode(
              "signature" = "signature_symbol", "symbol" = "signature_symbol",
              "name" = "signature_symbol", "score" = "signature_score",
              "weight" = "signature_score", "direction" = "signature_direction"
            )
        } else {
          stop("Signature not found in OmicSignature object, or signature is not a dataframe.")
        }

        if (nrow(signature) == 0) {
          stop("Signature is empty.")
        }
        ## check if signature_symbol and signature_score (lv2 or lv3) exists:
        if (!c("signature_symbol") %in% colnames(signature)) {
          stop("Signature dataframe does not contain \"signature_symbol\" column.")
        }
        if (!c("signature_score") %in% colnames(signature)) {
          warning("Signature score not found. \n")
        }

        ## check if the direction match with signature type:
        if (is.null(signatureType)) {
          stop("Signature type not specified. It needs to be uni-, bi- or multi-directional.")
        }
        ## each signature type need to be "else if". because if none of the
        ## type meet the criteria required, we need an "else" to output error.

        ## bi-directional signature:
        if (signatureType == "bi-directional") {
          if (!"signature_direction" %in% colnames(signature)) {
            stop("Signature is specified as bi-directional but \"signature_direction\" information not found.")
          }
          ## change direction symbol to + and - :
          colnames(signature) <- colnames(signature) %>%
            tolower() %>%
            dplyr::recode(
              "signature" = "signature_symbol", "symbol" = "signature_symbol",
              "name" = "signature_symbol", "score" = "signature_score",
              "weight" = "signature_score", "direction" = "signature_direction"
            )
          signature$signature_direction <- signature$signature_direction %>%
            tolower() %>%
            dplyr::recode("up" = "+", "dn" = "-", "down" = "-")
          signature$signature_direction <- as.factor(signature$signature_direction)

          ## check direction:
          summaryDirection <- summary(signature$signature_direction)
          if (isTRUE(all.equal(c("-", "+"), names(summary(signature$signature_direction))))) {
            private$verbose(v, "  Signature: Checked, signature is bi-directional
  						with - (Dn) and + (Up) directions. \n")
          } else if (names(summaryDirection) == c("-") | names(summaryDirection) == c("+")) {
            private$verbose(v, "  Signature: Checked, signature is bi-directional,
  						but only one direction is found. \n")
          } else {
            stop("Direction info in bi-directional signature is not valid.
  						Direction should be marked with \"-\" and \"+\".")
          }
        }

        ## uni-directional signature:
        else if (signatureType == "uni-directional") {
          signature$signature_direction <- NULL
        }
        ## multi-directional signature:
        else if (signatureType == "multi-directional") {
          summaryDirection <- summary(signature$signature_direction)
          if (length(summaryDirection) != categoryNum) {
            warning("Category number in metadata does not match with the number of
  						categories in the signature.")
          }
        }

        ## if none of the signature type is met:
        else {
          stop("Error: Signature information invalid.")
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
#' @importFrom dplyr filter pull %>%
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
    print = function(...) {
      cat("Signature Collection: \n")
      cat("  Metadata: \n")
      sh <- mapply(function(k, v) {
        cat("   ", k, "=", v, "\n")
      }, names(private$.metadata), private$.metadata)
      cat("  OmicSignature Objects: \n")
      cat("   ", paste(names(private$.OmicSigList), collapse = "\n    "))
      cat("\n  Available Difexp columns: \n")
      sh <- mapply(function(k) {
        cat("   ", k$metadata$signature_name, " (", paste(
          {
            if (is.null(k$difexp)) {
              "* NULL *"
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
    extract.signature = function(conditions, bind = TRUE) {
      a <- mapply(function(x) {
        try_temp <- try(x$extract.signature(conditions), silent = T)
        if (class(try_temp) == "try-error") {
          cat(paste(
            "  Warning: OmicSignature", x$metadata$signature_name,
            "does not have diffexp matrix or does not have the specified column. \n"
          ))
          try_temp <- NULL
        }
        return(try_temp)
      }, private$.OmicSigList)
      if (class(a) == "matrix") {
        rownames(a) <- sapply(X = private$.OmicSigList, function(x) {
          print(x$metadata$signature_name)
        })
        a <- apply(a, 2, function(x) {
          data.frame(matrix(unlist(x), nrow = length(x[[1]]), byrow = F), stringsAsFactors = F)
        })
      }
      if (class(a) == "list") {
        a <- lapply(a, function(x) {
          if (class(x) == "data.frame" && nrow(x) > 0) {
            colnames(x) <- c("symbol", "score", "direction")
            x$direction <- as.character(x$direction)
          } else {
            x <- NULL
          }
          return(x)
        })
      }
      if (bind) {
        res <- bind_rows(a, .id = "sig_name")
        if (is(res, "data.frame") && nrow(res) > 0) {
          colnames(res) <- c("sig_name", "symbol", "score", "direction")
          res <- res[order(res$score, decreasing = T), ]
        } else {
          res <- NULL
        }
        return(res)
      } else {
        return(a)
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
          stop("Element in OmicSigList is not an OmicSignature object. Please check your input.")
        }
      }
      return(OmicSigList)
    }
  )
)
