#' @title BRENDACurrentName
#' @description Get the current BRENDA term id and name from a term id,
#' term name, or synonym.
#' updated 07/2020
#' @param x A BRENDA term id, term name, or synonym of a term
#' @param obo Optional. The ontology file. BRENDA OBO file will be loaded if not provided.
#' @return If input is a valid BRENDA term, return a character of the current
#' term id and term name
#'
#' @examples
#' BRENDACurrentName("BTO:0005475") # valid current name, output itself
#' BRENDACurrentName("SUM149 cell") # valid synonym; change to current name
#' BRENDACurrentName("BTO:0005388") # valid synonym; change to current name
#' @export
#' 
BRENDACurrentName <- function(x, obo = BRENDAobo) {
  # if input is a current term id:
  if (startsWith(x, "BTO:") & x %in% obo$id) {
    x <- c(x, obo$name[x])
  }
  # else if input is a current term name:
  else if (x %in% obo$name) {
    x <- c(names(which(obo$name == x)), x)
  }
  # else, could be invalid or a synonym name
  else {
    x <- c(x, x)
  }

  # if x[1] is not a current id, then search if x[2] is a synonym:
  if (!x[1] %in% obo$id[!obo$obsolete]) {
    obo$synonym <- obo$synonym[!obo$obsolete] # exclude obsolete names
    for (i in c(1:length(obo$synonym))) {
      # if find x[2] is a synonym, change to current one:
      if (x[2] %in% obo$synonym[[i]]) {
        x <- c(names(obo$synonym)[i], obo$name[names(obo$synonym)[i]])
        break
      }
    }
    # after the loop, if x did not get changed, it is not found to be a synonym:
    if (x[1] == x[2]) {
      stop("Error: input is not a valid BRENDA id, name, nor existing synonym.")
    }
  }
  names(x) <- NULL
  return(x)
}

#' @title BRENDASearch
#' @description search for ontology term in an obo file or a vector
#' of all ontology term name
#' updated 07/2020
#' @param x A string or character vector to search for.
#' x will be divided into different search patterns by space. See examples below.
#' @param obo The ontology file or a character vector of all possible term names. Optional. BRENDA OBO file will be loaded if not provided.
#' @param contain_all if TRUE, will only return the results contain all search terms.
#' if FALSE, will return results contain any of the given pattern.
#' @return matrix including search result
#' @importFrom methods is
#' @examples
#' BRENDASearch("MDA MB cell", contain_all = TRUE) 
#' # search for results that contain "MDA" "MB" and "cell"
#' BRENDASearch(c("MDA MB", "cell"), contain_all = TRUE) 
#' # same as above
#' BRENDASearch(c("MDA-MB", "SUM"), contain_all = FALSE) 
#' # search for results that contain either "MDA-MB" or "SUM"
#' @export
BRENDASearch <- function(x, obo = BRENDAobo, contain_all = TRUE) {
  x <- unlist(strsplit(x, split = " "))
  if (is(obo, "ontology_index")) {
    obo <- obo$name[!obo$obsolete]
  }
  if (contain_all) {
    result <- c(1:length(obo))
    for (i in x) {
      result0 <- grep(i, obo)
      result <- intersect(result0, result)
    }
  } else {
    result <- numeric(0)
    for (i in x) {
      result0 <- grep(i, obo)
      result <- union(result0, result)
    }
  }
  result <- obo[result]
  result_df <- cbind(names(result), result)
  colnames(result_df) <- c("ID", "Name")
  return(result_df)
}

#' @title BRENDADevelopFrom
#' @description Get the tissue a term is developed from, if available
#' updated 07/2020
#' @param x A BRENDA term id, term name, or synonym of a term.
#' @param obo Optional. The ontology file. BRENDA OBO file will be loaded if not provided.
#' @return If input is a valid BRENDA term, return a character of the term id
#' and term name the input is developed from.
#'
#' @examples
#' BRENDADevelopFrom("BTO:0000995")
#' BRENDADevelopFrom("BTO:0000150")
#' BRENDADevelopFrom("BTO:0000187") # input is a valid BRENDA term but
#' # it is not developed from any other tissues, gives character0
#' @export
BRENDADevelopFrom <- function(x, obo = BRENDAobo) {
  id <- BRENDACurrentName(x, obo)[1]
  result <- c(obo$develops_from[[id]], obo$name[obo$develops_from[[id]]])
  if (length(result) == 0) {
    result <- character(0)
  }
  names(result) <- NULL
  return(result)
}

#' @title BRENDAPartOf
#' @description Get the tissue a term is part of, if available. The function
#' only retrieves the information directly saved in the obo file, and will not
#' automatically find "ancestors" in "part of" relationship.
#' For example, if A is part of B, B is part of C,
#' the function will NOT automatically return C in the result of A, unless
#' the relationship between A and C is specified in the obo file.
#' updated 07/2020
#' @param x A BRENDA term id, term name, or synonym of a term.
#' @param obo Optional. The ontology file. BRENDA OBO file will be loaded if not provided.
#' @return If input is a valid BRENDA term, return a dataframe of the term id
#' and term name the input is part of. Some terms are part of multiple terms.
#'
#' @examples
#' BRENDAPartOf("BTO:0000187")
#' BRENDAPartOf("myeloblast")
#' BRENDAPartOf("BTO:0001732")
#' @export
BRENDAPartOf <- function(x, obo = BRENDAobo) {
  id <- BRENDACurrentName(x, obo)[1]
  result <- matrix(c(obo$part_of[[id]], obo$name[obo$part_of[[id]]]),
    ncol = 2, byrow = F
  )
  colnames(result) <- c("id", "name")
  if (nrow(result) == 0) {
    result <- character(0)
  }
  return(result)
}

#' @title BRENDAAncestors
#' @description Get all the ancestors of a term. Parents of its parents will
#' be retrieved. Based on `get_ancestors()` function in `ontologyIndex`
#' updated 07/2020
#' @param x A BRENDA term id, term name, or synonym of a term
#' @param obo Optional. The ontology file. BRENDA OBO file will be loaded if not provided.
#' @return If input is a valid BRENDA term, return a dataframe of the term ids
#' and term names of the input's ancestors.
#'
#' @examples
#' BRENDAAncestors("BTO:0000034")
#' BRENDAAncestors("SUM149 cell") # synonym; function will change it to the current name
#' BRENDAAncestors("BTO:0000216") # input valid but no result, gives character0
#' @export
BRENDAAncestors <- function(x, obo = BRENDAobo) {
  x <- BRENDACurrentName(x, obo)
  result <- ontologyIndex::get_ancestors(obo, terms = x[1])
  if (length(result) > 1) {
    result <- matrix(c(result, obo$name[result]), ncol = 2, byrow = F)
    colnames(result) <- c("id", "name")
  } else {
    result <- character(0)
  }
  return(result)
}

#' @title BRENDADescendants
#' @description Get all the descendants of a term. Children of its children will
#' be retrieved. Based on `get_descendants()` function in `ontologyIndex`
#' updated 07/2020
#' @param x A BRENDA term id, term name, or synonym of a term.
#' @param obo Optional. The ontology file. BRENDA OBO file will be loaded if not provided.
#' @return If input is a valid BRENDA term, return a dataframe of the term ids.
#' and term names of the input's descendants.
#'
#' @examples
#' BRENDADescendants("BTO:0000034")
#' BRENDADescendants("BTO:0000004") # input valid but no result, gives character0
#' @export
BRENDADescendants <- function(x, obo = BRENDAobo) {
  x <- BRENDACurrentName(x, obo)
  result <- ontologyIndex::get_descendants(obo, roots = x[1])
  if (length(result) > 1) {
    result <- matrix(c(result, obo$name[result]), ncol = 2, byrow = F)
    colnames(result) <- c("id", "name")
  } else {
    result <- character(0)
  }
  return(result)
}
