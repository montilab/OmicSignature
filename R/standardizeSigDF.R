#' @title remove empty, duplicate rows in signature dataframe
#' updated 08/2020
#'
#' @importFrom dplyr distinct %>%
#' @importFrom stats complete.cases
#' @param sigdf signature dataframe
#' @return signature dataframe with empty, duplicate rows removed and ordered by score
#' @export
standardizeSigDF <- function(sigdf){
	sigdf <- replaceSigCol(sigdf, ObjtoGeneral = F)
	sigdf <- sigdf[complete.cases(sigdf), ]
	sigdf <- sigdf[sigdf$signature_symbol != "", ]
	sigdf <- sigdf[sigdf$signature_score != "", ]
	sigdf <- sigdf[order(abs(as.numeric(as.character(sigdf$signature_score))), decreasing = T), ]
	sigdf <- dplyr::distinct(sigdf, sigdf$signature_symbol, .keep_all = T)#[, c(1:3)]
	sigdf$`sigdf$signature_symbol` <- NULL
	sigdf <- sigdf[order(sigdf$signature_direction, decreasing = F), ]
	return(sigdf)
}
