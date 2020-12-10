#' @title a test function 
#' @description type a number, and it will gives you its square and cubic value.
#' Or a text, it will gives you the length of it.
#'
#' @param num A number
#' @return A list
#' @export
testRepo <- function(num=NULL) {
	cat("Hello! Welcome to SigRepo package. -- developing version\n")
  if (is(num, "numeric")){
  res <- list("square" = num^2, "cubic" = num^3)
  return(res)
  } else {
  	stop("Please input a number.")
  }
}
