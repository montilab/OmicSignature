% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replaceSigCol.R
\name{replaceSigCol}
\alias{replaceSigCol}
\title{rename the columns of the signature data frame}
\usage{
replaceSigCol(sigdf, to_long_name = FALSE)
}
\arguments{
\item{sigdf}{signature dataframe}

\item{to_long_name}{if TRUE, replace "symbol" to "signature_symbol", mainly
used to facilitate write into json format; if FALSE, replace "signature_symbol"
to "symbol", i.e. standard sigdf names.}
}
\value{
signature dataframe with column names changed to standard
}
\description{
updated 08/2024
}
