% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OmicSignature.R
\name{OmicSignature}
\alias{OmicSignature}
\title{OmicSignature R6 object}
\description{
a R6 object to store signatures generated from experiments. In cluding metadata, signature, and an optional differential expression analysis result dataframe.
updated 10/2024
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{metadata}}{a list to describe the metadata}

\item{\code{signature}}{a dataframe contains probe_id, feature_name, score (optional) and direction (optional)}

\item{\code{difexp}}{a dataframe for differential expression result}

\item{\code{removeDifexp}}{a function to remove difexp from the object}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-OmicSignature-new}{\code{OmicSignature$new()}}
\item \href{#method-OmicSignature-print}{\code{OmicSignature$print()}}
\item \href{#method-OmicSignature-extractSignature}{\code{OmicSignature$extractSignature()}}
\item \href{#method-OmicSignature-clone}{\code{OmicSignature$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OmicSignature-new"></a>}}
\if{latex}{\out{\hypertarget{method-OmicSignature-new}{}}}
\subsection{Method \code{new()}}{
Create a new OmicSignature object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OmicSignature$new(metadata, signature, difexp = NULL, print_message = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{metadata}}{required. a list. See `createMetadata` for more information}

\item{\code{signature}}{required. a vector, or a dataframe including columns: "probe_id", "feature_name" and "direction", and an optional column "score"}

\item{\code{difexp}}{optional}

\item{\code{print_message}}{use TRUE if want to see all messages printed}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OmicSignature-print"></a>}}
\if{latex}{\out{\hypertarget{method-OmicSignature-print}{}}}
\subsection{Method \code{print()}}{
Print an OmicSignature object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OmicSignature$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OmicSignature-extractSignature"></a>}}
\if{latex}{\out{\hypertarget{method-OmicSignature-extractSignature}{}}}
\subsection{Method \code{extractSignature()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OmicSignature$extractSignature(conditions)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{conditions}}{conditions for new signatures}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a dataframe of new signatures
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OmicSignature-clone"></a>}}
\if{latex}{\out{\hypertarget{method-OmicSignature-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OmicSignature$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
