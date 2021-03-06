% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index_r.R
\name{rli}
\alias{rli}
\title{Calculate status index from numeric status scores}
\usage{
rli(w, Wex = 5, N = length(w))
}
\arguments{
\item{w}{species weights (numeric)}

\item{Wex}{maxiumum weighted score for extinct species. Default \code{5} for NatureServe
ranking system}

\item{N}{number of species (default length of 'w')}
}
\value{
numeric vector on the scale between 0 and 1.
}
\description{
Calculate status index from numeric status scores
}
\examples{
rli(c(0,2,5,2))
}
