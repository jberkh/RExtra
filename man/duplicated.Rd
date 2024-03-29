% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/duplicated.R
\name{duplicated}
\alias{duplicated}
\title{Duplicated() wrapper}
\usage{
duplicated(
  x,
  incomparables = FALSE,
  fromLast = FALSE,
  nmax = NA,
  allItems = FALSE,
  ...
)
}
\arguments{
\item{x}{Vector to find duplicates in}

\item{incomparables}{Same as in base::duplicated()}

\item{fromLast}{Same as in base::duplicated(). Ignored when allItems is set TRUE}

\item{nmax}{Same as in base::duplicated()}

\item{allItems}{Logical. Return all duplicated items}
}
\value{
A multiplicated string
}
\description{
Adds functionality to base duplicated function to return all duplicated items
}
\examples{
duplicated(c(1,1,2,3,4,5,5), allItems = T)
}
