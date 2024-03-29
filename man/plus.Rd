% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plus.R
\name{+}
\alias{+}
\title{String concatenation}
\usage{
x + y
}
\arguments{
\item{x}{String object or string vector}

\item{y}{String object string vector. Coerced to string if other type}
}
\value{
A concatenated string (vector)
}
\description{
Concatenate strings with the + operator
}
\examples{
"Hello " + "World"
"Hello " + c("World", "Moon")
"Label_" + 1:5
}
