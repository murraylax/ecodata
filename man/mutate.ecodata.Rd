% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecodata.R
\name{mutate.ecodata}
\alias{mutate.ecodata}
\title{Create and Modify Variables}
\usage{
mutate.ecodata(.data, ..., units = NULL)
}
\arguments{
\item{.data}{An ecodata data frame}

\item{...}{Parameters passed on to mutate}

\item{units}{Optional, character string, or vector of strings, for the units to give to the new variables.
Should be a vector of the same length as the number of expressions, or a single string if
all the expressions have the same units.
Default for each new variable is to use the units for the first variable in the expression.}
}
\value{
An ecodata data frame with the new variables and metadata attributes
}
\description{
Wrapper to the `dplyr::mutate()` function that adds metadata attributes to the new variables.
}
\seealso{
`dplyr::mutate()`
}
