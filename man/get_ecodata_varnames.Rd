% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecodata.R
\name{get_ecodata_varnames}
\alias{get_ecodata_varnames}
\title{Get all variable names}
\usage{
get_ecodata_varnames(data)
}
\arguments{
\item{data}{Data frame with fetched with a get_ecodata() function}
}
\value{
Returns a vector of variable names
}
\description{
Fetch all the variables in a given data frame, except the date and recession
}
