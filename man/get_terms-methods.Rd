% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_AllGenerics.R
\name{get_terms}
\alias{get_terms}
\title{Getter to obtain the terms used to perform a query into CTDbase}
\usage{
get_terms(object)
}
\arguments{
\item{object}{Object of class \code{\link{CTDdata}}.}
}
\value{
A list with two accessors: \code{"used"} for the terms that
exists in CTDbase, and \code{"lost"} with the terms that do not
exists in CTDbase.
}
\description{
Getter to obtain the terms used to perform a query into CTDbase
}
\examples{
data("gala")
get_terms(gala)[["lost"]]
}
