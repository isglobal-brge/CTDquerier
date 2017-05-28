#' Package for CTDbase data query, data visualization and data analysis.
#'
#'
#' @section Data Download:
#' \code{CTDquerier} offers two functions to query CTDbase (http://ctdbase.org):
#' \link{\code{query_ctd_gene}} to query CTDbase given a set of genes; and
#' \link{\code{query_ctd_chem}} to query CTDbase given a set of chemicals. Both
#' functions returns \link{\code{CTDquery}} objects. Raw downloaded information
#' can be retrieved from \link{\code{CTDquery}} using method
#' \code{\link[psygenet2r]{extract}}.
#'
#' @section Data Visualization:
#' \link{\code{CTDquery}} objects offers basic visualization of the downloaded
#' information using standard \code{plot} method.
#'
#'
#' @docType package
#' @name CTDquerier
#'
#' @import utils
#'
#' @importFrom RCurl getURL
#' @importFrom stringr str_replace
#' @importFrom stringdist stringdist
#' @importFrom psygenet2r extract
NULL
