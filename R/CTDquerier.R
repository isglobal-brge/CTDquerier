#' @title Package for CTDbase data query, data visualization and data analysis.
#' 
#' @description It can retrieve information related to genes, chemicals and diseases.
#' 
#' @section Data Download:
#' \code{CTDquerier} offers two functions to query CTDbase (http://ctdbase.org):
#' \code{\link{query_ctd_gene}} to query CTDbase given a set of genes; and
#' \code{\link{query_ctd_chem}} to query CTDbase given a set of chemicals. Both
#' functions returns \code{\link{CTDdata}} objects. Raw downloaded information
#' can be retrieved from \code{\link{CTDdata}} using method
#' \code{\link{get_table}}.
#'
#' @section Data Visualization:
#' \code{\link{CTDdata}} objects offers basic visualization of the downloaded
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
#' @importFrom methods new
#' @importFrom stats fisher.test quantile
#' @importFrom S4Vectors DataFrame tail head
NULL
