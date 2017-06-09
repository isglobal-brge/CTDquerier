#' Getter to obtain the terms used to perform a query into CTDbase
#'
#' @name get_terms
#' @rdname get_terms-methods
#' @aliases get_terms
#' @param object Object of class \link{\code{CTDuqery}}.
#' @return A list with two accessors: \code{"used"} for the terms that
#' exsist in CTDbase, and \code{"lost"} with the terms that do not
#' existi in CTDbase.
#' @examples
#' data("gala")
#' get_terms(gala)[["lost"]]
#' @export get_terms
setGeneric("get_terms", function(object)
  standardGeneric("get_terms")
)

#' Method to perform enrichment analysis using two \code{CTDuqery} objects
#'
#' This methods performes a fisher test using the genes or chemicals
#' in two objects of class \link{\code{CTDuqery}}.
#'
#' @name enrich
#' @rdname enrich-methods
#' @aliases enrich
#' @param x Object of class \link{\code{CTDuqery}}.
#' @param y Object of class \link{\code{CTDuqery}}.
#' @return A list with class \code{htest}. Check
#' \code{fisher.test} for more information.
#' @export enrich
setGeneric("enrich", function(x, y, ...)
    standardGeneric("enrich")
)
