#' Getter to obtain the terms used to perform a query into CTDbase
#'
#' @name get_terms
#' @rdname get_terms-methods
#' @aliases get_terms
#' @param object Object of class \link{CTDuqery}.
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
