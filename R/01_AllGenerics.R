#' Getter to obtain the terms used to perform a query into CTDbase
#'
#' @name get_terms
#' @rdname get_terms-methods
#' @aliases get_terms
#' @param object Object of class \code{\link{CTDdata}}.
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

#' Method to perform enrichment analysis using two \code{CTDdata} objects
#'
#' This methods performs a fisher test using the genes in two objects of
#' class \code{\link{CTDdata}}. The object in 'x' is used as source while
#' the object on 'y' is used as universe. When object 'x' corresponds to
#' an object created with \code{\link{query_ctd_gene}}, the used genes
#' are the found terms in CTDbase. In the other cases (chemicals and
#' disease \code{\link{CTDdata}}), the genes from  the 'gene interactions'
#' table are used. If \code{universe} is missing, all genes in CTDbase
#' are used as universe.
#'
#' @name enrich
#' @rdname enrich-methods
#' @aliases enrich
#' @param x Object of class \code{\link{CTDdata}}.
#' @param y Object of class \code{\link{CTDdata}}.
#' @param universe Vector of strings corresponding to the genes to be used
#' as universe.
#' @param use (default: \code{"curated"}) It can take values \code{"curated"}
#' or \code{"all"} to filter or not filter for curated the genes into
#' the \code{CTDdata} objects.
#' and \code{phyper} (with \code{lower.tail=TRUE}) respectively.
#' @param warnings (default: \code{TRUE}).
#' @param ... NOT USED
#' @return A list with class \code{htest}. Check
#' \code{fisher.test} for more information.
#' @export enrich
setGeneric("enrich", function(x, y, universe, use = "curated",
        warnings = TRUE, ...)
    standardGeneric("enrich")
)

#' Method to obtain a specific inner table from a \code{CTDdata} object.
#'
#' Obtain the raw data from a \code{CTDdata} object, result from a query to
#' CTDbase.
#'
#' @name get_table
#' @rdname get_table-methods
#' @aliases get_table
#' @param object Object of class \code{CTDdata}.
#' @param index_name String indicating the type of data to obtain.
#' @param ... NOT USED
#' @details Available tables are (\code{index_name}):
#' \enumerate{
#'   \item \code{"gene interactions"}: (Only for chemicals) Table with
#'   a relation of the genes interacting with the given chemicals.
#'   \item \code{"chemical interactions"}: (Only for genes) Table with
#'   a relation of the chemicals interacting with the given genes.
#'   \item \code{"diseases"}: Table with a relation of the diseases
#'   associated with given genes or chemicals.
#'   \item \code{"gene-gene interactions"}: (Only for genes) Table with
#'   a relation of the genes interacting with the given genes.
#'   \item \code{"kegg pathways"}: Table with a relation of the KEGG pathways
#'   affected by the given chemicals or where the given genes play a role.
#'   \item \code{"go terms"}: Table with a relatio of the GO terms afected by
#'   the given chemicals or where the given genes play a role.
#' }
#' @return A \code{DataFrame} containing the raw result from CTDdata.
#' @examples
#' data("gala")
#' get_table(gala, "diseases")[1:3, ]
#' @export get_table
setGeneric ("get_table", function(object, index_name, ... )
    standardGeneric("get_table")
)
