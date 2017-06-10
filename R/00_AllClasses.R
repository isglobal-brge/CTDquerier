#' Class CTDquery
#'
#' Class resulting of \link{\code{query_ctd_gene}},
#' \link{\code{query_ctd_chem}} and \link{\code{query_ctd_dise}}. It is used
#' to encapsulate all the information in \emp{CTDbase} for given set of genes,
#' chemicals or diseases.
#'
#' @name CTDquery
#' @aliases CTDquery-class
#' @rdname CTDquery-class
#' @exportClass CTDquery
#' @slot type Character saving \code{"GENE"}, \code{"CHEMICAL"} or
#' \code{"DISEASE"} depending if it was created using
#' \code{\link{prepare_ctd_gene}}, \code{\link{prepare_ctd_chem}} or
#' \link{\code{query_ctd_dise}}
#' @slot terms \code{DataFrame} with the genes, chemicals or diseases used
#' to create the object.
#' @slot losts Character with the terms used to create the object
#' but that were nor present in CTDbase.
#' @slot gene_interactions (Only for chemicals) Table with a relation
#'   of the genes interacting with the given chemicals.
#' @slot chemicals_interactions (Only for genes) Table with a relation
#'   of the chemicals interacting with the given genes.
#' @slot diseases Table with a relation of the diseases associated with
#'   given genes or chemicals.
#' @slot gene_gene_interactions (Only for genes) Table with a relation
#'   of the genes interacting with the given genes.
#' @slot kegg Table with a relation of the KEGG pathways affected by the
#' given chemicals or where the given genes play a role.
#' @slot go Table with a relatio of the GO terms afected by the given
#' chemicals or where the given genes play a role.
#' @seealso \code{\link{query_ctd_gene}} to create a \code{CTDquery}
#' from a set of genes, \code{\link{query_ctd_chem}} to create a
#' \code{CTDquery} from a set of chemicals, \link{\code{query_ctd_dise}} to
#' create a \code{CTDquery} from a set of diseases,
#' \code{\link[psygenet2r]{extract}} to retrive encapsulated data and
#' \code{plot} to get nice plots from stored data.
#' @return An object of class \code{CTDquery}
setClass( "CTDquery",
  representation =
    representation(
      # esentials
      type                   = "character",
      terms                  = "DataFrame",
      losts                  = "character",
      # CTDbase results
      gene_interactions      = "DataFrame",
      chemicals_interactions = "DataFrame",
      diseases               = "DataFrame",
      gene_gene_interactions = "DataFrame",
      kegg                   = "DataFrame",
      go                     = "DataFrame"
    )
)
