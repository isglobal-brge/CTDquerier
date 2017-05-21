#' Class CTDquery
#'
#' Class resulting of \link{\code{prepare_ctd_gene}} and
#' \link{\code{prepare_ctd_chem}}. It is sued to encapsulate
#' all the information in CTDbase for given set of genes
#' or chemicals.
#'
#' @name CTDquery
#' @aliases CTDquery-class
#' @rdname CTDquery-class
#' @exportClass CTDquery
#' @slot type Character saving \code{"GENE"} or \code{"CHEMICAL"}
#' depending if it was created using \code{\link{prepare_ctd_gene}}
#' or \code{\link{prepare_ctd_chem}}.
#' @slot terms \code{DataFrame} with the genes or chemicals used
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
#' @seealso \code{\link{prepare_ctd_gene}} to create a \code{CTDquery}
#' from a set of genes, \code{\link{prepare_ctd_chem}} to create a
#' \code{CTDquery} from a set of chemicals and \link{extract} to
#' retrive encapsulated data.
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
