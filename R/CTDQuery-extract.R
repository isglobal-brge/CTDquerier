#' @describeIn CTDquery Method to obtain a specific inner table from an \code{CTDquery} object.
#' @param object Object of class \code{CTDquery}.
#' @param name Name of the table to be retrived.
#' @details Available tables are:
#' \enumerate{
#'   \item \code{"genes interactions"}: (Only for chemicals) Table with a relation
#'   of the genes interacting with the given chemicals.
#'   \item \code{"chemicals interactions"}: (Only for genes) Table with a relation
#'   of the chemicals interacting with the given genes.
#'   \item \code{"diseases"}: Table with a relation of the diseases associated with
#'   given genes or chemicals.
#'   \item \code{"gene-gene interactions"}: (Only for genes) Table with a relation
#'   of the genes interacting with the given genes.
#'   \item \code{"kegg pathways"}: Table with a relation of the KEGG pathways affected by the
#' given chemicals or where the given genes play a role.
#'   \item \code{"go terms"}: Table with a relatio of the GO terms afected by the given
#' chemicals or where the given genes play a role.
#' }

setMethod( "extract",
  signature  = "CTDquery",
  definition = function( object, index_name, ... ) {
      index_name <- base::tolower( index_name )
    if( index_name == "gene interactions" ) {
      return( object@gene_interactions )
    } else if( index_name == "chemical interactions" ) {
      return( object@chemicals_interactions )
    } else if( index_name == "diseases" ) {
      return( object@diseases )
    } else if( index_name == "gene-gene interactions" ) {
      return( object@gene_gene_interactions )
    } else if( index_name == "kegg pathways" ) {
      return( object@kegg )
    } else if( index_name == "go terms" ) {
      return( object@go )
    } else {
      stop( "Invalid provided 'index_name'. ")
    }
  }
)
