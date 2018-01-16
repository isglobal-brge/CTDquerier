#' @describeIn CTDdata Method to obtain a specific inner table from an \code{CTDdata} object.
#' @param object Object of class \code{CTDdata}.
setMethod( "get_table",
    signature  = "CTDdata",
    definition = function( object, index_name, ... ) {
        index_name <- base::tolower( index_name )
        switch( index_name,
            "gene interactions" = {
                return( object@gene_interactions )
             },
            "chemical interactions" = {
                return( object@chemicals_interactions )
            },
            "diseases" = {
                return( object@diseases )
            },
            "gene-gene interactions" = {
                return( object@gene_gene_interactions )
            },
            "kegg pathways" = {
                return( object@kegg )
            },
            "go terms" = {
                return( object@go )
            },
            stop( "Invalid provided 'index_name'. ")
        )
    }
)
