#' @describeIn CTDdata Method to perform enrichment analysis given two
#' object of class \code{CTDdata}.
#' @param universe String vector of genes used as universe. If not provided,
#' all genes in CTDbase are used.
#' @param use Select if all or only curated relations are used.
#' @param warnings Shows or hiddes warnings.
#' @param ... NOT USED
setMethod(
    f = "enrich",
    signature = "CTDdata",
    definition = function( x, y, universe, use = "curated",
                           warnings = TRUE, ... ) {
        use <- match.arg( tolower( use ), choices = c( "all", "curated" ) )
        if( x@type == "VOID" ) {
            warning( "No data was provided (argument 'x')" )
            return(invisible())
        }
        if( y@type == "VOID" ) {
            warning( "No data was provided (argument 'y')" )
            return(invisible())
        }

        if( missing( universe ) ) {
            universe <- get( "universe_gene" )
            #data( "universe_gene", package = "CTDquerier" )
            #universe = universe_gene; rm( universe_gene )
        } else {
            universe <- unique( universe )
        }

        if( x@type == "GENE" ) {
            x <- get_terms( x )[[ "found" ]]
        } else {
            tbl <- get_table( x, index_name = "gene interactions" )
            if( use == "curated" ) {
                if( !"Direct.Evidence" %in% colnames( tbl ) ) {
                    if( warnings ) {
                        warning( "Using 'CTDdata' (x) from chemical query. ",
                                 "No curated assocations can be extracted." )
                    }
                    x <- unique( tbl$Gene.Symbol )
                } else {
                    x <- unique(tbl[tbl$Direct.Evidence != "", "Gene.Symbol"])
                }
            } else {
                x <- unique( tbl$Gene.Symbol )
            }
        }

        if( y@type == "GENE" ) {
            y <- get_terms( y )[[ "found" ]]
        } else {
            tbl <- get_table( y, index_name = "gene interactions" )
            if( use == "curated" ) {
                if( !"Direct.Evidence" %in% colnames( tbl ) ) {
                    if( warnings ) {
                        warning( "Using 'CTDdata' (y) from chemical query. ",
                            "No curated assocations can be extracted." )
                    }
                    y <- unique( tbl$Gene.Symbol )
                } else {
                    y <- unique(tbl[tbl$Direct.Evidence != "", "Gene.Symbol"])
                }
            } else {
                y <- unique( tbl$Gene.Symbol )
            }
        }

        return( fisher.test( table( universe %in% y, universe %in% x ) ) )
    }
)
