#' @describeIn CTDquery Generates a basic plot showing the number of terms
#' that can be used to query CTDbase.
#' @param x Object of class \code{CTDquery}.
#' @param y NOT USED
#' @param index_name Name of the plot to be draw.
#' @export
setMethod(
    f = "plot",
    signature = "CTDquery",
    definition = function( x, y, index_name = "base",  representation = "heatmap", ... ) {
        index_name <- base::tolower( index_name )
        index_name <- match.arg( index_name, choices = c( "base", "disease" ) )

        if( index_name == "base" ) {
            in_plot_base( x )
        } else {
            if( x@type == "GENE" ) { ## BLUE
                if( index_name == "disease" ) {
                    if( representation == "heatmap" ) {
                        int_plot_gene_disease_heatmap( x, ... )
                    } else {
                        stop( "No network representation for 'disease association'." )
                    }
                }
            }
        }
    }
)
