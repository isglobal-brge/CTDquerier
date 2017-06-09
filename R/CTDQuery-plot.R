#' @describeIn CTDquery Generates a basic plot showing the number of terms
#' that can be used to query CTDbase.
#' @param x Object of class \code{CTDquery}.
#' @param y NOT USED
#' @param index_name Name of the plot to be draw.
#' @export
setMethod(
    f = "plot",
    signature = "CTDquery",
    definition = function( x, y, index_name = "base", representation = "heatmap", ... ) {
        index_name <- base::tolower( index_name )
        #index_name <- match.arg( index_name, choices = c( "base", "disease", "chemical" ) )

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
                } else if( index_name == "chemical" ) {
                    if( representation == "heatmap" ) {
                        int_plot_gene_chemical_heatmap( x, ... )
                    } else {
                        stop( "No network representation for 'disease association'." )
                    }
                } else if( index_name == "gene-gene interaction" ) {
                    if( representation == "heatmap" ) {
                        warning( "Are you sure you dont what to use 'network' representation?" )
                        int_plot_gene_gene_heatmap( x, ... )
                    } else if ( representation == "network" ) {
                        int_plot_gene_gene_network( x, ... )
                    } else {
                        stop( "Only available 'heatmap' and 'network' representations." )
                    }
                } else if( index_name == "kegg pathways" ) {
                    if( representation == "heatmap" ) {
                        #int_plot_gene_kegg_heatmap( x, ... )
                        stop( "No heatmap representation for 'kegg pathways'." )
                    } else {
                        int_plot_gene_kegg_network( x, ... )
                    }
                } else if( index_name == "go terms" ) {
                    if( representation == "heatmap" ) {
                        #int_plot_gene_go_heatmap( x, ... )
                        stop( "No heatmap representation for 'go terms'." )
                    } else {
                        int_plot_gene_go_network( x, ... )
                    }
                } else {
                    stop( "Invalid provided 'index_name' for 'GENE' query. ")
                }
            } else if( x@type == "CHEMICAL" ) { ## GREEN
                if( index_name == "gene" ) {
                    if( representation == "heatmap" ) {
                        int_plot_chem_gene_heatmap( x, ... )
                    } else {
                        int_plot_chem_gene_network( x, ... )
                    }
                } else if( index_name == "disease" ) {
                    if( representation == "heatmap" ) {
                        int_plot_chem_disease_heatmap( x, ... )
                    } else {
                        stop( "No network representation for 'disease association'." )
                    }
                } else if( index_name == "kegg pathways" ) {
                    if( representation == "heatmap" ) {
                        int_plot_chem_kegg_heatmap( x, ... )
                    } else {
                        stop( "No network representation for 'kegg pathways'." )
                    }
                } else if( index_name == "go terms" ) {
                    if( representation == "heatmap" ) {
                        #int_plot_gene_go_heatmap( x, ... )
                        stop( "No heatmap representation for 'go terms'." )
                    } else {
                        int_plot_chem_go_heatmap( x, ... )
                    }
                } else {
                    stop( "Invalid provided 'index_name' for 'CHEMICAL' query. ")
                }
            } else if( x@type == "DISEASE" ) {
                if( index_name == "gene" ) {
                    if( representation == "heatmap" ) {
                        int_plot_dise_gene_heatmap( x, ... )
                    } else {
                        stop( "No network representation for 'gene association'." )
                    }
                } else if( index_name == "chemical" ) {
                    if( representation == "heatmap" ) {
                        int_plot_disease_chemical_heatmap( x, ... )
                    } else {
                        stop( "No network representation for 'chemical association'." )
                    }
                }
            }
        }
    }
)
