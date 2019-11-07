#' @describeIn CTDdata Generates a basic plot showing the number of terms
#' that can be used to query CTDbase.
#' @param x Object of class \code{CTDdata}.
#' @param y NOT USED
#' @param index_name Name of the plot to be draw. See \code{\link{CTDdata}}'s
#' detail section for more information (\code{?`CTDdata-class`}).
#' representation.
#' @param representation Can take values \code{"heatmap"} or \code{"network"}.
#' @export
setMethod(
    f = "plot",
    signature = "CTDdata",
    definition = function( x, y, index_name = "base", representation = "heatmap", ... ) {
        index_name <- base::tolower( index_name )
        if( x@type == "VOID" ) {
            warning( "No data was provided" )
            return(invisible())
        }

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
                } else if( index_name == "chemical interactions" ) {
                    if( representation == "heatmap" ) {
                        int_plot_gene_chemical_heatmap( x, ... )
                    } else {
                        stop( "No network representation for 'disease association'." )
                    }
                } else if( index_name == "gene-gene interactions" ) {
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
                        stop( "No heatmap representation for 'kegg pathways'." )
                    } else {
                        int_plot_gene_kegg_network( x, ... )
                    }
                } else if( index_name == "go terms" ) {
                    if( representation == "heatmap" ) {
                        stop( "No heatmap representation for 'go terms'." )
                    } else {
                        int_plot_gene_go_network( x, ... )
                    }
                } else {
                    stop( "Invalid provided 'index_name' for 'GENE' query. ")
                }
            } else if( x@type == "CHEMICAL" ) { ## GREEN
                if( index_name == "gene interactions" ) {
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
                        stop( "No heatmap representation for 'go terms'." )
                    } else {
                        int_plot_chem_go_heatmap( x, ... )
                    }
                } else {
                    stop( "Invalid provided 'index_name' for 'CHEMICAL' query. ")
                }
            } else if( x@type == "DISEASE" ) {
                if( index_name == "gene interactions" ) {
                    if( representation == "heatmap" ) {
                        int_plot_dise_gene_heatmap( x, ... )
                    } else {
                        stop( "No network representation for 'gene association'." )
                    }
                } else if( index_name == "chemical interactions" ) {
                    if( representation == "heatmap" ) {
                        int_plot_disease_chemical_heatmap( x, ... )
                    } else {
                        stop( "No network representation for 'chemical association'." )
                    }
                } else if( index_name == "kegg pathways" ) {
                    if( representation == "network" ) {
                        int_plot_dise_kegg_network( x, ... )
                    } else {
                        stop( "No heatmap representation for 'chemical association'." )
                    }
                } else {
                    stop( "Invalid provided 'index_name' for 'DISEASE' query. ")
                }
            }
        }
    }
)
