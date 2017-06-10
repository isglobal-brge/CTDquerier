#' @describeIn CTDquery Generates a basic plot showing the number of terms
#' that can be used to query CTDbase.
#' @param x Object of class \code{CTDquery}.
#' @param y NOT USED
#' @param index_name Name of the plot to be draw. \code{"base"} shows a
#' bar-plot indicating the number of terms lost&found at CTDbase. For
#' \emph{gene} queries \code{index_name} can take values \code{"disease"},
#' \code{"chemical"}, \code{"gene-gene interaction"}, \code{"kegg pathways"}
#' and \code{"go terms"}. The first two have only \code{"heatmap"}
#' representation, the last to have only \code{"network"} representation, and
#' \code{"gene-gene interaction"} has both. For \emph{chemical} queries
#' \code{index_name} can take values \code{"gene"}, \code{"disease"},
#' \code{"kegg pathways"} and \code{"go terms"}. The first has both
#' \code{"heatmap"} and \code{"network"} representation, while the last only
#' \code{"network"}. The otehr three have only \code{"heatmap"} representation.
#' For \emph{disease} queries \code{index_name} can take values \code{"gene"},
#' \code{"chemical"} and \code{"kegg pathways"}. The first two have
#' \code{"heatmap"} representation while the last \code{"network"}
#' representation.
#' @param representation Can take values \code{"heatmap"} or \code{"network"}.
#' @export
setMethod(
    f = "plot",
    signature = "CTDquery",
    definition = function( x, y, index_name = "base", representation = "heatmap", ... ) {
        index_name <- base::tolower( index_name )

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
