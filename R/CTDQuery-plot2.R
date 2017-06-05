#' #' @describeIn CTDquery Method to represent CTDbase information of a given query.
#' #' @param x Object of class \code{\link{CTDquery}}.
#' #' @param index_name Name of the table to be ploted (see \code{\link{extract}}).
#' #' @param representation Some of the tables can have both network or heat-map
#' #' representations.
#' #' @param ... Can take anyone of the following values: \code{filter.score} (default
#' #' \code{20}), \code{max.length} (default \code{50}, \code{30} or \code{2}),
#' #' \code{field.score} (default \code{"Inference"}, can also take \code{"Reference"}),
#' #' \code{subset.x} to subset X-axis and \code{subset.y} to subset Y-axis.
#' setMethod( "plot",
#'            signature  = "CTDquery",
#'            definition = function( x, index_name, representation = "heatmap", ... ) {
#'              index_name <- tolower( index_name )
#'              if( x@category == "GENE" ) { ## BLUE
#'                if( index_name == "chemicals interactions" ) {
#'                  if( representation == "heatmap" ) {
#'                    int_plot_gene_chemical_heatmap( x, ... )
#'                  } else {
#'                    stop( "No network representation for 'chemicals interations'." )
#'                  }
#'                } else if( index_name == "disease association" ) {
#'                  if( representation == "heatmap" ) {
#'                   int_plot_gene_disease_heatmap( x, ... )
#'                  } else {
#'                    stop( "No network representation for 'disease association'." )
#'                  }
#'                } else if( index_name == "gene-gene interactions" ) {
#'                  if( representation == "heatmap" ) {
#'                   int_plot_gene_gene_heatmap( x, ... )
#'                  } else {
#'                    stop( "No network representation for 'gene-gene interactions'." )
#'                  }
#'                } else if( index_name == "kegg pathways" ) {
#'                  if( representation == "heatmap" ) {
#'                    int_plot_gene_kegg_heatmap( x, ... )
#'                  } else {
#'                    stop( "No network representation for 'gene-gene interactions'." )
#'                  }
#'                } else if( index_name == "go terms" ) {
#'                  if( representation == "heatmap" ) {
#'                    int_plot_gene_go_heatmap( x, ... )
#'                  } else {
#'                    stop( "No network representation for 'gene-gene interactions'." )
#'                  }
#'                } else {
#'                  stop( "Invalid provided 'index_name' for 'GENE' query. ")
#'                }
#'              } else if( x@category == "CHEM" ) {  ## GREEN
#'                if( index_name == "gene interaction" ) {
#'                  if( representation == "heatmap" ) {
#'                   int_plot_chem_gene_heatmap( x, ... )
#'                  } else {
#'                    stop( "No network representation for 'gene-gene interactions'." )
#'                  }
#'                } else  if( index_name == "disease" ) {
#'                  if( representation == "heatmap" ) {
#'                   int_plot_chem_disease_heatmap( x, ... )
#'                  } else {
#'                    stop( "No network representation for 'gene-gene interactions'." )
#'                  }
#'                } else if( index_name == "kegg pathways" ) {
#'                  if( representation == "heatmap" ) {
#'                    int_plot_chem_kegg_heatmap( x, ... )
#'                  } else {
#'                    stop( "No network representation for 'gene-gene interactions'." )
#'                  }
#'                } else if( index_name == "go terms" ) {
#'                  if( representation == "heatmap" ) {
#'                    int_plot_chem_go_heatmap( x, ... )
#'                  } else {
#'                    stop( "No network representation for 'gene-gene interactions'." )
#'                  }
#'                } else {
#'                  stop( "Invalid provided 'index_name' for 'CHEMICAL' query. ")
#'                }
#'
#'              }
#'            }
#' )
