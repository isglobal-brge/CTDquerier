int_plot_gene_gene_network <- function( x, main ) {
    tbl <- get_table( x, index_name = "gene-gene interactions" )
    tbl <- data.frame( tbl )[ ,
                              c( "Source.Gene.Symbol", "Target.Gene.Symbol", "GeneSymbol", "Assay",
                                 "Interaction.Type", "Throughput" ) ]

    edges <- tbl[ , c( "Source.Gene.Symbol", "Target.Gene.Symbol") ]
    netw  <- igraph::graph.data.frame( edges, directed = FALSE )
    netw  <- igraph::simplify( netw )
    lay   <- igraph::layout_with_fr( netw )

    if( missing( main ) ) {
        main <- " "
    }


    igraph::plot.igraph( netw,
                         vertex.frame.color = "white",
                         layout             = lay,
                         vertex.color       = ifelse( igraph::V( netw )$name %in% tbl$GeneSymbol, "#A52A2A", "#FFA500" ),
                         vertex.label.dist  = 0,      #puts the name labels slightly off the dots
                         vertex.frame.color = 'blue', #the color of the border of the dots
                         vertex.label.color = 'black',#the color of the name labels
                         vertex.label.font  = 0,      #the font of the name labels
                         vertex.label       = igraph::V( netw )$name, #specifies the lables of the vertices. in this case the 'name' attribute is used
                         edge.color         = "darkgrey",
                         edge.width         = 3,
                         edge.arrow.size    = 0.5,
                         vertex.size        = 10,
                         vertex.label.cex   = 0.8,    #specifies the size of the font of the labels
                         main               = main
    )


}
