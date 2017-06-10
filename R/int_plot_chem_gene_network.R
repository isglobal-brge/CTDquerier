int_plot_chem_gene_network <- function( x, subset.chemical, subset.gene,
        filter.score, main ) {

    tbl <- psygenet2r::extract( x, index_name = "gene interactions" )
    tbl <- tbl[ tbl$Reference.Count >= filter.score, ]

    if( !missing( subset.chemical ) ) {
        tbl <- tbl[ tbl$Chemical.Name %in% subset.chemical, ]
    }

    if( !missing( subset.gene ) ) {
        tbl <- tbl[ tbl$Gene.Symbol %in% subset.gene, ]
    }

    x <- tbl[ , c( "Chemical.Name", "Interaction.Actions" ) ]
    y <- tbl[ , c( "Interaction.Actions", "Gene.Symbol" ) ]
    colnames( x ) <- colnames( y ) <- c( "O", "E" )

    edges <- rbind( x, y )
    netw  <- igraph::graph.data.frame( edges, directed = FALSE )
    netw  <- igraph::simplify( netw )
    lay   <- igraph::layout_with_fr( netw )

    if( missing( main ) ) {
        main <- " "
    }

    node_color <- ifelse( igraph::V( netw )$name %in% y$E, "#FFA500", #"#A52A2A"
        ifelse( igraph::V( netw )$name %in% y$O, "#E6E6FA", "#556B2F" ) )

    igraph::plot.igraph( netw,
                         vertex.frame.color = "white",
                         layout             = lay,
                         vertex.color       = node_color,
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
