int_plot_gene_go_network <- function( x, subset.x, subset.y, main,
        ontology = c("Biological Process", "Cellular Component",
                     "Molecular Function" ) ) {
    ontology <- match.arg( base::tolower( ontology ),
        base::tolower( c("Biological Process", "Cellular Component",
                         "Molecular Function" ) ) )

    tbl <- psygenet2r::extract( x, index_name = "go terms" )
    tbl$Ontology <- base::tolower( tbl$Ontology )
    tbl <- tbl[ tbl$Ontology %in% ontology, ]

    if( !missing( subset.x ) ) {
        tbl <- tbl[ tbl$GeneSymbol %in% subset.x, ]
    }

    if( !missing( subset.y ) ) {
        tbl <- tbl[ tbl$Pathway %in% subset.y, ]
    }

    edges <- tbl[ , c( "GO.Term.Name", "GeneSymbol" ) ]
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
