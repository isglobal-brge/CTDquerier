int_plot_chem_go_heatmap <- function( x, subset.x, subset.y, filter.score, main,
        max.length = 30, ontology = c("Biological Process", "Cellular Component",
                                      "Molecular Function" ) ) {
    ontology <- match.arg( base::tolower( ontology ),
                           base::tolower( c("Biological Process", "Cellular Component",
                                            "Molecular Function" ) ) )

    tbl <- psygenet2r::extract( x, index_name = "go terms" )
    tbl$Ontology <- base::tolower( tbl$Ontology )
    tbl <- tbl[ tbl$Ontology %in% ontology, ]

    if( !missing( subset.x ) ) {
        tbl <- tbl[ tbl$ChemicalName %in% toupper( subset.x ), ]
    }

    if( !missing( subset.y ) ) {
        tbl <- tbl[ tbl$Pathway %in% subset.y, ]
    }

    if( !missing( filter.score ) ) {
        tbl <- tbl[ tbl$P.value <= filter.score, ]
    }

    tbl$GO.Term.Name <- sapply( tbl$GO.Term.Name, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    } )
    tbl$ChemicalName <- sapply( tbl$ChemicalName, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    } )

    categories <- c( 0,
                     quantile( tbl$P.value, probs =  seq(0, 1, 0.33), na.rm = TRUE )[ -4 ],
                     max( tbl$P.value ) )
    if( categories[2] == 0 ) {
        categories <- categories[-1]
        categories <- c( categories, 1 )
    }

    tbl$Go <- paste0( tbl$GO.Term.Name, " (", tbl$Highest.GO.Level, ")" )

    tbl <- data.frame( tbl )[ , c( "Go", "ChemicalName", "P.value" ) ]
    tbl$CScore <- cut( tbl$P.value,
                       breaks =  categories,
                       include.lowest = TRUE, right = TRUE )

    rgb_grad <- rev( c( "#FFFFFF", "#CAE2D5", "#96C5AB", "#62A881", "#2E8B57" ) )
    names( rgb_grad ) <- levels( tbl$CScore )


    ggplot2::ggplot( data.frame( tbl ), ggplot2::aes_string( x = "ChemicalName", y = "Go" ) ) +
        ggplot2::theme_bw() +
        ggplot2::geom_tile( ggplot2::aes_string( fill = "CScore" ) ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
            axis.ticks = ggplot2::element_blank()
        ) +
        ggplot2::scale_fill_manual( breaks = names( rgb_grad ),
                                    values = rgb_grad, name = "P-Value" ) +
        ggplot2::xlab( "" ) + ggplot2::ylab( "" )
}
