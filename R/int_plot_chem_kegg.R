int_plot_chem_kegg_heatmap <- function( x, subset.chemical, subset.pathway,
        filter.score, max.length = 30 ) {
    tbl <- psygenet2r::extract( x, index_name = "kegg pathways" )

    if( !missing( subset.chemical ) ) {
        tbl <- tbl[ tbl$ChemicalName %in% base::toupper( subset.chemical ), ]
    }

    if( !missing( subset.pathway ) ) {
        tbl <- tbl[ base::toupper( tbl$Pathway ) %in%
                        base::toupper( subset.pathway ), ]
    }

    if( !missing( filter.score ) ) {
        tbl <- tbl[ tbl$P.value <= filter.score, ]
    }

    tbl$Pathway <- vapply( tbl$Pathway, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    }, FUN.VALUE = "character" )
    tbl$ChemicalName <- vapply( tbl$ChemicalName, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    }, FUN.VALUE = "character" )

    categories <- c( 0,
        quantile( tbl$P.value, probs =  seq(0, 1, 0.33), na.rm = TRUE )[ -4 ],
        max( tbl$P.value ) )

    tbl <- data.frame( tbl )[ , c( "Pathway", "ChemicalName", "P.value" ) ]
    tbl$CScore <- cut( tbl$P.value,
        breaks =  categories,
        include.lowest = TRUE, right = TRUE )

    rgb_grad <- rev( c( "#FFFFFF", "#CAE2D5", "#96C5AB", "#62A881", "#2E8B57" ) )
    names( rgb_grad ) <- levels( tbl$CScore )

    ggplot2::ggplot( data.frame( tbl ), ggplot2::aes_string( x = "ChemicalName", y = "Pathway" ) ) +
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
