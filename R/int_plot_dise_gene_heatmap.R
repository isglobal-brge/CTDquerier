int_plot_dise_gene_heatmap <- function( x, subset.x, subset.y, filter.score = 20, max.length = 30 ) {
    tbl <- psygenet2r::extract( x, index_name = "gene interactions" )
    tbl <- tbl[ tbl$Reference.Count >= filter.score, ]

    if( !missing( subset.x ) ) {
        tbl <- tbl[ tbl$Gene.Symbol %in% subset.x, ]
    }

    if( !missing( subset.y ) ) {
        tbl <- tbl[ tbl$Disease.Name %in% subset.y, ]
    }

    tbl$Disease.Name <- sapply( tbl$Disease.Name, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    } )

    chemicals <- unique( tbl$Chemical.Name )
    tbl <- data.frame( tbl )[ , c( "Disease.Name", "Gene.Symbol", "Reference.Count" ) ]
    tbl$Reference.Count <- as.numeric( tbl$Reference.Count )
    ggplot2::ggplot( data.frame( tbl ), ggplot2::aes_string( x = "Disease.Name", y = "Gene.Symbol" ) ) +
        ggplot2::theme_bw() +
        ggplot2::geom_tile( ggplot2::aes_string( fill = "Reference.Count" ) ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
            axis.ticks = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect (fill = "white", colour = "white" ),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::scale_fill_gradient( low = "white", high = "darkorange", name = "Reference Count" ) +
        ggplot2::xlab( "" ) + ggplot2::ylab( "" )
}
