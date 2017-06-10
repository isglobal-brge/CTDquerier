int_plot_dise_gene_heatmap <- function( x, subset.disease, subset.gene,
        field.score = "Inference", filter.score = 20, max.length = 30 ) {
    field.score <- match.arg( field.score, choices = c( "Inference", "Reference" ) )
    field.score <- ifelse( field.score == "Inference", "Inference.Score", "Reference.Count" )

    tbl <- psygenet2r::extract( x, index_name = "gene interactions" )
    tbl <- tbl[ !is.na( tbl[ , field.score ] ), ]
    tbl <- tbl[ tbl[ , field.score ] >= filter.score, ]

    if( !missing( subset.disease ) ) {
        tbl <- tbl[ tbl$Disease.Name %in% subset.disease, ]
    }

    if( !missing( subset.gene ) ) {
        tbl <- tbl[ tbl$Gene.Symbol %in% subset.gene, ]
    }

    tbl$Disease.Name <- vapply( tbl$Disease.Name, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    }, FUN.VALUE = "character" )

    tbl <- data.frame( tbl )[ , c( "Disease.Name", "Gene.Symbol", field.score ) ]
    tbl[ , field.score ] <- as.numeric( tbl[ , field.score ] )
    ggplot2::ggplot( data.frame( tbl ),
        ggplot2::aes_string( x = "Disease.Name", y = "Gene.Symbol" ) ) +
        ggplot2::theme_bw() +
        ggplot2::geom_tile( ggplot2::aes_string( fill = field.score ) ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
            axis.ticks = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect (fill = "white", colour = "white" ),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::scale_fill_gradient( low = "white", high = "darkorange",
                                      name = "Reference Count" ) +
        ggplot2::xlab( "" ) + ggplot2::ylab( "" )
}
