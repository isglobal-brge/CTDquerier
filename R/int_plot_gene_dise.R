int_plot_gene_disease_heatmap <- function( x, subset.gene, subset.disease,
        field.score = "Inference", filter.score = 50, max.length = 30 ) {
    field.score <- match.arg( field.score, choices = c( "Inference", "Reference" ) )
    field.score <- ifelse( field.score == "Inference", "Inference.Score", "Reference.Count" )
    tbl <- get_table( x, index_name = "diseases" )

    if( !missing( subset.gene ) ) {
        tbl <- tbl[ tbl$GeneSymbol %in% subset.gene, ]
    }

    if( !missing( subset.disease ) ) {
        tbl <- tbl[ tbl$Disease.Name %in% subset.disease, ]
    }

    genes <- unique( tbl$GeneSymbol )
    tbl[ is.na( tbl[ , field.score ] ), field.score ] <- 0
    tbl <- tbl[ tbl[ , field.score ] >= filter.score, ]
    tbl$Disease.Name <- vapply( tbl$Disease.Name, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    }, FUN.VALUE = "character" )
    tbl <- data.frame( tbl )[ , c( "Disease.Name", "GeneSymbol", field.score ) ]
    diseases <- unique( tbl$Disease.Name )

    field.name <- gsub( "\\.", " ", field.score )
    tbl[ , field.score ] <- as.numeric( tbl[ , field.score ] )
    if( length( diseases ) > 1 ) {
        ggplot2::ggplot( data.frame( tbl ),
            ggplot2::aes_string( x = "GeneSymbol", y = "Disease.Name" ) ) +
            ggplot2::theme_bw() +
            ggplot2::geom_tile( ggplot2::aes_string( fill = field.score ) ) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
                axis.ticks = ggplot2::element_blank()
            ) +
            ggplot2::scale_fill_gradient( low = "white", high = "steelblue",
                                          name = field.name ) +
            ggplot2::xlab( "" ) + ggplot2::ylab( "" )
    } else {
        tbl <- tbl[ order( tbl[ , field.score ], decreasing = TRUE ), ]
        lbl <- tbl$GeneSymbol[ order( tbl[ , field.score ], decreasing = TRUE ) ]
        lbl <- lbl[ !duplicated( lbl ) ]
        tbl$GeneSymbol <- factor(tbl$GeneSymbol, levels = lbl )
        ggplot2::ggplot( data.frame( tbl ),
            ggplot2::aes_string( x = "GeneSymbol", y = field.score ) ) +
            ggplot2::theme_bw() +
            ggplot2::geom_bar( stat = "identity", fill = "steelblue" ) +
            ggplot2::xlab( "" ) + ggplot2::ylab( field.name ) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 )
            )
    }
}
