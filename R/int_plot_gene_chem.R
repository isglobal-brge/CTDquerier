int_plot_gene_chemical_heatmap <- function( x, subset.gene, subset.chemical,
        filter.score = 20, max.length = 30 ) {
    tbl <- get_table( x, index_name = "chemical interactions" )

    if( !missing( subset.gene ) ) {
        tbl <- tbl[ tbl$GeneSymbol %in% subset.gene, ]
    }

    if( !missing( subset.chemical ) ) {
        tbl <- tbl[ tbl$Chemical.Name %in% subset.chemical, ]
    }

    genes <- unique( tbl$GeneSymbol )
    tbl[ is.na( tbl[ , "Reference.Count" ] ), "Reference.Count" ] <- 0
    tbl <- tbl[ tbl[ , "Reference.Count" ] >= filter.score, ]
    tbl$Chemical.Name <- vapply( tbl$Chemical.Name, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    }, FUN.VALUE = "character" )
    tbl <- data.frame( tbl )[ , c( "Chemical.Name", "GeneSymbol", "Reference.Count" ) ]
    chemicals <- unique( tbl$Chemical.Name )
    tbl$Reference.Count <- as.numeric( tbl$Reference.Count )
    
    tbl <- do.call( rbind, lapply( unique( tbl$GeneSymbol ), function( gn ) {
        sub <- tbl[ tbl$GeneSymbol == gn, ]
        new <- data.frame( "GeneSymbol" = gn,
                           "Chemical.Name" = unique( sub$Chemical.Name ),
                           "Reference.Count" = 0,
                           stringsAsFactors = FALSE
        )
        new$Reference.Count <- vapply( new$Chemical.Name, function( ch ) {
            max( sub[ sub$Chemical.Name == ch, "Reference.Count" ] )
        }, FUN.VALUE = 1.1 )
        new
    } ) )

    if( length( chemicals ) > 1 ) {
        ggplot2::ggplot( data.frame( tbl ),
            ggplot2::aes_string( x = "GeneSymbol", y = "Chemical.Name" ) ) +
            ggplot2::theme_bw() +
            ggplot2::geom_tile( ggplot2::aes_string( fill = "Reference.Count" ) ) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
                axis.ticks = ggplot2::element_blank()
            ) +
            ggplot2::scale_fill_gradient( low = "white", high = "steelblue",
                                          name = "Reference Count" ) +
            ggplot2::xlab( "" ) + ggplot2::ylab( "" )
    } else {
        tbl <- tbl[ order( tbl$Reference.Count, decreasing = TRUE ), ]
        lbl <- tbl$GeneSymbol[ order( tbl$Reference.Count, decreasing = TRUE ) ]
        lbl <- lbl[ !duplicated( lbl ) ]
        tbl$GeneSymbol <- factor(tbl$GeneSymbol, levels = lbl )
        ggplot2::ggplot( data.frame( tbl ),
            ggplot2::aes_string( x = "GeneSymbol", y = "Reference.Count" ) ) +
            ggplot2::theme_bw() +
            ggplot2::geom_bar( stat = "identity", fill = "steelblue" ) +
            ggplot2::xlab( "" ) + ggplot2::ylab( "Reference.Count" ) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 )
            ) + ggplot2::ggtitle( chemicals )
    }
}
