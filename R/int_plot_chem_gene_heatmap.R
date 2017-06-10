int_plot_chem_gene_heatmap <- function( x, subset.chemical, subset.gene,
        filter.score = 2, max.length = 30 ) {

    tbl <- psygenet2r::extract( x, index_name = "gene interactions" )
    tbl <- tbl[ tbl$Reference.Count >= filter.score,
                c( "Chemical.Name", "Gene.Symbol", "Reference.Count" ) ]

    if( !missing( subset.chemical ) ) {
        tbl <- tbl[ tbl$Chemical.Name %in% subset.chemical, ]
    }

    if( !missing( subset.gene ) ) {
        tbl <- tbl[ tbl$Gene.Symbol %in% subset.gene, ]
    }

    tbl$Chemical.Name <- vapply( tbl$Chemical.Name, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    }, FUN.VALUE = "character" )

    tbl$Reference.Count <- as.numeric( tbl$Reference.Count )
    ggplot2::ggplot( data.frame( tbl ),
        ggplot2::aes_string( x = "Chemical.Name", y = "Gene.Symbol" ) ) +
        ggplot2::theme_bw() +
        ggplot2::geom_tile( ggplot2::aes_string( fill = "Reference.Count" ) ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
            axis.ticks = ggplot2::element_blank()
        ) +
        ggplot2::scale_fill_gradient( low = "white", high = "seagreen",
                                      name = "Reference Count" ) +
        ggplot2::xlab( "" ) + ggplot2::ylab( "" )
}
