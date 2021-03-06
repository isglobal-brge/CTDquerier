int_plot_gene_gene_heatmap <- function( x, subset.souce, subset.target ) {
    tbl <- get_table( x, index_name = "gene-gene interactions" )
    tbl <- data.frame( tbl )[ ,
        c( "Source.Gene.Symbol", "Target.Gene.Symbol", "GeneSymbol", "Assay",
           "Interaction.Type", "Throughput" ) ]

    if( !missing( subset.souce ) ) {
        tbl <- tbl[ tbl$GeneSymbol %in% subset.souce, ]
    }

    tbl <- tbl[ tbl$Source.Gene.Symbol %in% unique( tbl$GeneSymbol ), ]
    if( !missing( subset.souce ) ) {
        tbl <- tbl[ tbl$Target.Gene.Symbol %in% subset.target, ]
    }

    ggplot2::ggplot( as.data.frame( tbl ),
        ggplot2::aes_string( x = "GeneSymbol", y = "Target.Gene.Symbol" ) ) +
        ggplot2::theme_bw() +
        ggplot2::geom_tile( ggplot2::aes_string( fill = "Assay" ) ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
            axis.ticks = ggplot2::element_blank()
        ) +
        ggplot2::facet_wrap( Interaction.Type ~ Throughput ) +
        ggplot2::xlab( "" ) + ggplot2::ylab( "" )
}
