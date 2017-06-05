int_plot_gene_gene_heatmap <- function( x, subset.x, subset.y ) {
  tbl <- psygenet2r::extract( x, index_name = "gene-gene interactions" )
  tbl <- data.frame( tbl )[ , c( "Source.Gene.Symbol", "Target.Gene.Symbol", "GeneSymbol", "Assay", "Interaction.Type", "Throughput" ) ]

  if( !missing( subset.x ) ) {
    tbl <- tbl[ tbl$GeneSymbol %in% subset.x, ]
  }

  tbl <- tbl[ tbl$Source.Gene.Symbol %in% unique( tbl$GeneSymbol ), ]
  if( !missing( subset.y ) ) {
    tbl <- tbl[ tbl$Target.Gene.Symbol %in% subset.y, ]
  }

  field.name <- gsub( "\\.", " ", field.score )
  ggplot2::ggplot( as.data.frame( tbl ), ggplot2::aes_string( x = "GeneSymbol", y = "Target.Gene.Symbol" ) ) +
    ggplot2::theme_bw() +
    ggplot2::geom_tile( ggplot2::aes_string( fill = "Assay" ) ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
      axis.ticks = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect (fill = "white", colour = "white" ),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::facet_wrap(  Interaction.Type ~ Throughput ) +
    ggplot2::xlab( "" ) + ggplot2::ylab( "" )
}
