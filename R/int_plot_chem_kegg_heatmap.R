int_plot_chem_kegg_heatmap <- function( x, subset.x, subset.y, max.length = 30 ) {
  tbl <- psygenet2r::extract( x, index_name = "kegg pathways" )

  if( !missing( subset.x ) ) {
    tbl <- tbl[ tbl$GeneSymbol %in% subset.x, ]
  }

  if( !missing( subset.y ) ) {
    tbl <- tbl[ tbl$Pathway %in% subset.y, ]
  }


  tbl$Pathway <- sapply( tbl$Pathway, function( name ) {
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

  chemicals <- unique( tbl$ChemicalName )
  tbl <- data.frame( tbl )[ , c( "Pathway", "ChemicalName" ) ]
  tbl$Score <- 0
  pathways <- unique( tbl$Pathway )
  for( gg in chemicals ) {
    for( pp in pathways ) {
      if( nrow( tbl[ tbl$ChemicalName == gg & tbl$Pathway == pp,  ] ) == 0 ) {
        tbl <- rbind(tbl, c( pp, gg, 0 ) )
      } else {
        tbl[ tbl$ChemicalName == gg & tbl$Pathway == pp, "Score" ] <- 1
      }
    }
  }
  rm( gg, pp )

  ggplot2::ggplot( data.frame( tbl ), ggplot2::aes_string( x = "ChemicalName", y = "Pathway" ) ) +
    ggplot2::theme_bw() +
    ggplot2::geom_tile( ggplot2::aes_string( fill = "Score" ) ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
      axis.ticks = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect (fill = "white", colour = "white" ),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::scale_fill_manual( values = c( "white", "darkgreen" ) ) +
    ggplot2::xlab( "" ) + ggplot2::ylab( "" )
}
