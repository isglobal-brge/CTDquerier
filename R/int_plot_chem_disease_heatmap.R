int_plot_chem_disease_heatmap <- function( x, subset.x, subset.y, field.score = "Inference", filter.score = 50, max.length = 30 ) {
  field.score <- match.arg( field.score, choices = c( "Inference", "Reference" ) )
  field.score <- ifelse( field.score == "Inference", "Inference.Score", "Reference.Count" )
  tbl <- psygenet2r::extract( x, index_name = "diseases" )
  tbl[ , field.score ] <- as.numeric( tbl[ , field.score ] )

  if( !missing( subset.x ) ) {
    tbl <- tbl[ tbl$GeneSymbol %in% subset.x, ]
  }

  if( !missing( subset.y ) ) {
    tbl <- tbl[ tbl$Disease.Name %in% subset.y, ]
  }

  tbl[ is.na( tbl[ , field.score ] ), field.score ] <- 0
  tbl <- tbl[ tbl[ , field.score ] >= filter.score, ]
  tbl$Disease.Name <- sapply( tbl$Disease.Name, function( name ) {
    if( nchar( name ) > max.length ) {
      paste0( substr( name, 1, 17 ), "..." )
    } else {
      name
    }
  } )
  tbl$Chemical.Name <- sapply( tbl$Chemical.Name, function( name ) {
    if( nchar( name ) > max.length ) {
      paste0( substr( name, 1, 17 ), "..." )
    } else {
      name
    }
  } )

  chemicals <- unique( tbl$Chemical.Name )
  tbl <- data.frame( tbl )[ , c( "Disease.Name", "Chemical.Name", field.score ) ]
  diseases <- unique( tbl$Disease.Name )
  for( cc in chemicals ) {
    for( dd in diseases ) {
      if( nrow( tbl[ tbl$Disease.Name == dd & tbl$Chemical.Name == cc,  ] ) == 0 ) {
        tbl <- rbind(tbl, c( dd, cc, 0 ) )
      }
    }
  }
  rm( cc, dd )
  field.name <- gsub( "\\.", " ", field.score )
  tbl[ , field.score ] <- as.numeric( tbl[ , field.score ] )
  ggplot2::ggplot( data.frame( tbl ), ggplot2::aes_string( x = "Chemical.Name", y = "Disease.Name" ) ) +
    ggplot2::theme_bw() +
    ggplot2::geom_tile( ggplot2::aes_string( fill = field.score ) ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
      axis.ticks = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect (fill = "white", colour = "white" ),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_gradient( low = "white", high = "seagreen", name = field.name ) +
    ggplot2::xlab( "" ) + ggplot2::ylab( "" )
}
