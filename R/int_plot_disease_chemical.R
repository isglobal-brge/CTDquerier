int_plot_disease_chemical_heatmap <- function( x, subset.x, subset.y,
        field.score = "Inference", filter.score = 20, max.length = 30 ) {
    field.score <- match.arg( field.score, choices = c( "Inference", "Reference" ) )
    field.score <- ifelse( field.score == "Inference", "Inference.Score", "Reference.Count" )
    tbl <- psygenet2r::extract( x, index_name = "chemical interactions" )
    tbl <- tbl[ tbl$Reference.Count >= filter.score, ]

    if( !missing( subset.x ) ) {
        tbl <- tbl[ tbl$Disease.Name %in% subset.x, ]
    }

    if( !missing( subset.y ) ) {
        tbl <- tbl[ tbl$Chemical.Name %in% subset.y, ]
    }

    diseases <- unique( tbl$Disease.Name )
    tbl[ is.na( tbl[ , field.score ] ), field.score ] <- 0
    tbl <- tbl[ tbl[ , field.score ] >= filter.score, ]
    tbl$Chemical.Name <- sapply( tbl$Chemical.Name, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    } )
    tbl <- data.frame( tbl )[ , c( "Chemical.Name", "Disease.Name", field.score ) ]
    chemicals <- unique( tbl$Chemical.Name )

    field.name <- gsub( "\\.", " ", field.score )
    tbl[ , field.score ] <- as.numeric( tbl[ , field.score ] )
    if( length( diseases ) > 1 ) {
        ggplot2::ggplot( data.frame( tbl ), ggplot2::aes_string( x = "Disease.Name", y = "Chemical.Name" ) ) +
            ggplot2::theme_bw() +
            ggplot2::geom_tile( ggplot2::aes_string( fill = field.score ) ) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
                axis.ticks = ggplot2::element_blank(),
                panel.background = ggplot2::element_rect (fill = "white", colour = "white" ),
                panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank()
            ) +
            ggplot2::scale_fill_gradient( low = "white", high = "darkorange", name = "Reference Count" ) +
            ggplot2::xlab( "" ) + ggplot2::ylab( "" )
    } else {
        tbl <- tbl[ order( tbl[ , field.score ], decreasing = TRUE ), ]
        lbl <- tbl$Chemical.Name[ order( tbl[ , field.score ], decreasing = TRUE ) ]
        lbl <- lbl[ !duplicated( lbl ) ]
        tbl$Chemical.Name <- factor(tbl$Chemical.Name, levels = lbl )
        ggplot2::ggplot( data.frame( tbl ), ggplot2::aes_string( x = "Chemical.Name", y = field.score ) ) +
            ggplot2::theme_bw() +
            ggplot2::geom_bar( stat = "identity", fill = "darkorange" ) +
            ggplot2::xlab( "" ) + ggplot2::ylab( field.name ) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 )
            )
    }
}
