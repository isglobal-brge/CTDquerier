int_plot_chem_disease_heatmap <- function( x, subset.chemical, subset.disease,
        field.score = "Inference", filter.score = 50, max.length = 30 ) {

    field.score <- match.arg( field.score,
        choices = c( "Inference", "Reference" ) )
    field.score <- ifelse(
        field.score == "Inference", "Inference.Score", "Reference.Count" )
    tbl <- get_table( x, index_name = "diseases" )
    tbl[ , field.score ] <- as.numeric( tbl[ , field.score ] )

    if( !missing( subset.chemical ) ) {
        tbl <- tbl[ tbl$Chemical.Name %in% subset.chemical, ]
    }

    if( !missing( subset.disease ) ) {
        tbl <- tbl[ tbl$Disease.Name %in% subset.disease, ]
    }

    tbl[ is.na( tbl[ , field.score ] ), field.score ] <- 0
    tbl <- tbl[ tbl[ , field.score ] >= filter.score,
                c( "Disease.Name", "Chemical.Name", field.score ) ]
    tbl$Disease.Name <- vapply( tbl$Disease.Name, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    }, FUN.VALUE = "character" )
    tbl$Chemical.Name <- vapply( tbl$Chemical.Name, function( name ) {
        if( nchar( name ) > max.length ) {
            paste0( substr( name, 1, 17 ), "..." )
        } else {
            name
        }
    }, FUN.VALUE = "character" )

    field.name <- gsub( "\\.", " ", field.score )
    tbl[ , field.score ] <- as.numeric( tbl[ , field.score ] )
    ggplot2::ggplot( data.frame( tbl ),
        ggplot2::aes_string( x = "Chemical.Name", y = "Disease.Name" ) ) +
        ggplot2::theme_bw() +
        ggplot2::geom_tile( ggplot2::aes_string( fill = field.score ) ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text( angle = 90, hjust = 1 ),
            axis.ticks = ggplot2::element_blank()
        ) +
        ggplot2::scale_fill_gradient( low = "white", high = "seagreen",
                                      name = field.name ) +
        ggplot2::xlab( "" ) + ggplot2::ylab( "" )
}
