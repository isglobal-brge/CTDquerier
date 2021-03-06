in_plot_base <- function( object ) {
    dta <- data.frame(
        Variable = c( "Found", "Lost" ),
        Value = c( length( object@terms$GeneSymbol ),
                   length( object@losts ) )
    )
    ggplot2::ggplot( dta, ggplot2::aes_string( x = "Variable", y = "Value", fill = "Variable" ) ) +
        ggplot2::theme_minimal() +
        ggplot2::geom_bar( stat = "identity" ) +
        ggplot2::xlab("") + ggplot2::ylab( "Count" ) +
        ggplot2::theme( legend.position = "none" ) +
        ggplot2::scale_fill_manual(
            breaks = c( "Found", "Lost" ),
            values = c( "#2E8B57", "#B22222" )
        )
}
