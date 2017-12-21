#' Function to create a leaf plot
#'
#' This functions taked a \code{data.frame} and returns a \code{gtable}
#' with three plots. The left-leafes, the axis names and the right-leafes.
#'
#' @param dta \code{data.frame} with, at last, three columns corresponding
#' to the axis labels, the left values and the right values.
#' @param label (default \code{"name"}) Name of the column in \code{dta}
#' with the labels.
#' @param valueLeft (default \code{"var1"}) Name of the column with the
#' values for the left plot.
#' @param valueRight (default \code{"var2"}) Name of the column with the
#' values for the right plot.
#' @param titleLeft (default \code{NULL}) Character used as a title for the
#' left plot.
#' @param titleRight (default \code{NULL}) Character used as a title for the
#' right plot.
#' @param colorLeft (default \code{"#FF7F50"}) Color for left plot bars.
#' @param colorRight (default \code{"#20B2AA"}) Color for right plot bars.
#' @return A ggplo2 object.
#' @examples
#' data <- data.frame(
#'    labels = LETTERS[1:15],
#'    right = runif(n = 15) * 11,
#'    left = runif(n = 15) * 9
#' )
#' leaf_plot( data, "labels", "left", "right", "runif09", "runif11")
#' @export
leaf_plot <- function( dta, label = "name", valueLeft = "var1",
        valueRight = "var2", titleLeft = NULL, titleRight = NULL,
        colorLeft = "#FF7F50", colorRight = "#20B2AA") {

    g.mid <- ggplot2::ggplot( data = dta,
            ggplot2::aes_string( x = 1,y = label ) ) +
        ggplot2::geom_text( ggplot2::aes_string(label = label ) ) +
        # ggplot2::geom_segment(
        #     ggplot2::aes_string( x = 0.94, xend = 0.95, yend = label ) )+
        # ggplot2::geom_segment(
        #     ggplot2::aes_string( x=1.05, xend=1.065, yend = label ) ) +
        ggplot2::ggtitle( "" ) +
        ggplot2::ylab( NULL ) +
        ggplot2::scale_x_continuous(
            expand = c( 0, 0 ), limits = c( 0.94, 1.065 ) ) +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text( color = NA ),
            axis.ticks.x = ggplot2::element_line( color = NA ),
            plot.margin = grid::unit( c( 1, -1, 1, -1 ), "mm" )
        )

    mm <- max( c( dta[ , valueLeft ], dta[ , valueRight ] ) )

    g1 <- ggplot2::ggplot( data = dta,
            ggplot2::aes_string(x = label, y = valueLeft ) ) +
        ggplot2::geom_bar( stat = "identity", fill = colorLeft ) +
        ggplot2::ggtitle( titleLeft ) +
        ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            plot.margin = grid::unit( c( 1, -1, 1, 0 ), "mm" ),
            plot.title = ggplot2::element_text(hjust = 0.5)
        ) +
        ggplot2::scale_y_reverse( limits = c( mm , 0 ) ) +
        ggplot2::coord_flip()

    g2 <- ggplot2::ggplot( data = dta,
            ggplot2::aes_string( x = label, y = valueRight ) ) +
        ggplot2::xlab( NULL ) +
        ggplot2::geom_bar( stat = "identity", fill = colorRight ) +
        ggplot2::ggtitle( titleRight ) +
        ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            plot.margin = grid::unit( c( 1, 0, 1, -1 ), "mm" ),
            plot.title = ggplot2::element_text(hjust = 0.5)
        ) +
        ggplot2::scale_y_continuous( limits = c( 0 , mm ) ) +
        ggplot2::coord_flip()

    gg1 <- ggplot2::ggplot_gtable( ggplot2::ggplot_build( g1 ) )
    gg2 <- ggplot2::ggplot_gtable( ggplot2::ggplot_build( g2 ) )
    gg.mid <- ggplot2::ggplot_gtable( ggplot2::ggplot_build( g.mid ) )

    gridExtra::grid.arrange( gg1, gg.mid, gg2, ncol = 3,
        widths = c( 4/9, 1/9, 4/9 ) )
}
