#' @describeIn CTDquery MEthod to perform enrichment analysis given two
#' object of class \code{CTDquery}.
#' @note The pairs of \code{CTDquery} objects given to x and y must follow a
#' specific pattern: gene-disease, gene-chemical or disease-chemical.
setMethod(
    f = "enrich",
    signature = "CTDquery",
    definition = function( x, y, ... ) {
        if( x@type == "GENE" ) {
            if( y@type == "DISEASE" | y@type == "CHEMICAL" ) {
                this_gene <- get_terms( x )[[ "found" ]]
                inthis_gene <- psygenet2r::extract( y, index_name = "gene interactions" )
                inthis_gene <- unique( inthis_gene$Gene.Symbol )

                download_ctd_genes()
                all_genes <- unique( load_ctd_gene()$GeneSymbol )

                mtr <- matrix( c(
                    length( intersect( this_gene, inthis_gene ) ),
                    sum( !this_gene %in% inthis_gene ),
                    length( inthis_gene ) - length( intersect( this_gene, inthis_gene ) ),
                    length( all_genes ) - length( inthis_gene )
                ), ncol = 2, byrow = TRUE )
            } else {
                stop( "Invalid argument 'y'. Only 'disease' or 'chemical' ",
                      "can be paired with 'gene' query.")
            }

        } else if ( x@type == "DISEASE" ) {
            if( y@type == "CHEMICAL" ) {
                this_gene <- sygenet2r::extract( y, index_name = "gene interactions" )
                inthis_gene <- psygenet2r::extract( y, index_name = "gene interactions" )
                this_gene <- unique( this_gene$Gene.Symbol )
                inthis_gene <- unique( inthis_gene$Gene.Symbol )

                download_ctd_genes()
                all_genes <- unique( load_ctd_gene()$GeneSymbol )

                mtr <- matrix( c(
                    length( intersect( this_gene, inthis_gene ) ),
                    sum( !this_gene %in% inthis_gene ),
                    length( inthis_gene ) - length( intersect( this_gene, inthis_gene ) ),
                    length( all_genes ) - length( inthis_gene )
                ), ncol = 2, byrow = TRUE )

            } else {
                stop( "Invalid argument 'y'. Only 'chemical' can be paired ",
                    "with 'disease' query.")
            }
        }

        fisher.test( mtr, alternative = "greater" )
    }
)
