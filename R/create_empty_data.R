create_empty_data <- function(losts) {
    if(missing(losts)) {
        losts <- c()
    }
    new( "CTDdata",
         type                   = "VOID",
         timestamp              = as.character( Sys.time() ),
         terms                  = S4Vectors::DataFrame(),
         losts                  = losts,
         gene_interactions      = S4Vectors::DataFrame(),
         chemicals_interactions = S4Vectors::DataFrame(),
         diseases               = S4Vectors::DataFrame(),
         gene_gene_interactions = S4Vectors::DataFrame(),
         kegg                   = S4Vectors::DataFrame(),
         go                     = S4Vectors::DataFrame()
    )
}
