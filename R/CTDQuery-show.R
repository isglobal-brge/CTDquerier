setMethod( "show",
    signature  = "CTDdata",
    definition = function( object ) {
        cat( "Object of class 'CTDdata'\n" )
        cat( "-------------------------\n" )
        cat( " . Type:", object@type, "\n" )
        cat( " . Creation (timestamp):", object@timestamp, "\n" )
        cat( " . Length:", nrow( object@terms ), "\n")
        if( nrow( object@terms ) == 1 ) {
            it <- object@terms[ 1, 1 ]
        } else {
            it <- paste0( object@terms[ 1,  1 ], ", ..., " ,
                          object@terms[ nrow( object@terms ), 1 ] )
        }
        cat( " . Items:", it, "\n" )
        if( object@type == "GENE" ) {
            xx <- as.data.frame(
                table( !is.na( object@diseases[ , "Direct.Evidence" ] ) ) )
            cat( " . Diseases:", length( unique( object@diseases$Disease.ID )),
                 "(", xx[2,2], "/", nrow( object@diseases ), ")\n" )
            cat( " . Gene-gene interactions:",
                 nrow( unique(
                     object@gene_gene_interactions[ ,
                        c("Source.Gene.ID", "Target.Gene.ID" ) ] ) ),
                 "(", nrow( object@gene_gene_interactions ), ")\n" )
            cat( " . Gene-chemical interactions:",
                 nrow( unique(
                     object@chemicals_interactions[ ,
                        c("Chemical.ID", "GeneID" ) ] ) ),
                 "(", nrow( object@chemicals_interactions ), ")\n" )
            xx <- "GeneID"
        } else if( object@type == "CHEMICAL" ) {
            xx <- as.data.frame(
                table( object@diseases[ , "Direct.Evidence" ] != "" ) )
            cat( " . Diseases:", length( unique( object@diseases$Disease.ID )),
                 "(", xx[2,2], "/", nrow( object@diseases ), ")\n" )
            cat( " . Diseases:", length( unique( object@diseases$Disease.ID )),
                 "(", nrow( object@diseases ), ")\n" )
            cat( " . Chemical-gene interactions:",
                 nrow( unique(
                     object@gene_interactions[ ,
                        c("Chemical.ID", "Gene.ID" ) ] ) ),
                 "(", nrow( object@gene_interactions ), ")\n" )
            xx <- "ChemicalID"
        } else if( object@type == "DISEASE" ) {
            xx <- as.data.frame(
                table( object@gene_interactions[ , "Direct.Evidence" ] != "" ))
            cat( " . Disease-gene interactions:",
                 nrow( unique(
                     object@gene_interactions[ ,
                        c("Disease.ID", "Gene.ID" ) ] ) ),
                 "(", xx[2,2], "/", nrow( object@gene_interactions ), ")\n" )
            xx <- as.data.frame( table(
                object@chemicals_interactions[ , "Direct.Evidence" ] != "" ) )
            cat( " . Gene-chemical interactions:",
                 nrow( unique(
                     object@chemicals_interactions[ ,
                        c("Chemical.ID", "Disease.ID" ) ] ) ),
                 "(", xx[2,2], "/", nrow(object@chemicals_interactions),")\n")
            xx <- "Disease.ID"
        }
        if( nrow( object@kegg ) > 0 ) {
            cat( " . KEGG pathways:",
                 nrow( unique( object@kegg[ , c( "Pathway.ID", xx ) ] ) ),
                 "(", nrow( object@kegg ), ")\n" )
        } else {
            cat( " . KEGG pathways: 0 (-)\n" )
        }
        if( nrow( object@go ) > 0 ) {
            cat( " . GO terms:",
                 nrow( unique( object@go[, c( "GO.Term.ID", xx ) ] ) ),
                 "(", nrow( object@go ), ")\n" )
        } else {
            cat( " . GO terms: 0 (-)\n" )
        }
    }
)
