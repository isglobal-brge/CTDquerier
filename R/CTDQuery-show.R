setMethod( "show",
  signature  = "CTDquery",
  definition = function( object ) {
    cat( "Object of class 'CTDquery'\n" )
    cat( "--------------------------------\n" )
    cat( " . Type:", object@type, "\n" )
    cat( " . Length:", nrow( object@terms ), "\n")
    if( nrow( object@terms ) == 1 ) {
     it <- object@terms[ 1, 1 ]
    } else {
     it <- paste0( object@terms[ 1,  1 ], ", ..., " , object@terms[ nrow( object@terms ), 1 ] )
    }
    cat( " . Items:", it, "\n" )
    cat( " . #Diseases:", nrow( object@diseases ), "\n" )
    if( object@type == "GENE" ) {
      cat( " . #Gene-gene interactions:", nrow( object@gene_gene_interactions ), "\n" )
      cat( " . #Gene-chemical interactions:", nrow( object@chemicals_interactions ), "\n" )
    } else if( object@type == "CHEMICAL" ) {
      cat( " . #Chemical-gene interactions:", nrow( object@gene_interactions ), "\n" )
    }
    cat( " . #KEGG pathways:", nrow( object@kegg ), "\n" )
    cat( " . #GO terms:", nrow( object@go ), "\n" )
 }
)
