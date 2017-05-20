setMethod( "show",
  signature  = "CTDquery",
  definition = function( object ) {
    cat( "Object of class 'CTDquery'\n" )
    cat( "--------------------------------\n" )
    cat( " . Type:", object@type, "\n" )
    cat( " . Length:", nrow( object@objects ), "\n")
    if( nrow( object@objects ) == 1 ) {
     it <- object@objects[ 1, 1 ]
    } else {
     it <- paste0( object@objects[ 1,  1 ], " ... " , object@objects[ nrow( object@objects ) - 1 , 1 ] )
    }
    cat( " . Items:", it, "\n" )
    cat( " . #Diseases:", nrow( object@diseases ), "\n" )
    if( object@type == "GENE" ) {
      cat( " . #Gene-gene interactions:", nrow( object@gene_gene_interactions ), "\n" )
      cat( " . #Gene-chemical interactions:", nrow( object@chemicals_interactions ), "\n" )
      cat( " . #KEGG pathways:", nrow( object@kegg ), "\n" )
      cat( " . #GO terms:", nrow( object@go ), "\n" )
    }
 }
)
