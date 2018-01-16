#' Function to query CTDbase using chemical terminology ( Chemical Name )
#'
#' This function cheks for CTDbase gene vocabulary and query CTDbase
#' for each one, downloading chemica-genes interactions, associated
#' diseases, associated KEGG pathways and associated GO terms.
#'
#' @param terms Character vector with the chemicals used in the query.
#' @param filename (default \code{"CTD_chemicals.tsv.gz"}) Name of the file
#' to store the CTDbase chemicals vocabilary.
#' @param mode (default \code{"auto"}) Mode passed to \code{download.file}.
#' @param max.distance (default \code{10}) Maximum distance allowed between a given element
#' in \code{terms} argument and a possible match in CTDbase.
#' @param verbose (default \code{FALSE}) If set to \code{TRUE} is shows relevant
#' information of each step.
#' @return An object of class \code{\link{CTDdata}}.
#' @examples
#' rst <- query_ctd_chem( terms = c( "Iron", "Air Pollutants" ), verbose = TRUE )
#' @export query_ctd_chem
query_ctd_chem <- function( terms, filename = "CTD_chemicals.tsv.gz", mode = "auto", max.distance = 10, verbose = FALSE ) {
  ## SETUP
  rst <- download_ctd_chem( filename, mode, verbose )
  if( rst == "" ) {
    stop( "CTDquerier was not ablte to download '", filename, "' from CTDbase." )
  }

  if( verbose ) {
    message( "Loading chemical vocabulary." )
  }
  tbl <- load_ctd_chem( filename )
  ## //

  ## VALIDATE INPUT CHEMICALS
  terms <- toupper( terms )
  keep <- tbl[ 1, ]; keep <- keep[ -1, ]
  disc <- character()
  if( verbose ) {
    message( "Computing lcs distance for each input chemical term." )
  }
  for( tt in terms ) {
    dist <- data.frame( Chemical = tbl$ChemicalName,
      DistanceName = stringdist::stringdist( tbl$ChemicalName, tt, method = "lcs" ),
      DistanceSynonyms = stringdist::stringdist( tbl$Synonyms, tt, method = "lcs" )
    )
    if( min( dist$DistanceName ) == 0 ) {
      if( verbose ) {
        message( "Perfect match for term '", tt, "' in chemical names.")
      }
      sel <- dist[ which( dist$DistanceName  == 0 ), ]
      keep <- rbind( keep, tbl[ as.numeric( rownames( sel ) ), ] )
    } else if( min( dist$DistanceSynonyms ) == 0 ) {
      if( verbose ) {
        message( "Perfect match for term '", tt, "' in chemical synonyms.")
      }
      sel <- dist[ which( dist$DistanceSynonyms  == 0 ), ]
      keep <- rbind( keep, tbl[ as.numeric( rownames( sel ) ), ] )
    } else if( min( dist$DistanceName ) < max.distance ) {
      tmp <- dist[ dist$DistanceName < max.distance, ]
      sel <- tmp[ order( tmp$DistanceName ),  ][ 1 , ]
      keep <- rbind( keep, tbl[ as.numeric( rownames( sel ) ), ] )
      rm( tmp )
      if( verbose ) {
        message( "No perfect match for term '", tt, "', taking term '", sel$Chemical, "' as new term." )
      }
    } else if( min( dist$DistanceSynonyms ) < max.distance ) {
      tmp <- dist[ dist$DistanceSynonyms < max.distance, ]
      sel <- tmp[ order( tmp$DistanceSynonyms ),  ][ 1 , ]
      keep <- rbind( keep, tbl[ as.numeric( rownames( sel ) ), ] )
      rm( tmp )
      if( verbose ) {
        message( "No perfect match for term '", tt, "', taking synonyms '", sel$Chemical, "' as new term." )
      }
    } else {
      disc <- c( disc, tt )
    }
  }
  keep$Acc <- gsub( "MESH:", "", keep[ , 2 ] )

  if( length( disc ) != 0 ) {
    warning( length( disc ) , "/", length( terms ), " terms were discarted." )
  }
  ## //

  chem_gene_interactions <- prepare_ctd_chem( "chem_gene_interactions" )
  chem_diseases <- prepare_ctd_chem( "chem_diseases" )
  chem_go <- prepare_ctd_chem( "chem_go" )
  chem_kegg <- prepare_ctd_chem( "chem_kegg" )
  for( ii in 1:nrow( keep ) ) {
    if( verbose ) {
      message( "Staring query for chemical '", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
    }

    ## PREPARE URLs
    url_disease <- get_ctd_url(
      index    = "chemical_disease",
      term     = keep[ ii, 10 ],
      category = "chem"
    )
    url_kegg <- get_ctd_url(
      index    = "chemical_kegg",
      term     = keep[ ii, 10 ],
      category = "chem"
    )
    url_go <- get_ctd_url(
      index    = "chemical_go",
      term     = keep[ ii, 10 ],
      category = "chem"
    )
    url_genes <- get_ctd_url(
      index    = "chemical_gene_interaction",
      term     = keep[ ii, 10 ],
      category = "chem"
    )
    ## //

    # DOWNLOAD CONTENT
    if( verbose ) {
      message( " . Downloading 'disease' table." )
    }
    tmp <- download_url( url = url_disease )
    if( nrow( tmp ) > 0 ) {
      tmp$ChemicalName <- keep[ ii, 1 ]
      tmp$ChemicalID <- keep[ ii, 2 ]
      chem_diseases <- rbind( chem_diseases, tmp )
    } else if( verbose ) {
      message(" . . No 'disease' table available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
    }

    if( verbose ) {
      message( " . Downloading 'chemical-gene interation' table." )
    }
    tmp <- download_url( url = url_genes )
    if( nrow( tmp ) > 0 ) {
      tmp$ChemicalName <- keep[ ii, 1 ]
      tmp$ChemicalID <- keep[ ii, 2 ]
      chem_gene_interactions <- rbind( chem_gene_interactions, tmp )
    } else if( verbose ) {
      message(" . . No 'chemical-gene interation' table available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
    }

    if( verbose ) {
      message( " . Downloading 'GO terms' table." )
    }
    tmp <- download_url( url = url_go )
    if( nrow( tmp ) > 0 ) {
      tmp$ChemicalName <- keep[ ii, 1 ]
      tmp$ChemicalID <- keep[ ii, 2 ]
      chem_go <- rbind( chem_go, tmp )
    } else if( verbose ) {
      message(" . . No 'GO terms' table available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
    }

    if( verbose ) {
      message( " . Downloading 'KEGG pathways' table." )
    }
    tmp <- download_url( url = url_kegg )
    if( nrow( tmp ) > 0 ) {
      tmp$ChemicalName <- keep[ ii, 1 ]
      tmp$ChemicalID <- keep[ ii, 2 ]
      chem_kegg <- rbind( chem_kegg, tmp )
    } else if( verbose ) {
      message(" . . No 'KEGG pathways' table available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
    }
    # //
  }

  new( "CTDdata",
    type                   = "CHEMICAL",
    terms                  = S4Vectors::DataFrame( keep ),
    losts                  = disc,
    gene_interactions      = chem_gene_interactions,
    chemicals_interactions = S4Vectors::DataFrame(),
    diseases               = chem_diseases,
    gene_gene_interactions = S4Vectors::DataFrame(),
    kegg                   = chem_kegg,
    go                     = chem_go
  )
}



prepare_ctd_chem <- function( set ) {
  if( set == "chem_gene_interactions" ) {
    chem_gene_interactions <- rbind( 1:9 )
    colnames( chem_gene_interactions ) <- c( "Chemical Name", "Chemical ID", "CAS RN", "Gene Symbol", "Gene ID", "Interaction", "Interaction Actions", "Reference Count", "Organism Count" )
    return( S4Vectors::DataFrame(chem_gene_interactions[ -1, ] ) )
  }

  if( set == "chem_diseases" ) {
    chem_diseases <- rbind( 1:9 )
    colnames( chem_diseases ) <- c( "Chemical Name", "Chemical ID", "CAS RN", "Disease Name", "Disease ID", "Direct Evidence", "Inference Network", "Inference Score", "Reference Count" )
    return( S4Vectors::DataFrame( chem_diseases[ -1, ] ) )
  }

  if( set == "chem_kegg" )  {
    chem_kegg <- rbind( 1:7 )
    colnames( chem_kegg ) <- c( "Pathway", "Pathway ID", "P-value", "Corrected P-value", "Annotated Genes Quantity", "Annotated Genes", "Genome Frequency" )
    return( S4Vectors::DataFrame( chem_kegg[ -1, ] ) )
  }

  if( set == "chem_go" ) {
    gene_go <- rbind( 1:9 )
    colnames( gene_go ) <- c( "Ontology", "Highest GO Level", "GO Term Name", "GO Term ID", "P-value", "Corrected P-value", "Annotated Genes Quantity", "Annotated Genes", "Genome Frequency" )
    return( S4Vectors::DataFrame( gene_go[ -1, ] ) )
  }

  stop( "[INTERNAL] Invalid selected set." )
}
