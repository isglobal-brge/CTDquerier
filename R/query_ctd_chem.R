#' Function to query CTDbase using gene terminology ( Gene Symbol )
#'
#' This function cheks for CTDbase gene vocabulary and query CTDbase
#' for each one, downloading (...)
#'
#' @param terms Character vector with the chemicals used in the query.
#' @param filename (default \code{"CTD_chemicals.tsv.gz"}) Name of the file
#' to store the CTDbase chemicals vocabilary.
#' @param mode (default \code{"auto"}) Mode passed to \code{download.file}.
#' @param max.distance (default \code{10}) Maximum distance allowed between a given element
#' in \code{terms} argument and a possible match in CTDbase.
#' @param verbose (default \code{FALSE}) If set to \code{TRUE} is shows relevant
#' information of each step.
#' @return
#' @example s
#' rst <- query_ctd_gene( terms = c("APP", "HMOX1A", "hmox1" ), verbose = TRUE )
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
  disc <- c()
  if( verbose ) {
    message( "Computing hamming distance for each input chemical term." )
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

  if( length( disc ) != 0 ) {
    warning( length( disc ) , "/", length( terms ), " terms were discarted." )
  }
  ## //
}
