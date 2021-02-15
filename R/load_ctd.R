#' Function to load the \code{.tsv.gz} file for genes
#'
#' This function works in pair with \code{\link{download_ctd_genes}}. This
#' function loads into the R session the downloaded \code{"CTD_genes.tsv.gz"}
#' file.
#'
#' @details The field included in the file (\code{CTD_genes.tsv.gz}) are:
#' \enumerate{
#'  \item GeneSymbol
#'  \item GeneName
#'  \item GeneID (NCBI Gene identifier)
#'  \item AltGeneIDs (alternative NCBI Gene identifiers; '|'-delimited list)
#'  \item Synonyms ('|'-delimited list)
#'  \item BioGRIDIDs ('|'-delimited list)
#'  \item PharmGKBIDs ('|'-delimited list)
#'  \item UniprotIDs ('|'-delimited list)
#' }
#'
#' @param verbose (default \code{FALSE}) If set to \code{TRUE} messages and
#' warnings are raised.
#' @return A \code{data.frame} with the content of the file "CTD_genes.tsv.gz"
#' @examples
#' download_ctd_genes()
#' fdl <- load_ctd_gene()
#' dim( fdl )
#' @export load_ctd_gene
load_ctd_gene <- function( verbose = FALSE ) {
    bfc <- .get_cache( verbose, ask = FALSE )
    if( nrow( BiocFileCache::bfcquery( bfc, "CTD_genes" ) ) != 1 ) {
        warning( "GENE vocabulary could not be loaded." )
        return(data.frame())
    } else {
        filename <- BiocFileCache::bfcrpath( bfc, "CTD_genes" )
    }

    tbl <- tryCatch( {
        read.delim( filename, header = FALSE, comment.char = "#", sep = "\t",
                    stringsAsFactors = FALSE )
    }, error = function( e ) 1 )

    if( class( tbl ) == "data.frame" ) {
        colnames( tbl ) <-
            c( "GeneSymbol", "GeneName", "GeneID", "AltGeneIDs", "Synonyms",
               "BioGRIDIDs", "PharmGKBIDs", "UniprotIDs" )
        tbl <- tbl[ !is.na( tbl$GeneSymbol ), ]
        tbl$GeneSymbol <- toupper( tbl$GeneSymbol )
        tbl
    } else {
        data.frame()
    }
}

#' Function to load the \code{.tsv.gz} file for chemicals
#'
#' @details The field included in the file (\code{CTD_chemicals.tsv.gz}) are:
#' \enumerate{
#'  \item ChemicalName
#'  \item ChemicalID (MeSH identifier)
#'  \item CasRN (CAS Registry Number, if available)
#'  \item Definition
#'  \item ParentIDs (identifiers of the parent terms; '|'-delimited list),
#'  \item TreeNumbers (identifiers of the chemical's nodes; '|'-delimited list),
#'  \item ParentTreeNumbers (identifiers of the parent nodes; '|'-delimited list),
#'  \item Synonyms ('|'-delimited list)
#'  \item DrugBankIDs ('|'-delimited list)
#'  }
#'
#' @param verbose (default \code{FALSE}) If set to \code{TRUE} messages and
#' warnings are raised.
#' @return A \code{data.frame} with the content of the file "CTD_genes.tsv.gz"
#' @examples
#' download_ctd_chem()
#' fdl <- load_ctd_chem()
#' dim( fdl )
#' @export load_ctd_chem
load_ctd_chem <- function( verbose = FALSE ) {
    bfc <- .get_cache( verbose, ask = FALSE )
    if( nrow( BiocFileCache::bfcquery( bfc, "CTD_chemicals" ) ) != 1 ) {
        warning( "CHEMICAL vocabulary could not be loaded." )
        return(data.frame())
    } else {
        filename <- BiocFileCache::bfcrpath( bfc, "CTD_chemicals" )
    }

    tbl <- tryCatch( {
        read.delim( filename, header = FALSE, comment.char = "#", sep = "\t",
                    stringsAsFactors = FALSE )
    }, error = function( e ) 1 )

    if( class( tbl ) == "data.frame" ) {
        colnames( tbl ) <-
          c( "ChemicalName", "ChemicalID", "CasRN", "Definition", "ParentIDs",
             "TreeNumbers", "ParentTreeNumbers", "Synonyms")#, "DrugBankIDs" )
        tbl <- tbl[ !is.na( tbl$ChemicalName ), ]
        tbl$ChemicalName <- toupper( tbl$ChemicalName )
        tbl$Synonyms <- toupper( tbl$Synonyms )
        tbl
    } else {
        data.frame()
    }
}


#' Function to load the \code{.tsv.gz} file for disease
#'
#' @details The field included in the file (\code{CTD_diseases.tsv.gz}) are:
#' \enumerate{
#'  \item DiseaseName
#'  \item DiseaseID (MeSH or OMIM identifier)
#'  \item Definition
#'  \item AltDiseaseIDs (alternative identifiers; '|'-delimited list)
#'  \item ParentIDs (identifiers of the parent terms; '|'-delimited list)
#'  \item TreeNumbers (identifiers of the disease's nodes; '|'-delimited list)
#'  \item ParentTreeNumbers (identifiers of the parent nodes; '|'-delimited list)
#'  \item Synonyms ('|'-delimited list)
#'  \item SlimMappings (MEDIC-Slim mappings; '|'-delimited list)
#'  }
#'
#' @param verbose (default \code{FALSE}) If set to \code{TRUE} messages and
#' warnings are raised.
#' @return A \code{data.frame} with the content of the file "CTD_genes.tsv.gz"
#' @examples
#' download_ctd_dise()
#' fdl <- load_ctd_dise()
#' dim( fdl )
#' @export load_ctd_dise
load_ctd_dise <- function( verbose = FALSE ) {
    bfc <- .get_cache( verbose, ask = FALSE )
    if( nrow( BiocFileCache::bfcquery( bfc, "CTD_diseases" ) ) != 1 ) {
        warning( "DISEASE vocabulary could not be loaded." )
        return(data.frame())
    } else {
        filename <- BiocFileCache::bfcrpath( bfc, "CTD_diseases" )
    }

    tbl <- tryCatch( {
        read.delim( filename, header = FALSE, comment.char = "#", sep = "\t",
                    stringsAsFactors = FALSE )
    }, error = function( e ) 1 )

  if( class( tbl ) == "data.frame" ) {
    colnames( tbl ) <-
      c( "DiseaseName", "DiseaseID", "AltDiseaseIDs", "Definition", "ParentIDs", "TreeNumbers", "ParentTreeNumbers", "Synonyms", "SlimMappings" )
    tbl <- tbl[ !is.na( tbl$DiseaseName ), ]
    tbl$DiseaseName <- toupper( tbl$DiseaseName )
    tbl
  } else {
    data.frame()
  }
}
