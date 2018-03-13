.get_cache <- function( verbose = FALSE ) {
    #path <- file.path(tempdir(), "tempCTDquerierCacheDir")
    #bfc <- BiocFileCache::BiocFileCache( path )
    if( verbose ) {
        cache <- rappdirs::user_cache_dir( appname = "CTDQuery",
            appauthor = "isglobal" )
        bfc <- BiocFileCache::BiocFileCache( cache )
    } else {
        suppressMessages( suppressWarnings({
            cache <- rappdirs::user_cache_dir( appname = "CTDQuery",
                                               appauthor = "isglobal" )
            bfc <- BiocFileCache::BiocFileCache( cache )
        } ) )
    }
    return( bfc )
}

#' Function to download genes available in CTDbase
#'
#' This function download the "Gene vocabulary" file (\code{CTD_genes.tsv.gz})
#' from \code{http://ctdbase.org/downloads}.
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
#' @param verbose (default \code{FALSE}) If set to \code{TRUE} is shows relevant
#' information of each step.
#' @return Passed name into \code{filename} argument if it could be download
#' \code{1} otherwise.
#' @examples
#' download_ctd_genes()
#' @export download_ctd_genes
download_ctd_genes <- function( verbose = FALSE ) {
    fileURL <- "http://ctdbase.org/reports/CTD_genes.tsv.gz"
    bfc <- .get_cache( verbose )

    if( nrow( BiocFileCache::bfcquery(bfc, "CTD_genes") ) == 0 ) {
        if( verbose ) message( "Downloading GENE vocabulary from CTDbase" )
        BiocFileCache::bfcadd(bfc, "CTD_genes", fileURL )
    }
}

#' Function to download checmicals available in CTDbase
#'
#' This function download the "Chemical vocabulary" file (\code{CTD_chemicals.tsv.gz})
#' from \code{http://ctdbase.org/downloads}.
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
#' @param filename (default \code{"CTD_chemicals.tsv.gz"}) Name of the file in
#' the local system.
#' @param mode (default \code{"auto"}) Mode passed to \code{download.file}.
#' @param verbose (default \code{FALSE}) If set to \code{TRUE} is shows relevant
#' information of each step.
#' @return Passed name into \code{filename} argument if it could be download
#' \code{1} otherwise.
#' @examples
#' download_ctd_chem()
#' file.exists( "CTD_chemicals.tsv.gz" )
#' @export download_ctd_chem
download_ctd_chem <- function( verbose = FALSE ) {
    fileURL <- "http://ctdbase.org/reports/CTD_chemicals.tsv.gz"
    bfc <- .get_cache( verbose )

    if( nrow( BiocFileCache::bfcquery(bfc, "CTD_chemicals") ) == 0 ) {
        if( verbose ) message( "Downloading CHEMICAL vocabulary from CTDbase" )
        BiocFileCache::bfcadd(bfc, "CTD_chemicals", fileURL )
    }
}

#' Function to download diseases available in CTDbase
#'
#' This function download the "Disease vocabulary" file (\code{CTD_diseases.tsv.gz})
#' from \code{http://ctdbase.org/downloads}.
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
#' @param filename (default \code{"CTD_diseases.tsv.gz"}) Name of the file in
#' the local system.
#' @param mode (default \code{"auto"}) Mode passed to \code{download.file}.
#' @param verbose (default \code{FALSE}) If set to \code{TRUE} is shows relevant
#' information of each step.
#' @return Passed name into \code{filename} argument if it could be download
#' \code{1} otherwise.
#' download_ctd_dise()
#' file.exists( "CTD_diseases.tsv.gz" )
#' @export download_ctd_dise
download_ctd_dise <- function( verbose = FALSE ) {
    fileURL <- "http://ctdbase.org/reports/CTD_diseases.tsv.gz"
    bfc <- .get_cache( verbose )

    if( nrow( BiocFileCache::bfcquery(bfc, "CTD_diseases") ) == 0 ) {
        if( verbose ) message( "Downloading DISEASE vocabulary from CTDbase" )
        BiocFileCache::bfcadd(bfc, "CTD_diseases", fileURL )
    }
}
