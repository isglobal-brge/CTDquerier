#' Function to query CTDbase using gene terminology ( Gene Symbol )
#'
#' This function checks for CTDbase gene vocabulary and query CTDbase
#' for each one, downloading gene-gene interactions, chemicals
#' interactions, associated disease, associated KEGG pathways and
#' associated GO terms.
#'
#' @param terms Character vector with the genes used in the query.
#' @param ask (default \code{TRUE}) If \code{TRUE} it asks the the persistent
#' location must be used to save the vocabulary if it was not downloaded
#' previously.
#' @param verbose (default \code{FALSE}) If set to \code{TRUE} is shows
#' relevant information of each step.
#' @return An object of class \code{\link{CTDdata}}.
#' @examples
#' rst <- query_ctd_gene( terms = c( "APP", "HMOX1A", "hmox1" ), verbose = TRUE )
#' @export query_ctd_gene
query_ctd_gene <- function( terms, ask = TRUE, verbose = FALSE ) {
    ## SETUP
    download_ctd_genes( verbose )
    if( verbose ) message( "Loading gene vocabulary." )
    tbl <- load_ctd_gene( verbose )
    ## //

    ## VALIDATE INPUT GENES
    terms <- toupper( terms )
    keep <- S4Vectors::DataFrame( GeneSymbol = "", GeneID = "" )
    disc <- character()
    for( ii in 1:length( terms ) ) {
        if( terms[ ii ] %in% tbl$GeneSymbol ) {
            keep <- rbind( keep, tbl[ tbl$GeneSymbol == terms[ ii ] , c( "GeneSymbol", "GeneID" ) ] )
        } else {
            disc <- c(disc, terms[ ii ])
        }
    }
    keep <- keep[ -1, ]

    if( length( disc ) != 0 ) {
        warning( length( disc ) , "/", length( terms ), " terms were dropped." )
    }

    if( nrow( keep ) == 0 ) {
        stop( "All input terms were dropped." )
    }
    ## //

    gene_interactions <- prepare_ctd_gene( "gene_interactions" )
    gene_gene_interactions <- prepare_ctd_gene( "gene_gene_interactions" )
    gene_diseases <- prepare_ctd_gene( "gene_diseases" )
    gene_go <- prepare_ctd_gene( "gene_go" )
    gene_kegg <- prepare_ctd_gene( "gene_kegg" )
    gene_chemical_interaction <- prepare_ctd_gene( "gene_chemical_interaction" )
    for( ii in 1:nrow( keep ) ) {
        if( verbose ) {
            message( "Staring query for gene '", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
        }

        ## PREPARE URLs
        # url_idx <- get_ctd_url(
        #   index    = "chemical_gene_interaction",
        #   term     = keep[ ii, 2 ],
        #   category = "gene"
        # )
        url_gg <- get_ctd_url(
            index    = "gene_gene_interaction",
            term     = keep[ ii, 2 ],
            category = "gene"
        )
        url_disease <- get_ctd_url(
            index    = "gene_disease",
            term     = keep[ ii, 2 ],
            category = "gene"
        )
        url_kegg <- get_ctd_url(
            index    = "gene_kegg",
            term     = keep[ ii, 2 ],
            category = "gene"
        )
        url_go <- get_ctd_url(
            index    = "gene_go",
            term     = keep[ ii, 2 ],
            category = "gene"
        )
        url_chemical <- get_ctd_url(
            index    = "gene_chemical_interaction",
            term     = keep[ ii, 2 ],
            category = "gene"
        )
        ## //

        # DOWNLOAD CONTENT
        # if( verbose ) {
        #   message( " . Downloading IDX" )
        # }
        # tmp <- download_url( url = url_idx )
        # tmp$GeneSymbol <- keep[ ii, 1 ]
        # tmp$GeneID <- keep[ ii, 2 ]
        # gene_interactions <- rbind( gene_interactions, tmp )

        if( verbose ) {
            message( " . Downloading 'gene-gene interaction' table." )
        }
        tmp <- download_url( url = url_gg, verbose = verbose )
        if( nrow( tmp ) > 0 ) {
            tmp$GeneSymbol <- keep[ ii, 1 ]
            tmp$GeneID <- keep[ ii, 2 ]
            gene_gene_interactions <- rbind( gene_gene_interactions, tmp )
        } else if( verbose ) {
            message(" . . No 'gene-gene interaction' table available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
        }

        if( verbose ) {
            message( " . Downloading 'disease' table." )
        }
        tmp <- download_url( url = url_disease, verbose = verbose )
        if( nrow( tmp ) > 0 ) {
            tmp$GeneSymbol <- keep[ ii, 1 ]
            tmp$GeneID <- keep[ ii, 2 ]
            gene_diseases <- rbind( gene_diseases, tmp )
        } else if( verbose ) {
            message(" . . No 'disease' table available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
        }

        if( verbose ) {
            message( " . Downloading 'gene-chemical interaction' table." )
        }
        tmp <- download_url( url = url_chemical, verbose = verbose )
        if( nrow( tmp ) > 0 ) {
            tmp$GeneSymbol <- keep[ ii, 1 ]
            tmp$GeneID <- keep[ ii, 2 ]
            gene_chemical_interaction <- rbind( gene_chemical_interaction, tmp )
        } else if( verbose ) {
            message(" . . No 'gene-chemical interaction' table available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
        }

        if( verbose ) {
            message( " . Downloading 'GO terms' table." )
        }
        tmp <- download_url( url = url_go, verbose = verbose )
        if( nrow( tmp ) > 0 ) {
            tmp$GeneSymbol <- keep[ ii, 1 ]
            tmp$GeneID <- keep[ ii, 2 ]
            gene_go <- rbind( gene_go, tmp )
        } else if( verbose ) {
            message(" . . No 'GO terms' table available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
        }

        if( verbose ) {
            message( " . Downloading 'KEGG pathways' table." )
        }
        tmp <- download_url( url = url_kegg, verbose = verbose )
        if( nrow( tmp ) > 0 ) {
            tmp$GeneSymbol <- keep[ ii, 1 ]
            tmp$GeneID <- keep[ ii, 2 ]
            gene_kegg <- rbind( gene_kegg, tmp )
        } else if( verbose ) {
            message(" . . No 'KEGG pathways' table available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
        }
        # //
    }
    rm( tmp, ii )

    new( "CTDdata",
         type                   = "GENE",
         timestamp              = as.character( Sys.time() ),
         terms                  = S4Vectors::DataFrame( keep ),
         losts                  = disc[ disc != "NA" ],
         gene_interactions      = S4Vectors::DataFrame(),
         chemicals_interactions = gene_chemical_interaction,
         diseases               = gene_diseases,
         gene_gene_interactions = gene_gene_interactions,
         kegg                   = gene_kegg,
         go                     = gene_go
    )
}


prepare_ctd_gene <- function( set ) {
    if( set == "gene_interactions" ) {
        gene_interactions <- rbind( 1:8 )
        colnames( gene_interactions ) <- c( "Chemical.Name", "Chemical.ID", "CAS.RN", "Interaction", "Interaction.Actions", "Reference.Count", "Organism.Count", "origin" )
        return( S4Vectors::DataFrame(gene_interactions[ -1, ] ) )
    }

    if( set == "gene_gene_interactions" ) {
        gene_gene_interactions <- rbind( 1:13 )
        colnames( gene_gene_interactions ) <- c( "Source.Gene.Symbol", "Source.Gene.ID", "Target.Gene.Symbol", "Target.Gene.ID", "Source.Organism", "Target.Organism", "Assay", "Interaction.Type", "Throughput", "Reference.Authors", "Reference.Citation", "PubMed.ID", "origin" )
        return( S4Vectors::DataFrame( gene_gene_interactions[ -1, ] ) )
    }

    if( set == "gene_diseases" ) {
        gene_diseases <- rbind( 1:6 )
        colnames( gene_diseases ) <- c( "Disease Name", "Disease ID", "Direct Evidence", "Inference Network", "Inference Score", "Reference Count" )
        return( S4Vectors::DataFrame( gene_diseases[ -1, ] ) )
    }

    if( set == "gene_kegg" )  {
        gene_kegg <- rbind( 1:2 )
        colnames( gene_kegg ) <- c( "Pathway", "Pathway ID" )
        return( S4Vectors::DataFrame( gene_kegg[ -1, ] ) )
    }

    if( set == "gene_go" ) {
        gene_go <- rbind( 1:5 )
        colnames( gene_go ) <- c( "Ontology", "Qualifiers", "GO Term Name", "GO Term ID", "Organisms (Evidence)" )
        return( S4Vectors::DataFrame( gene_go[ -1, ] ) )
    }

    if( set == "gene_chemical_interaction" ) {
        gene_chemical_interaction <- rbind( 1:7 )
        colnames( gene_chemical_interaction ) <- c( "Chemical Name", "Chemical ID", "CAS RN", "Interaction", "Interaction Actions", "Reference Count", "Organism Count" )
        return( S4Vectors::DataFrame( gene_chemical_interaction[ -1, ] ) )
    }

    stop( "[INTERNAL] Invalid selected set." )
}
