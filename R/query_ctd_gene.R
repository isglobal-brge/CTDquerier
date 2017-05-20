#' Function to query CTDbase using gene terminology ( Gene Symbol )
#'
#' This function cheks for CTDbase gene vocabulary and query CTDbase
#' for each one, downloading (...)
#'
#' @param terms Character vector with the genes used in the query.
#' @param filename (default \code{"CTD_genes.tsv.gz"}) Name of the file
#' to store the CTDbase gene vocabilary.
#' @param mode (default \code{"auto"}) Mode passed to \code{download.file}.
#' @param verbose (default \code{FALSE}) If set to \code{TRUE} is shows relevant
#' information of each step.
#' @return
#' @example s
#' rst <- query_ctd_gene( terms = c("APP", "HMOX1A", "hmox1" ), verbose = TRUE )
query_ctd_gene <- function( terms, filename = "CTD_genes.tsv.gz", mode = "auto", verbose = FALSE ) {
  ## SETUP
  rst <- download_ctd_genes( filename, mode, verbose )
  if( rst == "" ) {
    stop( "CTDquerier was not ablte to download '", filename, "' from CTDbase." )
  }

  if( verbose ) {
    message( "Loading gene vocabulary." )
  }
  tbl <- load_ctd_gene( filename )
  ## //

  ## VALIDATE INPUT GENES
  terms <- toupper( terms )
  keep <- S4Vectors::DataFrame( GeneSymbol = "", GeneID = "" )
  disc <- c()
  for( ii in 1:length( terms ) ) {
    if( terms[ ii ] %in% tbl$GeneSymbol ) {
      keep <- rbind( keep, tbl[ tbl$GeneSymbol == terms[ ii ] , c( "GeneSymbol", "GeneID" ) ] )
    } else {
      disc <- c(disc, terms[ ii ])
    }
  }
  keep <- keep[ -1, ]

  if( length( disc ) != 0 ) {
    warning( length( disc ) , "/", length( terms ), " terms were discarted." )
  }
  rm( disc )
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
      message( " . Downloading GGI" )
    }
    tmp <- download_url( url = url_gg )
    if( nrow( tmp ) > 0 ) {
      tmp$GeneSymbol <- keep[ ii, 1 ]
      tmp$GeneID <- keep[ ii, 2 ]
      gene_gene_interactions <- rbind( gene_gene_interactions, tmp )
    } else if( verbose ) {
      message(" . . No GGI available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
    }

    if( verbose ) {
      message( " . Downloading DSS" )
    }
    tmp <- download_url( url = url_disease )
    if( nrow( tmp ) > 0 ) {
      tmp$GeneSymbol <- keep[ ii, 1 ]
      tmp$GeneID <- keep[ ii, 2 ]
      gene_diseases <- rbind( gene_diseases, tmp )
    } else if( verbose ) {
      message(" . . No DSS available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
    }

    if( verbose ) {
      message( " . Downloading GCI" )
    }
    tmp <- download_url( url = url_chemical )
    if( nrow( tmp ) > 0 ) {
      tmp$GeneSymbol <- keep[ ii, 1 ]
      tmp$GeneID <- keep[ ii, 2 ]
      gene_chemical_interaction <- rbind( gene_chemical_interaction, tmp )
    } else if( verbose ) {
      message(" . . No GCI available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
    }

    if( verbose ) {
      message( " . Downloading GO" )
    }
    tmp <- download_url( url = url_go )
    if( nrow( tmp ) > 0 ) {
      tmp$GeneSymbol <- keep[ ii, 1 ]
      tmp$GeneID <- keep[ ii, 2 ]
      gene_go <- rbind( gene_go, tmp )
    } else if( verbose ) {
      message(" . . No GO available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
    }

    if( verbose ) {
      message( " . Downloading KEGG" )
    }
    tmp <- download_url( url = url_kegg )
    if( nrow( tmp ) > 0 ) {
      tmp$GeneSymbol <- keep[ ii, 1 ]
      tmp$GeneID <- keep[ ii, 2 ]
      gene_kegg <- rbind( gene_kegg, tmp )
    } else if( verbose ) {
      message(" . . No KEGG available for ", keep[ ii, 1 ], "' ( ", keep[ ii, 2 ], " )" )
    }
    # //
  }
  rm(tmp, ii)

  new( "CTDquery",
    type     = "GENE",
    objects  = S4Vectors::DataFrame( keep ),
       # gene_interactions      = gene_interactions,
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
    gene_diseases <- rbind( 1:2 )
    colnames( gene_diseases ) <- c( "Pathway", "Pathway ID" )
    return( S4Vectors::DataFrame( gene_diseases[ -1, ] ) )
  }

  if( set == "gene_go" ) {
    gene_diseases <- rbind( 1:5 )
    colnames( gene_diseases ) <- c( "Ontology", "Qualifiers", "GO Term Name", "GO Term ID", "Organisms (Evidence)" )
    return( S4Vectors::DataFrame( gene_diseases[ -1, ] ) )
  }

  if( set == "gene_chemical_interaction" ) {
    gene_chemical_interaction <- rbind( 1:7 )
    colnames( gene_chemical_interaction ) <- c( "Chemical Name", "Chemical ID", "CAS RN", "Interaction", "Interaction Actions", "Reference Count", "Organism Count" )
    return( S4Vectors::DataFrame( gene_chemical_interaction[ -1, ] ) )
  }

  stop( "[INTERNAL] Invalid selected set." )
}
