setClass( "CTDquery",
  representation =
    representation(
      # esentials
      type     = "character",
      objects  = "DataFrame",
      # CTDbase results
      # gene_interactions      = "DataFrame",
      chemicals_interactions = "DataFrame",
      diseases               = "DataFrame",
      gene_gene_interactions = "DataFrame",
      kegg                   = "DataFrame",
      go                     = "DataFrame"
    )
)
