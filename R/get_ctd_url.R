#' Given an index, a term and a category it returns the URL from CTD to
#' download the corresponding information.
get_ctd_url <- function( index, term = "", category = "" ) {

  url <- list()
  url$basic                     <- "http://ctdbase.org/basicQuery.go?bq=TERM&bqCat=CATEGORY&6578706f7274=1&d-4029212-e=5"
  url$gene_basic                <- "http://ctdbase.org/basicQuery.go?bq=TERM&bqCat=CATEGORY&6578706f7274=1&d-4029212-e=5"
  url$chemical_gene_interaction <- "http://ctdbase.org/detail.go?6578706f7274=1&d-1339283-e=5&view=ixn&type=CATEGORY&acc=TERM"

  url$chemical_gene             <- "http://ctdbase.org/detail.go?d-3572529-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=gene"
  url$chemical_disease          <- "http://ctdbase.org/detail.go?6578706f7274=1&d-1332398-e=5&type=CATEGORY&acc=TERM&view=disease"
  url$chemical_pathway          <- "http://ctdbase.org/detail.go?6578706f7274=1&d-7820438-e=5&type=CATEGORY&acc=TERM&view=pathway"

  url$gene_chemical             <- "http://ctdbase.org/detail.go?d-3572529-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=chem"
  url$gene_chemical_interaction <- "http://ctdbase.org/detail.go?d-1339283-e=5%&6578706f7274=1&type=CATEGORY&&acc=TERM&view=ixn"
  url$gene_disease              <- "http://ctdbase.org/detail.go?6578706f7274=1&d-1332398-e=5&type=CATEGORY&acc=TERM&view=disease"
  url$gene_gene_interaction     <- "http://ctdbase.org/detail.go?d-5152978-e=5&d-3572529-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=gg"
  url$gene_go                   <- "http://ctdbase.org/detail.go?d-7449458-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=go"
  url$gene_kegg                 <- "http://ctdbase.org/detail.go?d-148988-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=pathway"

  sel <- url[ index ][[ 1 ]]

  sel <- stringr::str_replace(
    string      = sel,
    pattern     = "CATEGORY",
    replacement = category
  )

  sel <- stringr::str_replace(
    string      = sel,
    pattern     = "TERM",
    replacement = term
  )

  sel <- stringr::str_replace(
    string      = sel,
    pattern     = " ",
    replacement = "+"
  )

  return( sel )
}
