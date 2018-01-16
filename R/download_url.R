download_url <- function( url, verbose = TRUE ) {
  url.content <- RCurl::getURL( url = url )
  if( length( grep( "Bad Gateway", url.content ) ) != 0 & verbose ) {
      warning( "Comparative Toxicogenomics Database returned a 'Bad Gateway' error. This table could not be downloaded." )
  }

  url.table = tryCatch({
    utils::read.delim( text = url.content, head = TRUE, sep = "\t", as.is = TRUE )
  }, error = function(e) {
    url.table <- data.frame()
  })

  if( ncol( url.table ) <= 1 ) {
    url.table <- data.frame()
  }

  return(url.table)

  if( url.content == "" ) {
    return( list( status=TRUE,  table=url.table ) )
  } else {
    return( list( status=FALSE, table=url.table ) )
  }
}
