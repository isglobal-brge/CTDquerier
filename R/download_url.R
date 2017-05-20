download_url <- function( url ) {
  url.content <- RCurl::getURL( url = url )

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
