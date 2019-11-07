#' @describeIn CTDdata Return a list with the terms found to create the object.
setMethod(
    f = "get_terms",
    signature = "CTDdata",
    definition = function(object) {
        if( nrow( object@terms ) > 0 ) {
            return(list(
                "found" = object@terms[ , 1 ],
                "lost" = object@losts
            ))
        } else {
            return(list(
                "found" = NA,
                "lost" = object@losts
            ))
        }
    }
)
