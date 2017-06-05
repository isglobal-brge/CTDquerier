#' @describeIn CTDquery Return a list with the terms found to create the object.
setMethod(
    f = "get_terms",
    signature = "CTDquery",
    definition = function(object) {
        return(list(
            "found" = object@terms$GeneSymbol,
            "lost" = object@losts
        ))
    }
)
