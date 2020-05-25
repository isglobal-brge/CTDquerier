#' Database storage from CTDbase
#'
#' Encapsulation of one or multiple database files from _CTDbase_.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data, database, association, interaction, ctdbase
ctdStorage <- R6Class("ctdStorage",
    private = list(
        file_list = NA
    ),
    public = list(
        #' @description
        #' Create a new `ctdStorage` object.
        #' @param file_list List of character vectors. Each vector containing two elements: name and path.
        #' @return A new `ctdStorage` object.
        initialize = function(file_list) {
            stopifnot(is.list(file_list), length(file_list) >= 1, sum(sapply(file_list, length) == 2) == length(file_list))
            private$file_list <- file_list
        },
        #' @description
        #' Shows a brief description of the `ctdStorage`.
        #' @param ... Do not used.
        #' @return A `invisible` object.
        print = function(...) {
            cat("ctdStorage (", length(provate$file_list), " source files)\n", sep = "")
            return(invisible(self))
        },
        #' @description
        #' Indicates if a given key is present in the `ctdStorage`.
        #' @param key `character` with the key to be checked.
        #' @return A `boolean` vector.
        includes = function(key) {
            stopifnot(is.character(key), length(key) >= 1)
            key <- tolower(key)
            main_keys <- tolower(sapply(private$file_list, "[[", 1))
            return(sapply(key, function(k) k %in% main_keys))
        }
    )
)
