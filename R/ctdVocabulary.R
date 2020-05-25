#' Vocabulary from CTDbase
#'
#' Encapsulation of a _CTDbase_ vocabulary file.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data, vocabulary, ctdbase
# @format An \code{\link{R6Class}} generator object
# @section Methods:
# \describe{
#   \item{\code{example_method(parameter_1 = 3)}}{This method uses \code{parameter_1} to ...}
# }
ctdVocabulary <- R6::R6Class("ctdVocabulary",
    private = list(
        # @field file_path Path to the loaded file.
        file_path = NA,
        # @field data_source Type to the loaded file (gene, disease, chemical, or pathway).
        data_source = NA,
        # @field data_tibble Content of the file as tibble.
        data_content = NULL
    ),
    public = list(
        #' @description
        #' Create a new `ctdVocabulary` object.
        #' @param file_path Path, including the file's name, to the file to be loaded.
        #' @param data_source Source of data of the file to be loaded, one of: _gene_, _disease_, _chemical_, or _pathway_.
        #' @param data_format Format of the file to be loaded (`"csv"` for `"csv"` or `"csv.gz"` file, or `"tsv"` for `"tsv"` or `"tsv.gz"` file).
        #' @return A new `ctdVocabulary` object.
        initialize = function(file_path, data_source, data_format = "csv") {
            data_source <- tolower(data_source)
            data_format <- tolower(data_format)
            stopifnot(is.character(file_path), length(file_path) == 1)
            stopifnot(is.character(data_source), length(data_source) == 1, data_source %in% c("gene", "disease", "chemical", "pathway"))
            stopifnot(is.character(data_format), length(data_format) == 1, data_format %in% c("csv", "tsv"))

            private$file_path <- file_path
            private$data_source = data_source

            if(data_source == "gene") {
                col_names <- c("GeneSymbol", "GeneName", "GeneID", "AltGeneIDs", "Synonyms", "BioGRIDIDs", "PharmGKBIDs", "UniProtIDs")
            } else if(data_source == "disease") {
                col_names <- c("DiseaseName", "DiseaseID", "AltDiseaseIDs", "Definition", "ParentIDs", "TreeNumbers", "ParentTreeNumbers", "Synonyms", "SlimMappings")
            } else if(data_source == "chemical") {
                col_names <- c("ChemicalName", "ChemicalID", "CasRN", "Definition", "ParentIDs", "TreeNumbers", "ParentTreeNumbers", "Synonyms", "DrugBankIDs")
            } else if(data_source == "pathway") {
                col_names <- c("PathwayName", "PathwayID")
            }

            if(data_format == "csv") {
                private$data_content <- x <- data.table::fread(file_path, sep =",", showProgress = FALSE, data.table = TRUE, header = FALSE, col.names = col_names)
            } else {
                private$data_content <- x <- data.table::fread(file_path, sep ="\t", showProgress = FALSE, data.table = TRUE, header = FALSE, col.names = col_names)
            }
        },
        #' @description
        #' Returns a `character` indicating the _source_ of the `ctdVocabulary`.
        #' @return A `character` object.
        source = function() {
            return(private$data_source)
        },
        #' @description
        #' Returns the content of the `ctdVocabulary`.
        #' @return A `data.table` object.
        table = function() {
            return(private$data_content)
        },
        #' @description
        #' Shows a brief description of the `ctdVocabulary`.
        #' @param ... Do not used.
        #' @return A `invisible` object.
        print = function(...) {
            cat("ctdVocabulary (source: ", private$data_source ,"\n", sep = "")
            cat("  . file: ", private$file_path, "\n", sep = "")
            cat("  . columns: ", ncol(self$table()), "\n", sep = "")
            cat("  . rows: ", nrow(self$table()), "\n", sep = "")
            return(invisible(self))
        },
        #' @description
        #' Searches for a term in a given `ctdVocabulary`. The following fields can be used for the search:
        #'   * Genes: _gene symbol_, _gene name_, and _gene id_.
        #'   * Diseases: _disease name_ and _disease id_.
        #'   * Chemicals: _chemical name_, _chemical id_, and _CAS registry number_.
        #'   * Pathway: _pathway name_ and _pathway id_.
        #' @param terms Terms to search in the given `ctdVocabulary` object.
        #' @return A `data.table` object. First column is filled with user term, second with internal "name", and the third with the similarity score, computed using _longest common substring distance_.
        search_term = function(terms) {
            terms <- tolower(terms)
            stopifnot(is.character(terms), length(terms) >= 1)

            if(private$data_source == "gene") {
                x <- do.call(rbind, lapply(terms, function(term) {
                    do.call(rbind, list(
                        data.frame(
                            user_term = term,
                            table_term = tolower(self$table()[ , "GeneSymbol", drop = TRUE]),
                            distance = stringdist::stringdist(term, tolower(self$table()[ , "GeneSymbol", drop = TRUE]), method = "lcs")
                        ),
                        data.frame(
                            user_term = term,
                            table_term = tolower(self$table()[ , "GeneName", drop = TRUE]),
                            distance = stringdist::stringdist(term, tolower(self$table()[ , "GeneName", drop = TRUE]), method = "lcs")
                        ),
                        data.frame(
                            user_term = term,
                            table_term = tolower(self$table()[ , "GeneID", drop = TRUE]),
                            distance = stringdist::stringdist(term, tolower(self$table()[ , "GeneID", drop = TRUE]), method = "lcs")
                        )
                    ))
                }))
            } else if(private$data_source == "disease") {
                x <- do.call(rbind, lapply(terms, function(term) {
                    do.call(rbind, list(
                        data.frame(
                            user_term = term,
                            table_term = tolower(self$table()[ , "DiseaseName", drop = TRUE]),
                            distance = stringdist::stringdist(term, tolower(self$table()[ , "DiseaseName", drop = TRUE]), method = "lcs")
                        ),
                        data.frame(
                            user_term = term,
                            table_term = tolower(self$table()[ , "DiseaseID", drop = TRUE]),
                            distance = stringdist::stringdist(term, tolower(self$table()[ , "DiseaseID", drop = TRUE]), method = "lcs")
                        )
                    ))
                }))
            } else if(private$data_source == "chemical") {
                x <- do.call(rbind, lapply(terms, function(term) {
                    do.call(rbind, list(
                        data.frame(
                            user_term = term,
                            table_term = tolower(self$table()[ , "ChemicalName", drop = TRUE]),
                            distance = stringdist::stringdist(term, tolower(self$table()[ , "ChemicalName", drop = TRUE]), method = "lcs")
                        ),
                        data.frame(
                            user_term = term,
                            table_term = tolower(self$table()[ , "ChemicalID", drop = TRUE]),
                            distance = stringdist::stringdist(term, tolower(self$table()[ , "ChemicalID", drop = TRUE]), method = "lcs")
                        ),
                        data.frame(
                            user_term = term,
                            table_term = tolower(self$table()[ , "CasRN", drop = TRUE]),
                            distance = stringdist::stringdist(term, tolower(self$table()[ , "CasRN", drop = TRUE]), method = "lcs")
                        )
                    ))
                }))
            } else { #if(private$data_source == "pathway") {}
                x <- do.call(rbind, lapply(terms, function(term) {
                    do.call(rbind, list(
                        data.frame(
                            user_term = term,
                            table_term = tolower(self$table()[ , "PathwayName", drop = TRUE]),
                            distance = stringdist::stringdist(term, tolower(self$table()[ , "PathwayName", drop = TRUE]), method = "lcs")
                        ),
                        data.frame(
                            user_term = term,
                            table_term = tolower(self$table()[ , "PathwayID", drop = TRUE]),
                            distance = stringdist::stringdist(term, tolower(self$table()[ , "PathwayID", drop = TRUE]), method = "lcs")
                        )
                    ))
                }))
            }

            x <- x[!is.na(x$distance), ]
            x <- x[order(x$distance), ]
            return(data.table::data.table(x))
        },
        #' @description
        #' Validates a series of terms into the `ctdVocabulary`.The following fields are used for validating the terms:
        #'   * Genes: _gene symbol_, _gene name_, and _gene id_.
        #'   * Diseases: _disease name_ and _disease id_.
        #'   * Chemicals: _chemical name_, _chemical id_, and _CAS registry number_.
        #'   * Pathway: _pathway name_ and _pathway id_.
        #' @param terms Terms to validate in the given `ctdVocabulary` object.
        #' @return A `ctdTerm` object.
        validate_term = function(terms) {
            terms <- unique(tolower(terms))
            stopifnot(is.character(terms), length(terms) >= 1)

            dis <- character()

            if(private$data_source == "gene") {
                x <- do.call(rbind, lapply(terms, function(term) {
                    x <- dplyr::filter(self$table(), tolower(GeneSymbol) == term | tolower(GeneName) == term | tolower(GeneID) == term)
                    if(nrow(x) == 0) {
                        dis <- c(dis, term)
                        return(data.frame())
                    } else {
                        x$user_term = term
                        return(cbind(x))
                    }
                }))
            } else if(private$data_source == "disease") {
                x <- do.call(rbind, lapply(terms, function(term) {
                    x <- dplyr::filter(self$table(), DiseaseName == term | DiseaseID == term)
                    if(nrow(x) == 0) {
                        dis <- c(dis, term)
                        return(data.frame())
                    } else {
                        x$user_term = term
                        return(x)
                    }
                }))
            } else if(private$data_source == "chemical") {
                x <- do.call(rbind, lapply(terms, function(term) {
                    x <- dplyr::filter(self$table(), PathwayName == term | ChemicalID == term | CasRN == term)
                    if(nrow(x) == 0) {
                        dis <- c(dis, term)
                        return(data.frame())
                    } else {
                        x$user_term = term
                        return(x)
                    }
                }))
            } else { #if(private$data_source == "pathway") {}
                x <- do.call(rbind, lapply(terms, function(term) {
                    x <- dplyr::filter(self$table(), ChemicalName == term | PathwayID == term)
                    if(nrow(x) == 0) {
                        dis <- c(dis, term)
                        return(data.frame())
                    } else {
                        x$user_term = term
                        return(x)
                    }
                }))
            }

            if(length(dis) != 0) {
                warning("Not all terms were validated. Use 'get_terms' from returng 'ctdTerm' object for more details.")
            }

            ctdTerm$new(private$data_source, terms, x)
        }
    )
)


#' Term from CTDbase's Vocabulary
#'
#' Result of validating one or more terms in a `ctdVocabulary` object.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data, vocabulary, term, ctdbase
ctdTerm <- R6::R6Class("ctdTerm",
    private = list(
        data_source = NA,
        user_terms = NA,
        validated_terms = NA
    ),
    public = list(
        #' @description
        #' Create a new `ctdTerm` object.
        #' @param data_source Source of data of the file to be loaded, one of: _gene_, _disease_, _chemical_, or _pathway_.
        #' @param user_terms `character` with all the terms given by user to be validated.
        #' @param validated_terms `data.table` with all the information related to terms in _CTDbase_.
        #' @return A new `ctdTerm` object.
        initialize = function(data_source, user_terms, validated_terms) {
            private$data_source <- data_source
            private$user_terms <- user_terms
            private$validated_terms <- validated_terms
        },
        #' @description
        #' Returns the terms used to create the `ctdTerm` indicating which of them where validated.
        #' @return A `list` with two entries classifying the terms (`valid`, `no_valid`) according if they are in _CTDbase_.
        get_terms = function() {
            valid = unique(private$validated_terms[ , "user_term"])
            return(list(
                valid = valid,
                no_valid = private$user_terms[!private$user_terms %in% valid]
            ))

        },
        #' @description
        #' Shows a brief description of the `ctdTerm`.
        #' @param ... Do not used.
        #' @return A `invisible` object.
        print = function(...) {
            cat("ctdTerm (source: ", private$data_source, ")\n", sep = "")
            cat("  . queried terms: (", length(private$user_terms), ") ", private$user_terms[1], ", ..., ", private$user_terms[length(private$user_terms)], "\n", sep = "")
            x <- unique(private$validated_terms[ , "user_term", drop = TRUE ])
            cat("  . queried terms: (", length(x), ") ", x[1], ", ..., ", x[length(x)], "\n", sep = "")
            return(invisible(self))
        }
    )
)





