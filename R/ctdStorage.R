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
        file_list = NA,
        lazy_load = TRUE,
        eager_unload = TRUE,
        # @description
        # private method to load a storage file
        load_storage = function(file_name) {
            stopifnot(length(file_name) == 1)
            file_name <- tolower(file_name)
            stopifnot(file_name %in% names(private$file_list))

            if(is.na(private$file_list[[file_name]]$file_content)) {
                col_names = list(
                    "chemical–gene interactions" = c("ChemicalName", "ChemicalID", "CasRN", "GeneSymbol", "GeneID", "GeneForms", "Organism", "OrganismID", "Interaction", "InteractionActions", "PubMedIDs"),
                    "chemical–gene interaction types" = c("TypeName", "Code", "Description", "ParentCode"),
                    "chemical–disease associations" = c("ChemicalName", "ChemicalID", "CasRN", "DiseaseName", "DiseaseID", "DirectEvidence", "InferenceGeneSymbol", "InferenceScore", "OmimIDs", "PubMedIDs"),
                    "chemical–go enriched associations" = c("ChemicalName", "ChemicalID", "CasRN", "Ontology", "GOTermName", "GOTermID", "HighestGOLevel", "PValue", "CorrectedPValue", "TargetMatchQty", "TargetTotalQty", "BackgroundMatchQty", "BackgroundTotalQty"),
                    "chemical–pathway enriched associations" = c("ChemicalName", "ChemicalID", "CasRN", "PathwayName", "PathwayID", "PValue", "CorrectedPValue", "TargetMatchQty", "TargetTotalQty", "BackgroundMatchQty", "BackgroundTotalQty"),
                    "gene–disease associations" = c("GeneSymbol", "GeneID", "DiseaseName", "DiseaseID", "DirectEvidence", "InferenceChemicalName", "InferenceScore", "OmimIDs", "PubMedIDs"),
                    "gene–pathway association" = c("GeneSymbol", "GeneID", "PathwayName", "PathwayID"),
                    "disease–pathway associations" = c("DiseaseName", "DiseaseID", "PathwayName", "PathwayID", "InferenceGeneSymbol"),
                    "chemical–phenotype interactions" = c("chemicalname", "chemicalid", "casrn", "phenotypename", "phenotypeid", "comentionedterms", "organism", "organismid", "interaction", "interactionactions", "anatomyterms", "inferencegenesymbols", "pubmedids", "last")
                )
                stopifnot(file_name %in% names(col_names))
                col_names = col_names[[file_name]]

                if(private$file_list[[file_name]]$file_format == "csv") {
                    private$file_list[[file_name]]$file_content <- data.table::fread(private$file_list[[file_name]]$file_path, sep = ",", showProgress = FALSE, data.table = TRUE, header = FALSE, col.names = col_names)
                } else {
                    private$file_list[[file_name]]$file_content <- data.table::fread(private$file_list[[file_name]]$file_path, sep = "\t", showProgress = FALSE, data.table = TRUE, header = FALSE, col.names = col_names)
                }
            }

            return(invisible(self))
        },
        # @description
        # private method to unload a storage file
        unload_storage = function(file_name) {
            stopifnot(file_name %in% names(private$file_list))
            x <- suppressMessages(suppressWarnings(gc()))
            return(invisible(self))
        }
    ),
    public = list(
        #' @description
        #' Create a new `ctdStorage` object.
        #'
        #' If `lazy_load` is set to `FALSE` all the provided files will be loaded at a once. This operation will be time and memory consuming, filling more than 11GB of RAM memory.
        #' @param file_list List of character vectors. Each vector containing two elements: name and path.
        #' @param lazy_load (default: `TRUE`) Indicates if the files will be loaded once they are required or immediately.
        #' @param eager_unload (default: `TRUE`) Indicates if the files will be un-loaded once they are not required or kept in memory.
        #' @param verbose (default: `TRUE`) If `lazy_load` is set to `FALSE` it will indicates the progression of loading the indicated storage files.
        #' @return A new `ctdStorage` object.
        initialize = function(file_list, lazy_load = TRUE, eager_unload = TRUE, verbose = TRUE) {
            stopifnot(is.list(file_list), length(file_list) >= 1, sum(sapply(file_list, length) == 3) == length(file_list))

            file_names <- tolower(sapply(file_list, "[[", 1))
            files_format <- tolower(sapply(file_list, "[[", 2))

            stopifnot(sum(sapply(files_format, function(x) x %in% c("csv", "tsv"))) == length(file_list))

            file_list <- lapply(file_list, function(x) list("file_path" = x[3], "file_format" = x[2], "file_content" = NA))

            private$file_list <- file_list
            names(private$file_list) <- file_names
            private$lazy_load <- lazy_load
            private$eager_unload <- eager_unload

            if(!lazy_load) {
                if(verbose) message(paste0("Loading storage files (N=", length(private$file_list), ")"))
                for(file_name in names(private$file_list)) {
                    if(verbose) message(paste0("  - Loading file '", file_name, "'"))
                    private$load_storage(file_name)
                }
            }
        },
        #' @description
        #' Shows a brief description of the `ctdStorage`.
        #' @param ... Do not used.
        #' @return A `invisible` object.
        print = function(...) {
            cat("ctdStorage (", length(private$file_list), " source files)\n", sep = "")
            cat("  . lazy load: ", private$lazy_load, "\n", sep = "")
            cat("  . eager unload: ", private$eager_unload, "\n", sep = "")
            return(invisible(self))
        },
        #' @description
        #' It returns a given storage file. If the file it is not loaded at a time, it loads it.
        #' @param file_name Name of the file to be returned.
        #' @return A `data.table` with the content of the file.
        table = function(file_name) {
            private$load_storage(file_name)
            return(private$file_list[[file_name]]$file_content)
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
