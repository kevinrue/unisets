
## extending RTLFile class to GMT ----

#' GMTFile Class
#'
#' @rdname import
#'
#' @export
#' @importClassesFrom rtracklayer RTLFile
setClass("GMTFile", contains = "RTLFile")

#' @param resource For `GMTFile()`, the .gmt file that will be imported in.
#'
#' @rdname import
#'
#' @return For `GMTFile()`, an object representing the path to a .gmt file on disk
#'
#' @export
GMTFile <- function(resource) {
    new("GMTFile", resource = resource)
}

## credit to Michael Lawrence
## implementation is largely based on:
## https://github.com/lawremi/rtracklayer/blob/master/R/bed.R

setGeneric("import.gmt", function(con, ...) standardGeneric("import.gmt"))

## import() ----

#' @importMethodsFrom rtracklayer import
#' @importFrom rtracklayer FileForFormat
setMethod("import.gmt", "ANY", function(con, ...) {
    x <- FileForFormat(con)
    print(x)
    # print(file.exists(resource(x)))
    import(x, ...)
})

#' @importFrom rtracklayer resource
#' @importFrom S4Vectors DataFrame
#' @importFrom utils stack
setMethod("import", "GMTFile", function(con, format, text...) {
    print("import GMTFile")
    ## Read in GMT into a list format
    path <- resource(con)
    sets <- readLines(path)
    sets <- strsplit(sets, "\t")
    names <- vapply(sets, function(set) set[[1]], character(1))
    genes <- lapply(sets, function(set) set[-(1:2)])
    names(genes) <- names

    ## Produce an error if names contains duplicates
    if (length(unique(names)) != length(names)) {
        dups <- names[duplicated(names)]
        err <- paste0("Duplicated geneset set names exist for the sets below. ",
            "Please check your GMT file.\n\n",
            c(paste0(dups, collapse = '\n')), '\n\n')
        stop(err)
    }

    ## Convert GMT to DataFrame of element:set of relations
    map <- DataFrame(stack(genes))
    colnames(map) <- c("element", "set")
    map$set <- as.character(map$set)

    ## Extract GMT source (url) to create setData slot
    source <- vapply(sets, function(set) set[[2]], character(1))
    source[source=="NA" | !nzchar(source)] <- NA
    set_data <- IdVector(id = names)
    elementMetadata(set_data) <- DataFrame(source = source)

    ## Construct and return the BaseSet
    bs <- BaseSets(map, setData = set_data)
    return(bs)
})


## export() ----

#' @importClassesFrom rtracklayer RTLFile
setGeneric("export.gmt", function(object, con, ...) standardGeneric("export.gmt"))

#' @importFrom rtracklayer export
setMethod("export.gmt", "ANY", function(object, con, ...) {
    export(object, con, "gmt", ...)
})

#' @importFrom rtracklayer export
#' @importFrom utils write.table
setMethod("export", c("BaseSets", "GMTFile"), function(object, con, format, ...) {
    x <- object
    path <- resource(con)
    if (!"source" %in% colnames(elementMetadata(setData(x)))) {
        message("'source' column not found in elementMetadata(setData(x)), setting to NA for export")
        source <- DataFrame(source = rep(NA_character_, nSets(x)),
                            row.names = id(setData(x)))
    } else {
        source <- DataFrame(source = elementMetadata(setData(x))[["source"]],
                            row.names = id(setData(x)))
    }

    ## Collapse into tab separated list
    df <- data.frame(relations(x))
    df$source <- source[df$set, ]
    df <- df[order(df$set, df$element), ]
    set_list <- lapply(with(df, split(df, set)), function(x) {
        paste(x$set[1], x$source[1],
              paste(x$element, collapse = "\t"),
              sep = "\t")
    })

    ## Collapse each set list into a row and write out
    out <- paste(unlist(set_list), collapse = "\n")
    write.table(out, path, sep = "\t",
                col.names = FALSE, row.names = FALSE, quote = FALSE)
})
