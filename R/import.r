## credit to Michael Lawrence
## implementation is largely based on:
## https://github.com/lawremi/rtracklayer/blob/master/R/bed.R

## extending RTLFile class to GMT ----
#' @importClassesFrom rtracklayer RTLFile
setGeneric("import.gmt", function(con, ...) standardGeneric("import.gmt"))

## import() ----
#' @importFrom rtracklayer import
setMethod("import.gmt", "ANY", function(con, ...) {
    import(con, format = "gmt", ...)
})

#' @importFrom rtracklayer import 
#' @importFrom S4Vectors DataFrame
#' @importFrom utils stack
setMethod("import", "GMTFile", function(con, format, text...) {
    ## import.gmt <- function(path) 
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
              
   
