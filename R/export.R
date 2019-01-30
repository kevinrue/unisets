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
    
    
          
