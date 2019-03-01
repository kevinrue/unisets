
## extending RTLFile class to GMT ----

#' Import and export
#'
#' The functions `import` and `export` load and save objects from and to particular file formats.
#' The `unisets` package aims to implement support for a number of annotation and sequence formats.
#' The following file formats are currently supported: [GMT](https://software.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats#GMT:_Gene_Matrix_Transposed_file_format_.28.2A.gmt.29).
#'
#' @name io
#' @rdname io
#' @aliases GMTFile-class
#'
#' @export
#' @importClassesFrom rtracklayer RTLFile
setClass("GMTFile", contains="RTLFile")

#' @name io
#' @rdname io
#'
#' @param resource For `GMTFile()`, the .gmt file that will be imported in.
#'
#' @return For `GMTFile()`, an object representing the path to a .gmt file on disk
#'
#' @export
#'
#' @examples
#' # Example file ----
#'
#' gmt <- system.file(package="unisets", "extdata", "example.gmt")
#'
#' # Import ----
#'
#' bs <- import(gmt)
#' bs
#'
#' # Export ----
#'
#' tf <- tempfile(fileext = ".gmt")
#' export(bs, tf)
GMTFile <- function(resource) {
    new("GMTFile", resource=resource)
}

## import.gmt() ----

#' @name io
#' @rdname io
#'
#' @param con The connection from which data is loaded or to which data is saved.
#' If this is a character vector, it is assumed to be a filename
#' and a corresponding file connection is created and then closed after exporting the object.
#' If a [`RTLFile-class`] derivative, the data is loaded from or saved to the underlying resource.
#' Certain subclasses of [`BiMap`][`AnnDbBimap`] are supported: [`Go3AnnDbBimap`].
#' @param ... Parameters to pass to the format-specific method.
#'
#' @importMethodsFrom rtracklayer import
#' @export
import.gmt <- function(con, ...) {
    ## Read in GMT into a list format
    sets <- readLines(con)
    sets <- strsplit(sets, "\t")
    names <- vapply(sets, function(set) set[[1]], character(1))
    genes <- lapply(sets, function(set) set[-(1:2)])
    names(genes) <- names

    ## Produce an error if names contains duplicates
    if (length(unique(names)) != length(names)) {
        dups <- names[duplicated(names)]
        err <- paste0(
            "Duplicated geneset names exist for the sets below. ",
            "Please check your GMT file.\n\n",
            c(paste0(dups, collapse='\n')), '\n\n')
        stop(err)
    }

    ## Convert GMT to DataFrame of element:set of relations
    map <- DataFrame(stack(genes))
    colnames(map) <- c("element", "set")
    map$set <- as.character(map$set)

    ## Extract GMT source (url) to create setData slot
    source <- vapply(sets, function(set) set[[2]], character(1))
    source[source == "NA" | !nzchar(source)] <- NA
    set_data <- IdVector(names)
    mcols(set_data) <- DataFrame(source=source)

    ## Construct and return the BaseSet
    bs <- BaseSets(map, setData=set_data)
    return(bs)
}

#' @name io
#' @rdname io
#' @aliases import import,GMTFile,ANY,ANY-method
#'
#' @param format,text Arguments defined in the [rtracklayer::import()] generic. Currently ignored.
#'
#' @export
#'
#' @importFrom rtracklayer resource
#' @importFrom S4Vectors DataFrame mcols<-
#' @importFrom utils stack
setMethod("import", "GMTFile", function(con, format, text, ...) {
    import.gmt(resource(con), ...)
})


## export.gmt() ----

#' @name io
#' @rdname io
#' @aliases export.gmt
#'
#' @param object An object of class inheriting from [`GMTFile`].
#'
#' @importFrom rtracklayer export
#' @export
export.gmt <- function(object, con, ...) {
    export(object, GMTFile(con), "gmt", ...)
}

#' @name io
#' @rdname io
#' @aliases export export,BaseSets,GMTFile,ANY-method
#'
#' @importFrom rtracklayer export
#' @importFrom utils write.table
#' @importFrom methods getPackageName
#' @importFrom S4Vectors mcols DataFrame
#' @export
setMethod("export", c("BaseSets", "GMTFile"), function(object, con, format, ...) {
    path <- resource(con)
    if (! "source" %in% colnames(mcols(setData(object)))) {
        message(
            "'source' column not found in mcols(setData(object)), ",
            sprintf("setting to \"%s\"", getPackageName()))
        source <- DataFrame(
            source=rep(getPackageName(), nSets(object)),
            row.names=ids(setData(object))
        )
    } else {
        source <- DataFrame(
            source=mcols(setData(object))[["source"]],
            row.names=ids(setData(object))
        )
    }

    ## Collapse into tab separated list
    df <- as.data.frame(object)
    df$source <- source[df$set, ]
    df <- df[order(df$set, df$element), ]
    set_list <- lapply(with(df, split(df, set)), function(x) {
        paste(x$set[1], x$source[1], paste(x$element, collapse="\t"), sep="\t")
    })

    ## Collapse each set list into a row and write out
    out <- paste(unlist(set_list), collapse="\n")
    write.table(out, path, sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)
})

# import.Go3AnnDbBimap() ----

#' @name io
#' @rdname io
#' @aliases import.Go3AnnDbBimap
#'
#' @param Go3AnnDbBimap A [`Go3AnnDbBimap`].
#'
#' @section Coercion to BaseSets:
#' `as(Go3AnnDbBimap, "BaseSets")` and `as.BaseSets(Go3AnnDbBimap)` return a `BaseSets` from a Gene Ontology `Bimap` stored distributed in a Bioconductor annotation package.
#'
#' @author Robert A. Amezquita
#'
#' @importFrom methods as
#' @importFrom AnnotationDbi select columns
#' @export
#'
#' @examples
#'
#' # Import (Go3AnnDbBimap) ----
#'
#' library(org.Hs.eg.db)
#' bs1 <- import(org.Hs.egGO)
#' bs1
import.Go3AnnDbBimap <- function(con, format, text, ...)  {
    # Import the relationships from the annotation BiMap
    relations <- DataFrame(as.data.frame(con))
    # Rename columns: gene_id -> element, go_id -> set, Element -> element; Ontology -> ontology
    protectedColumns <- c("gene_id", "go_id", "Evidence", "Ontology")
    colIdx <- match(protectedColumns, colnames(relations))
    colnames(relations)[colIdx] <- c("element", "set", "evidence", "ontology")

    # Prepare a default empty DataFrame if GO.db is not installed
    setData <- GOIdVector(unique(as.character(relations$set)))
    if ( requireNamespace("GO.db") ) {
        # Fetch GO metadata from GO.db if installed
        db <- GO.db::GO.db
        mcols(setData) <- DataFrame(select(db, ids(setData), columns(db)))
    }

    elementData <- EntrezIdVector(sort(unique(as.character(relations$element))))

    GOSets(relations, elementData, setData)
}

#' @name io
#' @rdname io
#' @aliases import,Go3AnnDbBimap,ANY,ANY-method
#'
#' @importFrom methods setMethod
#' @export
setMethod("import", "Go3AnnDbBimap", function(con, format, text, ...)  {
    import.Go3AnnDbBimap(con, format, text, ...)
})
