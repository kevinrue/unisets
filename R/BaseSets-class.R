
# Accessors ----

#' @rdname BaseSets-methods
#' @aliases relations,BaseSets-method
#'
#' @section Accessors:
#' `relations(x)` returns a `DataFrame` of relations including `"element"`, `"set"`, and any relation-wise metadata available.
#'
#' @importFrom S4Vectors DataFrame
setMethod("relations", "BaseSets", function(x) {
    # use the built-in conversion of Hits to DataFrame
    out <- as(x@relations, "DataFrame")
    colnames(out)[1:2] <- c("element", "set")
    # Substitute from/to by the corresponding identifiers
    out$element <- elementData(x)[out$element]
    out$set <- setData(x)[out$set]
    out
})

#' @rdname BaseSets-methods
#' @aliases elementData,BaseSets-method
#'
#' @section Accessors:
#' `elementData(x)` returns the metadata associated with each element as an object inheriting from [`IdVector`][IdVector-class].
setMethod("elementData", "BaseSets", function(x) {
    slot(x, "elementData")
})

#' @rdname BaseSets-methods
#' @aliases elements,BaseSets-method
#'
#' @section Accessors:
#' `elements(x)` returns an `IdVector` of sets and associated metadata as ordered in `relations(x)$element`.
#'
#' @importFrom S4Vectors from
setMethod("elements", "BaseSets", function(x) {
    elementData(x)[from(x@relations)]
})

#' @rdname BaseSets-methods
#' @aliases elementIds,BaseSets-method
#'
#' @section Accessors:
#' `elementIds(x)` and `elementIds(x) <- value` get and set the `character` vector of element identifiers.
setMethod("elementIds", "BaseSets", function(x) {
    ids(elementData(x))
})

#' @rdname BaseSets-methods
#' @aliases elementIds<-,BaseSets-method
#'
#' @importFrom methods validObject
setReplaceMethod("elementIds", "BaseSets",
    function(x, value)
    {
        x@elementData@ids <- value
        validObject(x)
        x
    }
)

#' @rdname BaseSets-methods
#' @aliases setData,BaseSets-method
#'
#' @section Accessors:
#' `setData(x)` returns the metadata associated with each set as an object inheriting from [`IdVector`][IdVector-class].
setMethod("setData", "BaseSets", function(x) {
    slot(x, "setData")
})

#' @rdname BaseSets-methods
#' @aliases sets,BaseSets-method
#'
#' @section Accessors:
#' `sets(x)` returns an `IdVector` of sets and associated metadata as ordered in `relations(x)$set`.
#'
#' @importFrom S4Vectors to
setMethod("sets", "BaseSets", function(x) {
    setData(x)[to(x@relations)]
})

#' @rdname BaseSets-methods
#' @aliases setIds,BaseSets-method
#'
#' @section Accessors:
#' `setIds(x)` and `setIds(x) <- value` get and set the `character` vector of element identifiers.
setMethod("setIds", "BaseSets", function(x) {
    ids(setData(x))
})

#' @rdname BaseSets-methods
#' @aliases setIds<-,BaseSets-method
#'
#' @importFrom methods validObject
setReplaceMethod("setIds", "BaseSets",
    function(x, value)
    {
        x@setData@ids <- value
        validObject(x)
        x
    }
)

# Dimensions ----

#' @rdname BaseSets-methods
#' @aliases length,BaseSets-method
#'
#' @section Dimensions:
#' `length(x)` returns the total count of relations.
setMethod("length", "BaseSets", function(x) {
    length(x@relations)
})

#' @rdname BaseSets-methods
#' @aliases nElements,BaseSets-method
#'
#' @section Dimensions:
#' `nElements(x)` returns the count of unique elements.
setMethod("nElements", "BaseSets", function(x) {
    length(elementData(x))
})

#' @rdname BaseSets-methods
#' @aliases nSets,BaseSets-method
#'
#' @section Dimensions:
#' `nSets(x)` returns the count of unique sets.
setMethod("nSets", "BaseSets", function(x) {
    length(setData(x))
})

#' @rdname BaseSets-methods
#' @aliases setLengths,BaseSets-method
#'
#' @section Dimensions:
#' `setLengths(x)` returns the count of relations per set.
#'
#' @importFrom methods as
setMethod("setLengths", "BaseSets", function(x) {
    x <- as(x, "list")
    lengths(x)
})

#' @rdname BaseSets-methods
#' @aliases elementLengths,BaseSets-method
#'
#' @section Dimensions:
#' `elementLengths(x)` returns the count of relations per element.
#'
#' @importFrom methods as
setMethod("elementLengths", "BaseSets", function(x) {
    # Note the difference between the argument and the method 'from'
    x <- as(x, "DataFrame")
    x <- split(x$set, x$element)
    lengths(x)
})

# subset() ----

#' @rdname BaseSets-methods
#' @aliases subset.BaseSets subset,BaseSets-method
#'
#' @param ... Additional arguments passed to and from other methods.
#'
#' @section Subsetting:
#' `subset(x, subset, ...)` returns subsets of relations which meet conditions.
#' For `FuzzySets` objects, the `subset` argument should be a logical expression referring to any of `"element"`, `"set"`, and any available relation metadata indicating elements or rows to keep: missing values are taken as false.
#' In addition, metadata for elements and sets that are not represented in the remaining relations are also dropped.
#'
#' @importFrom methods as
#' @importFrom BiocGenerics eval unique
#' @importFrom S4Vectors from to subset
#' @method subset BaseSets
#' @export
subset.BaseSets <- function(x, ...) subset(x, ...)

setMethod("subset", "BaseSets", function(x, ...) {
    .local <- function(x, subset, select, drop=FALSE, ...) {
        # Match code layout of the FuzzySets method
        table <- as(x, "data.frame")
        i <- eval(substitute(subset), table)

        keep.element <- unique(ids(elementData(x))[from(x@relations)[i]])
        keep.set <- unique(ids(setData(x))[to(x@relations)[i]])

        relations <- DataFrame(table[i, , drop=FALSE])
        elementData <- elementData(x)[which(ids(elementData(x)) %in% keep.element)]
        setData <- setData(x)[which(ids(setData(x)) %in% keep.set)]

        BaseSets(relations, elementData, setData)
    }
    .local(x, ...)
})

# show() ----

#' @importFrom S4Vectors mcols
setMethod("show", "BaseSets", function(object) {
    # Combine elementData, setData, and relations into a single DataFrame
    element <- elementData(object)[from(object@relations)]
    elementData <- mcols(element)
    mcols(element) <- NULL # avoid metadata columns
    set <- setData(object)[to(object@relations)]
    setData <- mcols(set)
    mcols(set) <- NULL # avoid metadata columns
    x <- DataFrame(
        element=element,
        set=set
    )
    x[["relationData"]] <- mcols(object@relations)
    x[["elementData"]] <- elementData
    x[["setData"]] <- setData

    .showSetAsTable(class(object), x)
})

# as.data.frame.BaseSets() ----

#' @rdname BaseSets-methods
#' @aliases as.DataFrame.BaseSets as.DataFrame
#'
#' @section Coercion:
#' `as(x, "DataFrame")` and `as.DataFrame(x)` return a `DataFrame` including `"element"`, `"set"`, and any relation-wise metadata available.
#'
#' @importFrom methods as
#' @export
as.DataFrame.BaseSets <- function(x, ...) {
    relations(x)
}

setAs("BaseSets", "DataFrame", function(from) {
    as.DataFrame.BaseSets(from)
})

#' @rdname BaseSets-methods
#' @aliases as.data.frame.BaseSets as.data.frame
#'
#' @section Coercion:
#' `as(x, "data.frame")` and `as.data.frame(x)`  return a `data.frame` including `"element"`, `"set"`, and any relation-wise metadata available.
#'
#' @importFrom methods as
#' @export
as.data.frame.BaseSets <- function(x, ...) {
    out <- as(x, "DataFrame")
    out <- as(out, "data.frame")
    out
}

setAs("BaseSets", "data.frame", function(from) {
    as.data.frame.BaseSets(from)
})

# as.list() ----

#' @rdname BaseSets-methods
#' @aliases as.list.BaseSets as.list
#'
#' @section Coercion:
#' `as(x, "list")` and `as.list(x)` return a `data.frame` including `"element"`, `"set"`, and any relation-wise metadata available.
#'
#' @importFrom methods as
#' @export
as.list.BaseSets <- function(x, ...) {
    out <- as(x, "DataFrame")
    split(out$element, out$set)
}

setAs("BaseSets", "list", function(from) {
    as.list.BaseSets(from)
})

# as.matrix() ----

#' @rdname BaseSets-methods
#' @aliases as.matrix.BaseSets as.matrix
#'
#' @section Coercion:
#' `as(x, "matrix")` and `as.matrix(x)` return a `matrix` with elements as rows, sets as columns, and a `logical` value to indicate membership.
#'
#' @importFrom methods as
#' @export
as.matrix.BaseSets <- function(x, ...) {
    out <- as(x, "data.frame")
    out[["value"]] <- TRUE
    out <- acast(out, element~set, value.var="value", fun.aggregate=any, fill=FALSE)
    out
}

#' @importFrom reshape2 acast
setAs("BaseSets", "matrix", function(from) {
    as.matrix.BaseSets(from)
})

# as.BaseSets.matrix() ----

#' @rdname BaseSets-methods
#' @aliases as.BaseSets.matrix as.BaseSets
#'
#' @param matrix A `matrix`.
#' The matrix will be coerced to `logical` type and relations indicating `TRUE` will be stored in the `BaseSets`.
#'
#' @importFrom methods as
#' @export
as.BaseSets.matrix <- function(matrix, ...) {
    storage.mode(matrix) <- "logical"
    out <- melt(matrix, varnames=c("element", "set"), as.is=TRUE)
    out <- out[which(out$value), c("element", "set"), drop=FALSE]
    out <- DataFrame(out)
    BaseSets(out)
}

#' @importFrom reshape2 melt
#' @importFrom S4Vectors DataFrame
setAs("matrix", "BaseSets", function(from) {
    as.BaseSets.matrix(from)
})

# as.BaseSets.Go3AnnDbBimap() ----

#' @rdname BaseSets-methods
#' @aliases as.BaseSets.Go3AnnDbBimap as.BaseSets
#'
#' @param Go3AnnDbBimap A [`Go3AnnDbBimap`].
#'
#' @importFrom methods as
#' @export
as.BaseSets.Go3AnnDbBimap <- function(Go3AnnDbBimap, ...) {
    # Import the relationships from the annotation BiMap
    relations <- DataFrame(as.data.frame(Go3AnnDbBimap))
    # Rename columns: gene_id -> element, go_id -> set
    colIdx <- match(c("gene_id", "go_id"), colnames(relations))
    colnames(relations)[colIdx] <- c("element", "set")

    # Prepare a default empty DataFrame if GO.db is not installed
    setData <- IdVector(unique(as.character(relations$set)))
    if ( requireNamespace("GO.db") ) {
        # Fetch GO metadata from GO.db if installed
        db <- GO.db::GO.db
        setData <- IdVector(keys(db))
        mcols(setData) <- DataFrame(select(db, keys(db), columns(db)))
    }

    elementData <- EntrezIdVector(sort(unique(as.character(relations$element))))

    BaseSets(relations, elementData, setData)
}

#' @importClassesFrom AnnotationDbi Go3AnnDbBimap
#' @importFrom AnnotationDbi select keys columns
#' @importFrom S4Vectors DataFrame mcols<-
setAs("Go3AnnDbBimap", "BaseSets", function(from) {
    as.BaseSets.Go3AnnDbBimap(from)
})
