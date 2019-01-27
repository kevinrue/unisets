# Direct pointers ----

#' @param x An object that inherits from `BaseSets`.
#'
#' @rdname BaseSets-class
#' @aliases relations,BaseSets-method
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

#' @rdname BaseSets-class
#' @aliases elementData,BaseSets-method
setMethod("elementData", "BaseSets", function(x) {
    slot(x, "elementData")
})

#' @rdname BaseSets-class
#' @aliases elements,BaseSets-method
#' @importFrom S4Vectors from
setMethod("elements", "BaseSets", function(x) {
    elementData(x)[from(x@relations)]
})

#' @rdname BaseSets-class
#' @aliases elementIds,BaseSets-method
setMethod("elementIds", "BaseSets", function(x) {
    id(elementData(x))
})

#' @param value An object of a class specified in the S4 method signature or as outlined in 'Slots'.
#'
#' @rdname BaseSets-class
#' @aliases elementIds<-,BaseSets-method
#' @importFrom methods validObject
setMethod("elementIds<-", "BaseSets", function(x, value) {
    x@elementData@id <- value
    validObject(x)
    x
})

#' @rdname BaseSets-class
#' @aliases setData,BaseSets-method
setMethod("setData", "BaseSets", function(x) {
    slot(x, "setData")
})

#' @rdname BaseSets-class
#' @aliases sets,BaseSets-method
#' @importFrom S4Vectors to
setMethod("sets", "BaseSets", function(x) {
    setData(x)[to(x@relations)]
})

#' @rdname BaseSets-class
#' @aliases setIds,BaseSets-method
setMethod("setIds", "BaseSets", function(x) {
    id(setData(x))
})

#' @rdname BaseSets-class
#' @aliases setIds<-,BaseSets-method
#' @importFrom methods validObject
setMethod("setIds<-", "BaseSets", function(x, value) {
    x@setData@id <- value
    validObject(x)
    x
})

# Dimensions ----

#' @rdname BaseSets-class
#' @aliases length,BaseSets-method
setMethod("length", "BaseSets", function(x) {
    length(x@relations)
})

#' @rdname BaseSets-class
#' @aliases nElements,BaseSets-method
setMethod("nElements", "BaseSets", function(x) {
    length(elementData(x))
})

#' @rdname BaseSets-class
#' @aliases nSets,BaseSets-method
setMethod("nSets", "BaseSets", function(x) {
    length(setData(x))
})

#' @rdname BaseSets-class
#' @aliases setLengths,BaseSets-method
#' @importFrom methods as
setMethod("setLengths", "BaseSets", function(x) {
    x <- as(x, "list")
    lengths(x)
})

#' @rdname BaseSets-class
#' @aliases elementLengths,BaseSets-method
#' @importFrom methods as
setMethod("elementLengths", "BaseSets", function(x) {
    # Note the difference between the argument and the method 'from'
    x <- as(x, "DataFrame")
    x <- split(x$set, x$element)
    lengths(x)
})

# subset() ----

setMethod("subset", "BaseSets", function(x, ...) {
    .local <- function (x, subset, select, drop=FALSE, ...) {
        # Match code layout of the FuzzySets method
        table <- as(x, "data.frame")
        i <- eval(substitute(subset), table)

        keep.element <- unique(id(elementData(x))[from(x@relations)[i]])
        keep.set <- unique(id(setData(x))[to(x@relations)[i]])

        relations <- DataFrame(table[i, , drop=FALSE])
        elementData <- elementData(x)[which(id(elementData(x)) %in% keep.element)]
        setData <- setData(x)[which(id(setData(x)) %in% keep.set)]

        BaseSets(relations, elementData, setData)
    }
    .local(x, ...)
})

# show() ----

#' @importFrom S4Vectors elementMetadata
setMethod("show", "BaseSets", function(object) {
    # Combine elementData, setData, and relations into a single DataFrame
    element <- elementData(object)[from(object@relations)]
    elementData <- elementMetadata(element)
    elementMetadata(element) <- NULL # avoid metadata columns
    set <- setData(object)[to(object@relations)]
    setData <- elementMetadata(set)
    elementMetadata(set) <- NULL # avoid metadata columns
    x <- DataFrame(
        element=element,
        set=set
    )
    x[["relationData"]] <- elementMetadata(object@relations)
    x[["elementData"]] <- elementData
    x[["setData"]] <- setData

    .showSetAsTable(class(object), x)
})

# as.data.frame.BaseSets() ----

setAs("BaseSets", "DataFrame", function(from) {
    relations(from)
})

setAs("BaseSets", "data.frame", function(from) {
    x <- as(from, "DataFrame")
    x <- as(x, "data.frame")
    x
})

# split() ----

setMethod("split", c("IdVector", "IdVector"), function(x, f, drop = FALSE, ...)  {
    split(x, as.vector(f), drop=drop, ...)
})

# as.list() ----

setAs("BaseSets", "list", function(from) {
    x <- as(from, "DataFrame")
    split(x$element, x$set)
})

#' @param ... Additional arguments passed to and from methods.
#'
#' @rdname BaseSets-class
#' @aliases as.list.BaseSets as.list
#' @importFrom methods as
#' @export
as.list.BaseSets <- function(x, ...) {
    as(x, "list")
}

# as.matrix() ----

#' @importFrom reshape2 acast
setAs("BaseSets", "matrix", function(from) {
    x <- as(from, "data.frame")
    x[["value"]] <- TRUE
    out <- acast(x, element~set, value.var="value", fun.aggregate=any, fill=FALSE)
    out
})

#' @rdname BaseSets-class
#' @aliases as.matrix.BaseSets as.matrix
#' @importFrom methods as
#' @export
as.matrix.BaseSets <- function(x, ...) {
    as(x, "matrix")
}

# as.BaseSets() ----

#' @importFrom reshape2 melt
#' @importFrom S4Vectors DataFrame
setAs("matrix", "BaseSets", function(from) {
    storage.mode(from) <- "logical"
    x <- melt(from, varnames=c("element", "set"), as.is=TRUE)
    x <- x[which(x$value), c("element", "set"), drop=FALSE]
    x <- DataFrame(x)
    BaseSets(x)
})

#' @aliases as.BaseSets.matrix as.BaseSets
#' @importFrom methods as
as.BaseSets.matrix <- function(x, ...) {
    as(x, "BaseSets")
}

#' @importClassesFrom AnnotationDbi Go3AnnDbBimap
#' @importFrom AnnotationDbi select keys columns
#' @importFrom S4Vectors DataFrame elementMetadata<-
setAs("Go3AnnDbBimap", "BaseSets", function(from) {
    # Import the relationships from the annotation BiMap
    relations <- DataFrame(as.data.frame(from))
    # Rename columns: gene_id -> element, go_id -> set
    colIdx <- match(c("gene_id", "go_id"), colnames(relations))
    colnames(relations)[colIdx] <- c("element", "set")

    # Prepare a default empty DataFrame if GO.db is not installed
    setData <- IdVector(unique(relations$set))
    if ( requireNamespace("GO.db") ) {
        # Fetch GO metadata from GO.db if installed
        db <- GO.db::GO.db
        setData <- IdVector(keys(db))
        elementMetadata(setData) <- DataFrame(select(db, keys(db), columns(db)))
    }

    elementData <- EntrezIdVector(sort(unique(relations$element)))

    BaseSets(relations, elementData, setData)
})

#' @aliases as.BaseSets.Go3AnnDbBimap as.BaseSets
#' @importFrom methods as
as.BaseSets.Go3AnnDbBimap <- function(x, ...) {
    as(x, "BaseSets")
}
