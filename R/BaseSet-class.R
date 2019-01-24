# Direct pointers ----

#' @param x An object that inherits from `BaseSets`.
#'
#' @rdname BaseSets-class
#' @aliases relations,BaseSets-method
setMethod("relations", "BaseSets", function(x) {
    slot(x, "relations")
})

#' @param value An object of a class specified in the S4 method signature or as outlined in 'Slots'.
#'
#' @rdname BaseSets-class
#' @aliases relations<-,BaseSets-method
#' @importFrom methods validObject
#' @importFrom methods slot<-
setMethod("relations<-", "BaseSets", function(x, value) {
    slot(x, "relations") <- value
    validObject(x)
    x
})

#' @rdname BaseSets-class
#' @aliases elements,BaseSets-method
setMethod("elements", "BaseSets", function(x) {
    slot(x, "relations")$element
})

#' @rdname BaseSets-class
#' @aliases elementData,BaseSets-method
setMethod("elementData", "BaseSets", function(x) {
    slot(x, "elementData")
})

#' @rdname BaseSets-class
#' @aliases elementData<-,BaseSets-method
#' @importFrom methods validObject
#' @importFrom methods slot<-
setMethod("elementData<-", "BaseSets", function(x, value) {
    slot(x, "elementData") <- value
    validObject(x)
    x
})

#' @rdname BaseSets-class
#' @aliases elementIds,BaseSets-method
setMethod("elementIds", "BaseSets", function(x) {
    rownames(elementData(x))
})

#' @rdname BaseSets-class
#' @aliases elementIds<-,BaseSets-method
#' @importFrom methods validObject
setMethod("elementIds<-", "BaseSets", function(x, value) {
    translation <- value
    names(translation) <- rownames(elementData(x))
    # Rename elements in both relations and metadata tables
    # Use @ directly to avoid triggering the validObject method
    rownames(x@elementData) <- value
    x@relations$element@id <- translation[x@relations$element@id]

    validObject(x)
    x
})

#' @rdname BaseSets-class
#' @aliases sets,BaseSets-method
setMethod("sets", "BaseSets", function(x) {
    slot(x, "relations")$set
})

#' @rdname BaseSets-class
#' @aliases setData,BaseSets-method
setMethod("setData", "BaseSets", function(x) {
    slot(x, "setData")
})

#' @rdname BaseSets-class
#' @aliases setData<-,BaseSets-method
#' @importFrom methods validObject
#' @importFrom methods slot<-
setMethod("setData<-", "BaseSets", function(x, value) {
    slot(x, "setData") <- value
    validObject(x)
    x
})

#' @rdname BaseSets-class
#' @aliases setIds,BaseSets-method
setMethod("setIds", "BaseSets", function(x) {
    rownames(setData(x))
})

#' @rdname BaseSets-class
#' @aliases setIds<-,BaseSets-method
#' @importFrom methods validObject
setMethod("setIds<-", "BaseSets", function(x, value) {
    translation <- value
    names(translation) <- rownames(setData(x))
    # Rename sets in both relations and metadata tables
    # Use @ directly to avoid triggering the validObject method
    rownames(x@setData) <- value
    x@relations$set@id <- translation[x@relations$set@id]

    validObject(x)
    x
})

# Dimensions ----

#' @rdname BaseSets-class
#' @aliases nRelations,BaseSets-method
setMethod("nRelations", "BaseSets", function(x) {
    nrow(relations(x))
})

#' @rdname BaseSets-class
#' @aliases nElements,BaseSets-method
setMethod("nElements", "BaseSets", function(x) {
    nrow(elementData(x))
})

#' @rdname BaseSets-class
#' @aliases nSets,BaseSets-method
setMethod("nSets", "BaseSets", function(x) {
    nrow(setData(x))
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

    x <- split(
        relations(x)$set@id,
        relations(x)$element@id)
    lengths(x)
})

# subset() ----

setMethod("subset", "BaseSets", function(x, ...) {
    .local <- function (x, subset, select, drop=FALSE, ...) {
        # Match code layout of the FuzzySets method
        table <- relations(x)
        # Coerce to character
        table$element <- as.character(table$element)
        table$set <- as.character(table$set)

        i <- eval(substitute(subset), table)
        relations <- relations(x)[i, , drop=FALSE]

        elementData <- elementData(x)[unique(relations$element), , drop=FALSE]
        setData <- setData(x)[unique(relations$set), , drop=FALSE]

        BaseSets(relations, elementData, setData)
    }
    .local(x, ...)
})

# show() ----

setMethod("show", "BaseSets", function(object) {
    # Format the object
    x <- relations(object)
    # Use [[ to nest the DataFrame
    x[["elementData"]] <- elementData(object)[x$element, , drop=FALSE]
    x[["setData"]] <- setData(object)[x$set, , drop=FALSE]

    .showSetAsTable(class(object), x)
})

# as.list() ----

setAs("BaseSets", "list", function(from) {
    split(
        relations(from)$element@id,
        relations(from)$set@id)
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
    x <- cbind(as.data.frame(relations(from)), value=TRUE)
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
#' @importFrom S4Vectors DataFrame
setAs("Go3AnnDbBimap", "BaseSets", function(from) {
    # Import the relationships from the annotation BiMap
    dataframe <- DataFrame(as.data.frame(from))
    relations <- dataframe[, c("gene_id", "go_id")]
    colnames(relations) <- c("element", "set")

    # Coerce to dedicated classes
    relations$element <- EntrezIdVector(relations$element)

    # Prepare a default empty DataFrame if GO.db is not installed
    setData <- DataFrame(row.names=unique(relations$set))
    if ( requireNamespace("GO.db") ) {
        db <- GO.db::GO.db
        # Fetch GO metadata from GO.db if installed
        setData <- select(db, keys(db), columns(db))
        setData <- DataFrame(setData)
        rownames(setData) <- setData$GOID
        setData$GOID <- NULL
        setData <- setData[unique(relations$set), , drop=FALSE]
    }

    BaseSets(relations, setData=setData)
})

#' @aliases as.BaseSets.Go3AnnDbBimap as.BaseSets
#' @importFrom methods as
as.BaseSets.Go3AnnDbBimap <- function(x, ...) {
    as(x, "BaseSets")
}
