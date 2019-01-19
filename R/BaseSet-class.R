# Direct pointers ----

#' @param x An object that inherits from `BaseSets`.
#'
#' @rdname BaseSets-class
#' @aliases relations,BaseSets-method
setMethod("relations", "BaseSets", function(x)  {
    slot(x, "relations")
})

#' @param value An object of a class specified in the S4 method signature or as outlined in 'Slots'.
#'
#' @rdname BaseSets-class
#' @aliases relations<-,BaseSets-method
#' @importFrom methods validObject
setMethod("relations<-", "BaseSets", function(x, value)  {
    slot(x, "relations") <- value
    validObject(x)
    x
})

#' @rdname BaseSets-class
#' @aliases elementData,BaseSets-method
setMethod("elementData", "BaseSets", function(x)  {
    slot(x, "elementData")
})

#' @rdname BaseSets-class
#' @aliases elementData<-,BaseSets-method
#' @importFrom methods validObject
setMethod("elementData<-", "BaseSets", function(x, value)  {
    slot(x, "elementData") <- value
    validObject(x)
    x
})

#' @rdname BaseSets-class
#' @aliases elementIds,BaseSets-method
setMethod("elementIds", "BaseSets", function(x)  {
    rownames(elementData(x))
})

#' @rdname BaseSets-class
#' @aliases elementIds<-,BaseSets-method
#' @importFrom methods validObject
setMethod("elementIds<-", "BaseSets", function(x, value)  {
    translation <- value
    names(translation) <- rownames(elementData(x))
    # Rename elements in both relations and metadata tables
    # Use @ directly to avoid triggering the validObject method
    rownames(x@elementData) <- value
    x@relations$element <- translation[x@relations[, "element"]]

    validObject(x)
    x
})

#' @rdname BaseSets-class
#' @aliases setData,BaseSets-method
setMethod("setData", "BaseSets", function(x)  {
    slot(x, "setData")
})

#' @rdname BaseSets-class
#' @aliases setData<-,BaseSets-method
#' @importFrom methods validObject
setMethod("setData<-", "BaseSets", function(x, value)  {
    slot(x, "setData") <- value
    validObject(x)
    x
})

#' @rdname BaseSets-class
#' @aliases setIds,BaseSets-method
setMethod("setIds", "BaseSets", function(x)  {
    rownames(setData(x))
})

#' @rdname BaseSets-class
#' @aliases setIds<-,BaseSets-method
#' @importFrom methods validObject
setMethod("setIds<-", "BaseSets", function(x, value)  {
    translation <- value
    names(translation) <- rownames(setData(x))
    # Rename sets in both relations and metadata tables
    # Use @ directly to avoid triggering the validObject method
    rownames(x@setData) <- value
    x@relations[, "set"] <- translation[x@relations[, "set"]]

    validObject(x)
    x
})

# Dimensions ----

#' @rdname BaseSets-class
#' @aliases nRelations,BaseSets-method
setMethod("nRelations", "BaseSets", function(x)  {
    nrow(relations(x))
})

#' @rdname BaseSets-class
#' @aliases nElements,BaseSets-method
setMethod("nElements", "BaseSets", function(x)  {
    nrow(elementData(x))
})

#' @rdname BaseSets-class
#' @aliases nSets,BaseSets-method
setMethod("nSets", "BaseSets", function(x)  {
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
        relations(x)$set,
        relations(x)$element)
    lengths(x)
})

# subset ----

setMethod("subset", "BaseSets", function(x, ...)  {
    .local <- function (x, subset, select, drop=FALSE, ...) {
        # Match code layout of the FuzzySets method
        table <- relations(x)

        i <- eval(substitute(subset), table)
        relations <- relations(x)[i, , drop=FALSE]

        elementData <- elementData(x)[unique(relations$element), , drop=FALSE]
        setData <- setData(x)[unique(relations$set), , drop=FALSE]

        BaseSets(relations, elementData, setData)
    }
    .local(x, ...)
})

setMethod("show", "BaseSets", function(object) {
    # Format the object
    x <- relations(object)
    # Use [[ to nest the DataFrame
    x[["elementData"]] <- elementData(object)[x$element, , drop=FALSE]
    x[["setData"]] <- setData(object)[x$set, , drop=FALSE]

    .showSetAsTable(class(object), x)
})

setAs("BaseSets", "list", function(from) {
    split(
        relations(from)$element,
        relations(from)$set)
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

#' @importFrom reshape2 acast
setAs("BaseSets", "matrix", function(from) {
    x <- cbind(as.data.frame(relations(from)), value=TRUE)
    out <- acast(x, element~set, value.var="value", fill=FALSE)
    out
})

#' @rdname BaseSets-class
#' @aliases as.matrix.BaseSets as.matrix
#' @importFrom methods as
#' @export
as.matrix.BaseSets <- function(x, ...)  {
    as(x, "matrix")
}

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
as.BaseSets.matrix <- function(x, ...)  {
    as(x, "BaseSets")
}
