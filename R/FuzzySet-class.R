# Direct pointers ----

#' @param x An object that inherits from `FuzzySets`.
#'
#' @rdname FuzzySets-class
#' @aliases relations,FuzzySets-method
#' @importFrom S4Vectors DataFrame
setMethod("relations", "FuzzySets", function(x) {
    out <- relations(as(x, "BaseSets"))
    out$membership <- membership(x)
    out
})

#' @rdname FuzzySets-class
#' @aliases membership,FuzzySets-method
setMethod("membership", "FuzzySets", function(x) {
    membership(x@relations)
})

#' @param value An object of a class specified in the S4 method signature or as outlined in 'Slots'.
#'
#' @rdname FuzzySets-class
#' @aliases membership<-,FuzzySets-method
#' @importFrom methods validObject
setMethod("membership<-", "FuzzySets", function(x, value) {
    membership(x@relations) <- value
    validObject(x)
    x
})

# subset ----

setMethod("subset", "FuzzySets", function(x, ...) {
    .local <- function (x, subset, select, drop=FALSE, ...) {
        # Only difference with the BaseSets parent method
        table <- cbind(as(x, "data.frame"), membership=membership(x@relations))
        i <- eval(substitute(subset), table)

        keep.element <- unique(id(elementData(x))[from(x@relations)[i]])
        keep.set <- unique(id(setData(x))[to(x@relations)[i]])

        relations <- DataFrame(table[i, c("element", "set"), drop=FALSE])
        elementData <- elementData(x)[which(id(elementData(x)) %in% keep.element)]
        setData <- setData(x)[which(id(setData(x)) %in% keep.set)]
        membership <- membership(x@relations)[i]

        fs <- BaseSets(relations, elementData, setData)
        # Upgrade Hits to FuzzyHits
        relations <- new("FuzzyHits", fs@relations, membership=membership)
        fs@relations <- relations
        fs <- new("FuzzySets", fs)
        validObject(fs)
        fs
    }
    .local(x, ...)
})

# show() ----

setMethod("show", "FuzzySets", function(object) {
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
    x[["membership"]] <- membership(object@relations)
    x[["elementData"]] <- elementData
    x[["setData"]] <- setData

    .showSetAsTable(class(object), x)
})

#' @rdname FuzzySets-class
#' @aliases as.list.FuzzySets
#' @importFrom methods as
#' @export
as.list.FuzzySets <- function(x, ...) {
    as(x, "list")
}

#' @param fill Value with which to fill in structural missings, defaults to 0 (element is not a member of the set).
#'
#' @section Conversion to matrix:
#' As it is possible to store multiple relations between the same gene and gene set, it may be necessary to collapse multiple observations of the membership function into a single value.
#' This can be controlled using the `fun.aggregate` argument passed down to the `acast` function.
#' See examples below.
#'
#' @rdname FuzzySets-class
#' @aliases as.matrix.FuzzySets
#' @importFrom methods as
#' @importFrom reshape2 acast
#' @export
#'
#' @examples
#' # Converting to matrix: multiple observations ----
#'
#' as.matrix(fs, fun.aggregate=min)
#'
as.matrix.FuzzySets <- function(x, ..., fill=0) {
    out <- as(x, "data.frame")
    out[["value"]] <- membership(x@relations)
    out <- acast(out, element~set, value.var="value", fill=fill, ...)
    out
}

#' @importFrom reshape2 acast
setAs("FuzzySets", "matrix", function(from) {
    as.matrix.FuzzySets(from)
})

#' @importFrom reshape2 melt
#' @importFrom S4Vectors DataFrame
setAs("matrix", "FuzzySets", function(from) {
    storage.mode(from) <- "double"
    x <- melt(from, varnames=c("element", "set"), as.is=TRUE)
    x <- x[!is.na(x$value), , drop=FALSE]
    relations <- x[, c("element", "set"), drop=FALSE]
    relations <- DataFrame(relations)
    membership <- x$value
    FuzzySets(relations, membership=membership)
})

#' @aliases as.FuzzySets.matrix as.FuzzySets
#' @importFrom methods as
as.FuzzySets.matrix <- function(x, ...) {
    as(x, "FuzzySets")
}

#' @importFrom methods new
setAs("BaseSets", "FuzzySets", function(from) {
    fuzzyhits <- as(from@relations, "FuzzyHits")
    membership(fuzzyhits) <- rep(1, nRelations(from))
    from@relations <- fuzzyhits
    x <- new("FuzzySets", from)
    validObject(x)
    x
})

#' @aliases as.FuzzySets.BaseSets as.FuzzySets
#' @importFrom methods as
as.FuzzySets.BaseSets <- function(x, ...) {
    as(x, "FuzzySets")
}
