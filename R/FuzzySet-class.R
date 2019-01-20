# Direct pointers ----

#' @rdname FuzzySets-class
#' @aliases membership,FuzzySets-method
setMethod("membership", "FuzzySets", function(x)  {
    x@membership
})

#' @param value An object of a class specified in the S4 method signature or as outlined in 'Slots'.
#'
#' @rdname FuzzySets-class
#' @aliases membership<-,FuzzySets-method
#' @importFrom methods validObject
setMethod("membership<-", "FuzzySets", function(x, value)  {
    x@membership <- value
    validObject(x)
    x
})

# subset ----

setMethod("subset", "FuzzySets", function(x, ...)  {
    .local <- function (x, subset, select, drop=FALSE, ...) {
        # Only difference with the BaseSets parent method
        table <- cbind(relations(x), membership=membership(x))

        i <- eval(substitute(subset), table)
        relations <- relations(x)[i, , drop=FALSE]
        membership <- membership(x)[i]

        elementData <- elementData(x)[unique(relations$element), , drop=FALSE]
        setData <- setData(x)[unique(relations$set), , drop=FALSE]

        fs <- new(
            "FuzzySets",
            relations=relations,
            elementData=elementData,
            setData=setData,
            membership=membership)
        validObject(fs)
        fs
    }
    .local(x, ...)
})

setMethod("show", "FuzzySets", function(object) {
    # Format the object
    x <- relations(object)
    x[["membership"]] <- membership(object)
    x[["elementData"]] <- elementData(object)[x$element, , drop=FALSE]
    x[["setData"]] <- setData(object)[x$set, , drop=FALSE]

    .showSetAsTable(class(object), x)
})

#' @param x An object that inherits from `FuzzySets`.
#'
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
as.matrix.FuzzySets <- function(x, ..., fill=0)  {
    xbind <- cbind(as.data.frame(relations(x)), value=membership(x))
    out <- acast(xbind, element ~ set, value.var="value", fill=fill, ...)
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
as.FuzzySets.matrix <- function(x, ...)  {
    as(x, "FuzzySets")
}

#' @importFrom methods new
setAs("BaseSets", "FuzzySets", function(from) {
    new("FuzzySets", from, membership=rep(1, nRelations(from)))
})

#' @aliases as.FuzzySets.BaseSets as.FuzzySets
#' @importFrom methods as
as.FuzzySets.BaseSets <- function(x, ...)  {
    as(x, "FuzzySets")
}
