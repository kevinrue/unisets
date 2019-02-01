# Direct pointers ----

#' @param x An object that inherits from `FuzzySets`.
#'
#' @rdname FuzzySets-class
#' @aliases membership,FuzzySets-method
setMethod("membership", "FuzzySets", function(x) {
    as.numeric(membership(x@relations))
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
        table <- as(x, "data.frame")
        i <- eval(substitute(subset), table)

        keep.element <- unique(ids(elementData(x))[from(x@relations)[i]])
        keep.set <- unique(ids(setData(x))[to(x@relations)[i]])

        relations <- DataFrame(table[i, , drop=FALSE])
        elementData <- elementData(x)[which(ids(elementData(x)) %in% keep.element)]
        setData <- setData(x)[which(ids(setData(x)) %in% keep.set)]
        membership <- membership(x)[i]

        out <- FuzzySets(relations, elementData, setData)
        out
    }
    .local(x, ...)
})

# as.list() ----

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
as.matrix.FuzzySets <- function(x, ..., fill=NA_real_) {
    out <- as(x, "data.frame")
    out[["value"]] <- membership(x)
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
    relations <- melt(from, varnames=c("element", "set"), value.name = "membership", as.is=TRUE)
    relations <- DataFrame(relations)
    FuzzySets(relations)
})

#' @aliases as.FuzzySets.matrix as.FuzzySets
#' @importFrom methods as
as.FuzzySets.matrix <- function(x, ...) {
    as(x, "FuzzySets")
}

#' @importFrom methods new
setAs("BaseSets", "FuzzySets", function(from) {
    from@relations <- as(from@relations, "FuzzyHits")
    to <- new("FuzzySets", from)
    to
})

#' @aliases as.FuzzySets.BaseSets as.FuzzySets
#' @importFrom methods as
as.FuzzySets.BaseSets <- function(x, ...) {
    as(x, "FuzzySets")
}
