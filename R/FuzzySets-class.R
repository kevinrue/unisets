# Direct pointers ----

#' @rdname FuzzySets-methods
#' @aliases membership,FuzzySets-method
#'
#' @section Accessors:
#' `membership(x)` returns a `numeric` vector of membership function for each relation.
#'
#' @importFrom S4Vectors DataFrame
setMethod("membership", "FuzzySets", function(x) {
    as.numeric(membership(x@relations))
})

#' @rdname FuzzySets-methods
#' @aliases membership<-,FuzzySets-method
#'
#' @importFrom methods validObject
setReplaceMethod("membership", "FuzzySets",
    function(x, value)
    {
        membership(x@relations) <- value
        validObject(x)
        x
    }
)

# subset ----

#' @rdname FuzzySets-methods
#' @aliases subset.FuzzySets subset,FuzzySets-method
#'
#' @param ... Additional arguments passed to and from other methods.
#'
#' @section Subsetting:
#' `subset(x, subset, ...)` returns subsets of relations which meet conditions.
#' For `FuzzySets` objects, the `subset` argument should be a logical expression referring to any of `"element"`, `"set"`, `"membership"`, and any available relation metadata indicating elements or rows to keep: missing values are taken as false.
#' In addition, metadata for elements and sets that are not represented in the remaining relations are also dropped.
#'
#' @importFrom methods as
#' @importFrom BiocGenerics eval unique
#' @importFrom S4Vectors from to subset
#' @method subset FuzzySets
#' @export
subset.FuzzySets <- function(x, ...) subset(x, ...)

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

# as.matrix.FuzzySets() ----

#' @rdname FuzzySets-methods
#' @aliases as.matrix.FuzzySets
#'
#' @param fill Value with which to fill in structural missings, passed to [`acast()`].
#' Defaults to `NA_real_`, to contrast with relations explictly associated with a membership function of 0.
#' @section Coercion:
#' `as(x, "matrix")` and `as.matrix(x)` return a `matrix` with elements as rows, sets as columns, and a numeric value indicating the membership function.
#'
#' @section Coercion to matrix:
#' As it is possible to store multiple relations between the same gene and gene set, it may be necessary to collapse multiple observations of the membership function into a single value.
#' To this end, the form `as.matrix(x, fun.aggregate)` can be used to provide an aggregation function.
#' See examples.
#'
#' @importFrom methods as
#' @importFrom reshape2 acast
#' @export
#'
as.matrix.FuzzySets <- function(x, fill=NA_real_, ...) {
    out <- as(x, "data.frame")
    out[["value"]] <- membership(x)
    out <- acast(out, element~set, value.var="value", fill=fill, ...)
    out
}

#' @importFrom reshape2 acast
setAs("FuzzySets", "matrix", function(from) {
    as.matrix.FuzzySets(from)
})

# as.FuzzySets.matrix() ----

#' @rdname FuzzySets-methods
#' @aliases as.FuzzySets.matrix as.FuzzySets
#'
#' @param matrix A `matrix`.
#' The matrix will be coerced to `double` type and the value will be taken to indicate the membership function.
#'
#' @importFrom methods as
as.FuzzySets.matrix <- function(matrix, ...) {
    storage.mode(matrix) <- "double"
    relations <- melt(matrix, varnames=c("element", "set"), value.name="membership", as.is=TRUE)
    relations <- DataFrame(relations)
    FuzzySets(relations)
}

#' @importFrom reshape2 melt
#' @importFrom S4Vectors DataFrame
setAs("matrix", "FuzzySets", function(from) {
    as.FuzzySets.matrix(from)
})

# as.FuzzySets.BaseSets() ----

#' @importFrom methods new
setAs("BaseSets", "FuzzySets", function(from) {
    from@relations <- as(from@relations, "FuzzyHits")
    to <- new("FuzzySets", from)
    to
})
