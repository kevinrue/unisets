# Direct pointers ----

#' @rdname FuzzySets-methods
#' @aliases membership,FuzzySets-method
#'
#' @section Accessors:
#' `membership(object)` returns a `numeric` vector of membership function for each relation.
#'
#' @importFrom S4Vectors DataFrame
#'
#' @examples
#' # Constructor ----
#'
#' # Visually intuitive definition of sets, elements, and membership
#' sets <- list(
#'   set1=c("A"=0.1, "B"=0.2),
#'   set2=c("B"=0.3, "C"=0.4, "D"=0.5),
#'   set3=c("E"=0.8))
#'
#' # unlist the set names
#' unlistSets <- rep(names(sets), lengths(sets))
#' # unlist the element names
#' unlistElements <- unlist(sapply(sets, names))
#' # unlist the membership values
#' unlistMembership <- unlist(sets)
#'
#' # Reformat as a table
#' relations <- DataFrame(
#'   element=unlistElements,
#'   set=unlistSets,
#'   membership=unlistMembership
#' )
#'
#' fs <- FuzzySets(relations=relations)
#' fs
#'
#' # Accessors ----
#'
#' membership(fs)
setMethod("membership", "FuzzySets", function(object) {
    as.numeric(membership(object@relations))
})

#' @rdname FuzzySets-methods
#' @aliases membership<-,FuzzySets-method
#'
#' @importFrom methods validObject
#'
#' @examples
#' fs1 <- fs
#' membership(fs1)[1] <- 0
setReplaceMethod("membership", "FuzzySets",
    function(object, value)
    {
        membership(object@relations) <- value
        validObject(object)
        object
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
#'
#' @examples
#'
#' # Subsetting ----
#'
#' fs1 <- subset(fs, set == "set1" | membership > 0.5)
#' fs1
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
#' @examples
#'
#' # Coercion from/to FuzzySets ----
#'
#' matrix1 <- as(fs, "matrix")
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
#'
#' @examples
#'
#' # Coercion to FuzzySets ----
#'
#' fs1 <- as(matrix1, "FuzzySets")
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

# setValidity ----

#' @importFrom methods slot
setValidity("FuzzySets", function(object) {

    errors <- c()

    if (!all(c("membership") %in% colnames(relations(object)))){
        error <- 'colnames(relations(object)) must include c("membership")'
        return(error)
    }


    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})
