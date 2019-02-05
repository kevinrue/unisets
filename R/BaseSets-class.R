
# Accessors ----

#' @rdname BaseSets-methods
#' @aliases relations,BaseSets-method
#'
#' @section Accessors:
#' `relations(object)` returns a `DataFrame` of relations including `"element"`, `"set"`, and any relation-wise metadata available.
#'
#' @importFrom S4Vectors DataFrame
#'
#' @examples
#'
#' # Accessors ----
#'
#' relations(bs)
setMethod("relations", "BaseSets", function(object) {
    # use the built-in conversion of Hits to DataFrame
    out <- as(slot(object, "relations"), "DataFrame")
    colnames(out)[1:2] <- c("element", "set")
    # Substitute from/to by the corresponding identifiers
    out$element <- elementData(object)[out$element]
    out$set <- setData(object)[out$set]
    out
})

#' @rdname BaseSets-methods
#' @aliases elementData,BaseSets-method
#'
#' @section Accessors:
#' `elementData(object)` returns the metadata associated with each element as an object inheriting from [`IdVector`][IdVector-class].
#'
#' @examples
#' elementData(bs)
setMethod("elementData", "BaseSets", function(object) {
    slot(object, "elementData")
})

#' @rdname BaseSets-methods
#' @aliases setData,BaseSets-method
#'
#' @section Accessors:
#' `setData(object)` returns the metadata associated with each set as an object inheriting from [`IdVector`][IdVector-class].
#'
#' @examples
#' setData(bs)
setMethod("setData", "BaseSets", function(object) {
    slot(object, "setData")
})

#' @rdname BaseSets-methods
#' @aliases elements,BaseSets-method
#'
#' @section Accessors:
#' `elements(object)` returns an `IdVector` of sets and associated metadata as ordered in `relations(object)$element`.
#'
#' @importFrom S4Vectors from
#'
#' @examples
#' elements(bs)
setMethod("elements", "BaseSets", function(object) {
    elementData(object)[from(object@relations)]
})

#' @rdname BaseSets-methods
#' @aliases sets,BaseSets-method
#'
#' @section Accessors:
#' `sets(object)` returns an `IdVector` of sets and associated metadata as ordered in `relations(object)$set`.
#'
#' @importFrom S4Vectors to
#'
#' @examples
#' sets(bs)
setMethod("sets", "BaseSets", function(object) {
    setData(object)[to(object@relations)]
})

#' @rdname BaseSets-methods
#' @aliases elementIds,BaseSets-method
#'
#' @section Accessors:
#' `elementIds(object)` and `elementIds(object) <- value` get and set the `character` vector of element identifiers.
#'
#' @examples
#'
#' # Getter/Setters ----
#'
#' elementIds(bs)
setMethod("elementIds", "BaseSets", function(object) {
    ids(elementData(object))
})

#' @rdname BaseSets-methods
#' @aliases elementIds<-,BaseSets-method
#'
#' @importFrom methods validObject
#'
#' @examples
#' bs1 <- bs
#' elementIds(bs1) <- paste0("gene", seq_len(nElements(bs)))
setReplaceMethod("elementIds", "BaseSets",
    function(object, value)
    {
        object@elementData@ids <- value
        validObject(object)
        object
    }
)

#' @rdname BaseSets-methods
#' @aliases setIds,BaseSets-method
#'
#' @section Accessors:
#' `setIds(object)` and `setIds(object) <- value` get and set the `character` vector of element identifiers.
#'
#' @examples
#'
#' setIds(bs1)
setMethod("setIds", "BaseSets", function(object) {
    ids(setData(object))
})

#' @rdname BaseSets-methods
#' @aliases setIds<-,BaseSets-method
#'
#' @importFrom methods validObject
#'
#' @examples
#' bs1 <- bs
#' setIds(bs1) <- paste0("geneset", seq_len(nSets(bs)))
setReplaceMethod("setIds", "BaseSets",
    function(object, value)
    {
        object@setData@ids <- value
        validObject(object)
        object
    }
)

# Dimensions ----

#' @rdname BaseSets-methods
#' @aliases length,BaseSets-method
#'
#' @section Dimensions:
#' `length(x)` returns the total count of relations.
#'
#' @examples
#'
#' # Dimensions ----
#'
#' length(bs)
setMethod("length", "BaseSets", function(x) {
    length(x@relations)
})

#' @rdname BaseSets-methods
#' @aliases nElements,BaseSets-method
#'
#' @section Dimensions:
#' `nElements(object)` returns the count of unique elements.
#'
#' @examples
#' nElements(bs)
setMethod("nElements", "BaseSets", function(object) {
    length(elementData(object))
})

#' @rdname BaseSets-methods
#' @aliases nSets,BaseSets-method
#'
#' @section Dimensions:
#' `nSets(object)` returns the count of unique sets.
#'
#' @examples
#' nSets(bs)
setMethod("nSets", "BaseSets", function(object) {
    length(setData(object))
})

#' @rdname BaseSets-methods
#' @aliases setLengths,BaseSets-method
#'
#' @section Dimensions:
#' `setLengths(object)` returns the count of relations per set.
#'
#' @importFrom methods as
#'
#' @examples
#' setLengths(bs)
setMethod("setLengths", "BaseSets", function(object) {
    out <- as(object, "list")
    lengths(out)
})

#' @rdname BaseSets-methods
#' @aliases elementLengths,BaseSets-method
#'
#' @section Dimensions:
#' `elementLengths(object)` returns the count of relations per element.
#'
#' @importFrom methods as
#'
#' @examples
#' elementLengths(bs)
setMethod("elementLengths", "BaseSets", function(object) {
    # Note the difference between the argument and the method 'from'
    out <- as(object, "DataFrame")
    out <- split(out$set, out$element)
    lengths(out)
})

# subset() ----

#' @rdname BaseSets-methods
#' @aliases subset.BaseSets subset,BaseSets-method
#'
#' @param ... Additional arguments passed to and from other methods.
#'
#' @section Subsetting:
#' `subset(object, subset, ...)` returns subsets of relations which meet conditions.
#' For `FuzzySets` objects, the `subset` argument should be a logical expression referring to any of `"element"`, `"set"`, and any available relation metadata indicating elements or rows to keep: missing values are taken as false.
#' In addition, metadata for elements and sets that are not represented in the remaining relations are also dropped.
#'
#' @importFrom methods as
#' @importFrom BiocGenerics eval unique
#' @importFrom S4Vectors from to subset
#' @method subset BaseSets
#' @export
#'
#' @examples
#'
#' # Subsetting ----
#'
#' bs1 <- subset(bs, set == "set1" | element == "E")
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
#' `as(object, "DataFrame")` and `as.DataFrame(object)` return a `DataFrame` including `"element"`, `"set"`, and any relation-wise metadata available.
#'
#' @importFrom methods as
#' @export
#'
#' @examples
#'
#' # Coercion from BaseSets ----
#'
#' DF1 <- as(bs, "DataFrame")
as.DataFrame.BaseSets <- function(object, ...) {
    relations(object)
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
#'
#' @examples
#'
#' df1 <- as(bs, "data.frame")
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
#'
#' @examples
#'
#' l1 <- as(bs, "list")
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
#' @section Coercion from BaseSets:
#' `as(x, "matrix")` and `as.matrix(x)` return a `matrix` with elements as rows, sets as columns, and a `logical` value to indicate membership.
#'
#' @importFrom methods as
#' @export
#'
#' @examples
#'
#' m1 <- as(bs, "matrix")
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

# as.FuzzySets.BaseSets() ----

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases as.BaseSets.matrix as.BaseSets
#'
#' @importFrom methods new
#'
#' @examples
#'
#' fs <- as(bs, "FuzzySets")
setAs("BaseSets", "FuzzySets", function(from) {
    from@relations <- as(from@relations, "FuzzyHits")
    to <- new("FuzzySets", from)
    to
})

# as.BaseSets.list() ----

#' @rdname BaseSets-methods
#' @aliases as.BaseSets.list as.BaseSets
#'
#' @param list A `list` of named character vectors.
#' The names are taken as the set identifiers.
#' The character vectors are taken as identifiers of elements that are member of each set.
#'
#' @section Coercion to BaseSets:
#' `as(list, "BaseSets")` and `as.BaseSets(object)` return a `BaseSets` from a list of character vectors.
#'
#' @importFrom methods as
#' @importFrom S4Vectors DataFrame
#' @export
#'
#' @examples
#'
#' # Coercion to BaseSets ----
#'
#' # list
#' bs1 <- as(list(set1=c("A", "B"), set2=c("B", "C")), "BaseSets")
as.BaseSets.list <- function(list, ...) {
    stopifnot(!is.null(names(list)))
    relations <- DataFrame(
        element=unlist(list, use.names=FALSE),
        set=rep(names(list), lengths(list))
    )
    BaseSets(relations)
}

setAs("list", "BaseSets", function(from) {
    as.BaseSets.list(from)
})

# as.BaseSets.matrix() ----

#' @rdname BaseSets-methods
#' @aliases as.BaseSets.matrix as.BaseSets
#'
#' @param matrix A `matrix`.
#' The matrix will be coerced to `logical` type and relations indicating `TRUE` will be stored in the `BaseSets`.
#'
#' @section Coercion to BaseSets:
#' `as(matrix, "BaseSets")` and `as.BaseSets(object)` return a `BaseSets` from an incidence matrix.
#'
#' @importFrom methods as
#' @export
#'
#' @examples
#' # matrix
#' bs1 <- as(m1, "BaseSets")
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

# setValidity ----

#' @importFrom methods slot
setValidity("BaseSets", function(object) {

    errors <- c()

    elementData <- elementData(object)
    setData <- setData(object)

    if (any(duplicated(ids(elementData)))) {
        error <- 'duplicated values in ids(elementData(object))'
        errors <- c(errors, error)
    }

    if (any(duplicated(ids(setData)))) {
        error <- 'duplicated values in ids(setData(object))'
        errors <- c(errors, error)
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})
