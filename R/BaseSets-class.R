
# Accessors ----

#' @rdname BaseSets-methods
#' @aliases relations,BaseSets-method
#'
#' @section Accessors:
#' `relations(object)` returns the `relations` slot.
#'  A `Hits` objets storing the integer index of elements (`from`) and sets (`to`) in the `elementData` and `setData` slots, respectively, and associated relation metadata (`mcols`).
#'
#' @importFrom S4Vectors DataFrame
#'
#' @examples
#' # Constructor ----
#'
#' # Visually intuitive definition of sets
#' sets <- list(
#'   set1=c("A", "B"),
#'   set2=c("B", "C", "D"),
#'   set3=c("E"))
#'
#' bs <- as(sets, "BaseSets")
#' bs
#'
#' # Accessors ----
#'
#' relations(bs)
setMethod("relations", "BaseSets", function(object) {
    slot(object, "relations")
})

#' @rdname BaseSets-methods
#' @aliases relations<-,BaseSets-method
#'
#' @importFrom methods validObject slot<-
#'
#' @examples
#'
#' bs1 <- bs
#' mcols(relations(bs1))[["NEW"]] <- paste0("value", seq_len(length(bs1)))
setReplaceMethod("relations", "BaseSets",
    function(object, value)
    {
        slot(object, "relations") <- value
        validObject(object)
        object
    }
)

#' @rdname BaseSets-methods
#' @aliases elementData,BaseSets-method
#'
#' @section Accessors:
#' `elementData(object)` returns the `elementData` slot.
#'  An [IdVector-class] objets storing the unique element identifiers (`ids`) and associated element metadata (`mcols`).
#'
#' @examples
#'
#' elementData(bs)
setMethod("elementData", "BaseSets", function(object) {
    slot(object, "elementData")
})

#' @rdname BaseSets-methods
#' @aliases elementData<-,BaseSets-method
#'
#' @importFrom methods validObject
#'
#' @examples
#'
#' bs1 <- bs
#' mcols(elementData(bs1))[["NEW"]] <- paste0("value", seq_len(nElements(bs1)))
setReplaceMethod("elementData", "BaseSets",
    function(object, value)
    {
        slot(object, "elementData") <- value
        validObject(object)
        object
    }
)

#' @rdname BaseSets-methods
#' @aliases setData,BaseSets-method
#'
#' @section Accessors:
#' `setDatav(object)` returns the `setData` slot.
#'  An [IdVector-class] objets storing the unique set identifiers (`ids`) and associated set metadata (`mcols`).
#'
#' @examples
#'
#' setData(bs)
setMethod("setData", "BaseSets", function(object) {
    slot(object, "setData")
})

#' @rdname BaseSets-methods
#' @aliases setData<-,BaseSets-method
#'
#' @importFrom methods validObject
#'
#' @examples
#'
#' bs1 <- bs
#' mcols(setData(bs1))[["NEW"]] <- paste0("value", seq_len(nSets(bs1)))
setReplaceMethod("setData", "BaseSets",
    function(object, value)
    {
        slot(object, "setData") <- value
        validObject(object)
        object
    }
)

#' @rdname BaseSets-methods
#' @aliases elements,BaseSets-method
#'
#' @section Accessors:
#' `elements(object)` returns an [`IdVector-class`] element identifiers and associated metadata as ordered in `relations(object)$element`
#' (i.e., of length equal to `length(object)`).
#'
#' @importFrom S4Vectors from
#'
#' @examples
#'
#' elements(bs)
#' ids(elements(bs))
#' mcols(elements(bs))
setMethod("elements", "BaseSets", function(object) {
    elementData(object)[from(relations(object))]
})

#' @rdname BaseSets-methods
#' @aliases sets,BaseSets-method
#'
#' @section Accessors:
#' `sets(object)` returns an [`IdVector-class`] of set identifiers and associated metadata as ordered in `relations(object)$set`.
#' (i.e., of length equal to `length(object)`).
#'
#' @importFrom S4Vectors to
#'
#' @examples
#'
#' sets(bs)
#' ids(sets(bs))
#' mcols(sets(bs))
setMethod("sets", "BaseSets", function(object) {
    setData(object)[to(relations(object))]
})

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
    length(relations(x))
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

# [ ----

#' @rdname BaseSets-methods
#' @aliases [,BaseSets-method
#'
#' @section Subsetting:
#' `x[i]` returns new [`BaseSets-class`] object of the same class as `x` made of the elements selected by `i`. `i` can be missing; an NA-free logical, numeric, or character vector or factor (as ordinary vector or [`Rle`] object); or an [`IntegerRanges`][IntegerRanges-class] object.
#'
#' @param i index specifying elements to extract or replace.
#' @param j,drop Ignored.
#'
#' @importFrom methods callNextMethod
#' @importClassesFrom IRanges IntegerRanges
#'
#' @examples
#'
#' # Subsetting ----
#'
#' bs1 <- bs[1:5]
setMethod("[", "BaseSets", function(x, i, j, ..., drop = TRUE) {
    keep.element <- unique(ids(elementData(x))[from(relations(x))[i]])
    keep.set <- unique(ids(setData(x))[to(relations(x))[i]])

    relations <- DataFrame(as(x, "data.frame")[i, , drop=FALSE], row.names=NULL)
    elementData <- elementData(x)[which(ids(elementData(x)) %in% keep.element)]
    setData <- setData(x)[which(ids(setData(x)) %in% keep.set)]

    BaseSets(relations, elementData, setData)
})

# subset() ----

#' @rdname BaseSets-methods
#' @aliases subset.BaseSets subset,BaseSets-method
#'
#' @param ... Additional arguments passed to and from other methods.
#'
#' @section Subsetting:
#'
#' `subset(object, subset, ...)` returns subsets of relations which meet conditions.
#' The `subset` argument should be a logical expression referring to any of `"element"`, `"set"`, and any available relation metadata indicating elements or rows to keep: missing values are taken as false.
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
#' bs1 <- subset(bs, set == "set1" | element == "E")
#' bs1
subset.BaseSets <- function(x, ...) subset(x, ...)

setMethod("subset", "BaseSets", function(x, ...) {
    .local <- function(x, subset, select, drop=FALSE, ...) {
        # Match code layout of the FuzzySets method
        table <- as(x, "data.frame")
        i <- eval(substitute(subset), table)
        x[i]
    }
    .local(x, ...)
})

# show() ----

#' @importFrom S4Vectors mcols
setMethod("show", "BaseSets", function(object) {
    .showSetAsTable(class(object), as(object, "DataFrame"))
})

# duplicated() ----

#' @rdname BaseSets-methods
#' @aliases duplicated,BaseSets-method
#'
#' @param incomparables Ignored.
#'
#' @section Duplication and uniqueness:
#' `duplicated(x)` determines which relations of a `BaseSets` are duplicates of relations with smaller subscripts, and returns a logical vector indicating which relations are duplicates.
#'
#' @export
#' @importMethodsFrom BiocGenerics duplicated
#'
#' @examples
#'
#' # Duplication and uniqueness ----
#'
#' bs1 <- bs
#' relations(bs1) <- rep(relations(bs1), each=2)
#' table(duplicated(bs1))
setMethod("duplicated", "BaseSets", function(x, incomparables = FALSE, ...) {
    duplicated(relations(x))
})

# unique() ----

#' @rdname BaseSets-methods
#' @aliases unique,BaseSets-method
#'
#' @section Duplication and uniqueness:
#'
#' `unique(x)` returns a `BaseSets` like `x` but with duplicate relations removed.
#'
#' @export
#' @importMethodsFrom BiocGenerics unique
#'
#' @examples
#' unique(bs1)
setMethod("unique", "BaseSets", function(x, incomparables = FALSE, ...)  {
    i <- !duplicated(x, incomparables = incomparables, ...)
    x[i]
})

# as.data.frame.BaseSets() ----

#' @rdname BaseSets-methods
#' @aliases as.DataFrame.BaseSets as.DataFrame
#'
#' @section Coercion from BaseSets:
#' `as(object, "DataFrame")` and `as.DataFrame(object)` return a nested `DataFrame` including columns `"element"`, `"set"`, `"relationData"`, `"elementData"`, and `"setData"`.
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
    # Combine elementData, setData, and relations into a single DataFrame
    element <- elementData(object)[from(relations(object))]
    elementData <- mcols(element)
    mcols(element) <- NULL # avoid metadata columns
    set <- setData(object)[to(relations(object))]
    setData <- mcols(set)
    mcols(set) <- NULL # avoid metadata columns
    out <- DataFrame(element=element, set=set, row.names=NULL)
    out[["relationData"]] <- mcols(relations(object))
    out[["elementData"]] <- elementData
    out[["setData"]] <- setData
    out
}

setAs("BaseSets", "DataFrame", function(from) {
    as.DataFrame.BaseSets(from)
})

#' @rdname BaseSets-methods
#' @aliases as.data.frame.BaseSets as.data.frame
#'
#' @section Coercion from BaseSets:
#' `as(x, "data.frame")` and `as.data.frame(x)`  return a flattened `data.frame` including `"element"`, `"set"`, and columns in `mcols(relations(x))` if any.
#'
#' @importFrom methods as
#' @export
#'
#' @examples
#'
#' df1 <- as(bs, "data.frame")
as.data.frame.BaseSets <- function(x, ...) {
    out <- as(x, "DataFrame")
    out <- data.frame(
        out[, c("element", "set")],
        as.data.frame(out$relationData)
    )
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
#' @section Coercion from BaseSets:
#' `as(x, "list")` and `as.list(x)` return a named `list`.
#' Names are set identifiers, and values are character vectors of element identifiers.
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
#' @aliases as.FuzzySets.BaseSets
#'
#' @importFrom methods new
#'
#' @examples
#'
#' bs1 <- bs
#' mcols(relations(bs1))[["membership"]] <- runif(length(bs1))
#' fs <- as(bs1, "FuzzySets")
setAs("BaseSets", "FuzzySets", function(from) {
    relations(from) <- as(relations(from), "FuzzyHits")
    to <- new("FuzzySets", from)
    to
})

# as.GOSets.BaseSets() ----

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases as.GOSets.BaseSets
#'
#' @importFrom methods new
#'
#' @examples
#'
#' # Fetch a sample of GO annotations
#' library(org.Hs.eg.db)
#' gs <- import(org.Hs.egGO)
#' bs1 <- as(gs, "BaseSets")
#' colnames(mcols(relations(bs1))) <- c("evidence", "ontology")
#' gs1 <- as(bs1, "GOSets")
setAs("BaseSets", "GOSets", function(from) {
    relations(from) <- as(relations(from), "GOHits")
    to <- new("GOSets", from)
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
