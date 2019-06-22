
# Accessors ----

#' @rdname Sets-methods
#' @aliases relations,Sets-method
#'
#' @section Accessors:
#' `relations(object)` returns the `relations` slot.
#'  A `Hits` objets storing the integer index of elements (`from`) and sets (`to`) in the `elementInfo` and `setInfo` slots, respectively, and associated relation metadata (`mcols`).
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
#' bs <- as(sets, "Sets")
#' bs
#'
#' # Accessors ----
#'
#' relations(bs)
setMethod("relations", "Sets", function(object) {
    slot(object, "relations")
})

#' @rdname Sets-methods
#' @aliases relations<-,Sets-method
#'
#' @importFrom methods validObject slot<-
#'
#' @examples
#'
#' bs1 <- bs
#' mcols(relations(bs1))[["NEW"]] <- paste0("value", seq_len(length(bs1)))
setReplaceMethod("relations", "Sets",
    function(object, value)
    {
        slot(object, "relations") <- value
        validObject(object)
        object
    }
)

#' @rdname Sets-methods
#' @aliases elementInfo,Sets-method
#'
#' @section Accessors:
#' `elementInfo(object)` returns the `elementInfo` slot.
#'  An [`IdVector-class`] objets storing the unique element identifiers (`ids`) and associated element metadata (`mcols`).
#'
#' @examples
#'
#' elementInfo(bs)
setMethod("elementInfo", "Sets", function(object) {
    slot(object, "elementInfo")
})

#' @rdname Sets-methods
#' @aliases elementInfo<-,Sets-method
#'
#' @importFrom methods validObject
#'
#' @examples
#'
#' bs1 <- bs
#' mcols(elementInfo(bs1))[["NEW"]] <- paste0("value", seq_len(nElements(bs1)))
setReplaceMethod("elementInfo", "Sets",
    function(object, value)
    {
        slot(object, "elementInfo") <- value
        validObject(object)
        object
    }
)

#' @rdname Sets-methods
#' @aliases setInfo,Sets-method
#'
#' @section Accessors:
#' `setInfo(object)` returns the `setInfo` slot.
#'  An [`IdVector-class`] objets storing the unique set identifiers (`ids`) and associated set metadata (`mcols`).
#'
#' @examples
#'
#' setInfo(bs)
setMethod("setInfo", "Sets", function(object) {
    slot(object, "setInfo")
})

#' @rdname Sets-methods
#' @aliases setInfo<-,Sets-method
#'
#' @importFrom methods validObject
#'
#' @examples
#'
#' bs1 <- bs
#' mcols(setInfo(bs1))[["NEW"]] <- paste0("value", seq_len(nSets(bs1)))
setReplaceMethod("setInfo", "Sets",
    function(object, value)
    {
        slot(object, "setInfo") <- value
        validObject(object)
        object
    }
)

#' @rdname Sets-methods
#' @aliases elements,Sets-method
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
setMethod("elements", "Sets", function(object) {
    elementInfo(object)[from(relations(object))]
})

#' @rdname Sets-methods
#' @aliases sets,Sets-method
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
setMethod("sets", "Sets", function(object) {
    setInfo(object)[to(relations(object))]
})

# Dimensions ----

#' @rdname Sets-methods
#' @aliases length,Sets-method
#'
#' @section Dimensions:
#' `length(x)` returns the total count of relations.
#'
#' @examples
#'
#' # Dimensions ----
#'
#' length(bs)
setMethod("length", "Sets", function(x) {
    length(relations(x))
})

#' @rdname Sets-methods
#' @aliases nElements,Sets-method
#'
#' @section Dimensions:
#' `nElements(object)` returns the count of unique elements.
#'
#' @examples
#' nElements(bs)
setMethod("nElements", "Sets", function(object) {
    length(elementInfo(object))
})

#' @rdname Sets-methods
#' @aliases nSets,Sets-method
#'
#' @section Dimensions:
#' `nSets(object)` returns the count of unique sets.
#'
#' @examples
#' nSets(bs)
setMethod("nSets", "Sets", function(object) {
    length(setInfo(object))
})

#' @rdname Sets-methods
#' @aliases setLengths,Sets-method
#'
#' @section Dimensions:
#' `setLengths(object)` returns the count of relations per set.
#'
#' @importFrom methods as
#'
#' @examples
#' setLengths(bs)
setMethod("setLengths", "Sets", function(object) {
    out <- as(object, "list")
    lengths(out)
})

#' @rdname Sets-methods
#' @aliases elementLengths,Sets-method
#'
#' @section Dimensions:
#' `elementLengths(object)` returns the count of relations per element.
#'
#' @importFrom methods as
#'
#' @examples
#' elementLengths(bs)
setMethod("elementLengths", "Sets", function(object) {
    # Note the difference between the argument and the method 'from'
    out <- as(object, "DataFrame")
    out <- split(out$set, out$element)
    lengths(out)
})

# c() ----

#' @rdname Sets-methods
#' @aliases c,Sets-method
#'
#' @section Combining:
#' `c(x, ...)` combines its arguments
#'
#' @examples
#'
#' # Combining ----
#'
#' bs1 <- c(bs, bs)
c.Sets <- function(x, ...) {
    c(x, ...)
}

setMethod(
    "c", "Sets",
    function(x, ...){
        .local <- function (x, objects=list(), use.names = TRUE,  ignore.mcols = FALSE, check = TRUE)
        {
            all_objects <- c(list(x), objects)

            newElementData <- lapply(all_objects, elementInfo)
            newSetData <- lapply(all_objects, setInfo)
            newRelations <- lapply(all_objects, as.data.frame)

            newElementData <- do.call(c, newElementData)
            newSetData <- do.call(c, newSetData)
            newRelations <- do.call(rbind, newRelations)

            newElementData <- unique(newElementData)
            newSetData <- unique(newSetData)

            Sets(newRelations, newElementData, newSetData)
        }
        .local(x, list(...))
    }
)

# [ ----

#' @rdname Sets-methods
#' @aliases [,Sets-method
#'
#' @section Subsetting:
#' `x[i, drop=TRUE]` returns new [`Sets-class`] object of the same class as `x` made of the elements selected by `i`. `i` can be missing; an `NA`-free logical, numeric, or character vector or factor (as ordinary vector or [`Rle`] object); or an [`IntegerRanges`][IntegerRanges-class] object.
#' The `drop` logical value controls whether the metadata of elements and sets orphaned during the subsetting should be removed from the `elementInfo` and `setInfo` slots, respectively.
#'
#' @param i index specifying elements to extract or replace.
#' @param j Ignored.
#' @param drop A logical scalar indicating whether to remove orphan elements and sets from the `elementInfo` and `setInfo` slots, respectively.
#'
#' @importFrom methods callNextMethod
#' @importClassesFrom IRanges IntegerRanges
#'
#' @examples
#'
#' # Subsetting ----
#'
#' bs1 <- bs[1:5]
#' bs1 <- bs[1:5, , drop=FALSE] # keep metadata of orphan elements and sets
setMethod("[", "Sets", function(x, i, j, ..., drop = TRUE) {
    keep.element <- unique(ids(elementInfo(x))[from(relations(x))[i]])
    keep.set <- unique(ids(setInfo(x))[to(relations(x))[i]])

    relations <- DataFrame(as.data.frame(x)[i, , drop=drop], row.names=NULL)
    elementInfo <- elementInfo(x)
    setInfo <- setInfo(x)
    if (isTRUE(drop)) {
        elementInfo <- elementInfo[which(ids(elementInfo) %in% keep.element)]
        setInfo <- setInfo[which(ids(setInfo) %in% keep.set)]
    }

    Sets(relations, elementInfo, setInfo)
})

# subset() ----

#' @rdname Sets-methods
#' @aliases subset.Sets subset,Sets-method
#'
#' @param ... Additional arguments passed to and from other methods.
#'
#' @section Subsetting:
#'
#' `subset(object, subset, ..., drop=TRUE)` returns subsets of relations which meet conditions.
#' The `subset` argument should be a logical expression referring to any of `"element"`, `"set"`, and any available relation metadata indicating elements or rows to keep: missing values are taken as false.
#' The `drop` logical scalar controls whether elements and sets orphaned during the subsetting should be removed from the `elementInfo` and `setInfo` slots, respectively.

#'
#' @importFrom methods as
#' @importFrom BiocGenerics eval unique
#' @importFrom S4Vectors from to subset
#' @method subset Sets
#' @export
#'
#' @examples
#'
#' bs1 <- subset(bs, set == "set1" | element == "E")
#' bs1
subset.Sets <- function(x, ...) subset(x, ...)

setMethod("subset", "Sets", function(x, ...) {
    .local <- function(x, subset, select, drop=TRUE, ...) {
        # Match code layout of the FuzzySets method
        table <- as.data.frame(x)
        i <- eval(substitute(subset), table)
        out <- x[i, drop=drop]
        # For derived subclasses, coerce back to the original
        as(out, class(x))
    }
    .local(x, ...)
})

# show() ----

setMethod("show", "Sets", function(object) {
    showSets(object, margin="  ", print.classinfo=TRUE, print.nnode=TRUE)
})

#' @importFrom S4Vectors mcols nLnode nRnode
showSets <- function(
    x, margin="", print.classinfo=FALSE, print.nnode=FALSE
) {
    nm <- length(slot(x, "relations")) # number of mappings
    ne <- nLnode(slot(x, "relations")) # number of unique elements
    ns <- nRnode(slot(x, "relations")) # number of unique sets
    ned <- ncol(mcols(slot(x, "elementInfo"))) # number of element metadata
    nsd <- ncol(mcols(slot(x, "setInfo"))) # number of set metadata
    # Display class name and basic summary
    cat(
        class(x), " with ",
        nm, ifelse(nm == 1, " relation", " relations"), " between ",
        ne, ifelse(ne == 1, " element", " elements"), " and ",
        ns, ifelse(ns == 1, " set\n", " sets\n"),
        sep = "")
    # Display compact view of the relations and metadata
    # TODO: ask S4Vectors to export makePrettyMatrixForCompactPrinting
    out <- S4Vectors:::makePrettyMatrixForCompactPrinting(
        x, .make_naked_matrix_from_Sets)
    # Prepare class information for each column
    if (print.classinfo) {
        .COL2CLASS <- c(element = "character", set = "character")
        # TODO: ask S4Vectors to export makeClassinfoRowForCompactPrinting
        classinfo <- S4Vectors:::makeClassinfoRowForCompactPrinting(relations(x), .COL2CLASS)
        ## A sanity check, but this should never happen!
        stopifnot(identical(colnames(classinfo), colnames(out)))
        out <- rbind(classinfo, out)
    }
    print(out, quote=FALSE, right=TRUE, max=length(out))
    # Display compact view of element metadata
    cat("-----------")
    cat(
        "\nelementInfo: ", class(slot(x, "elementInfo")), " with ",
        ned, " metadata",
        ifelse(
            ned > 0,
            sprintf(
                " (%s%s)",
                paste(head(colnames(mcols(slot(x, "elementInfo"))), 2), collapse = ", "),
                ifelse(ned > 2, ", ...", "")
                ),
            ""),
        sep = "")
    cat(
        "\n    setInfo: ", class(slot(x, "setInfo")), " with ",
        nsd, " metadata",
        ifelse(
            nsd > 0,
            sprintf(
                " (%s%s)",
                paste(head(colnames(mcols(slot(x, "setInfo"))), 2), collapse = ", "),
                ifelse(nsd > 2, ", ...", "")
                ),
            ""),
        "\n",
        sep = "")
    invisible(NULL)
}

# duplicated() ----

#' @rdname Sets-methods
#' @aliases duplicated,Sets-method
#'
#' @param incomparables Ignored.
#'
#' @section Duplication and uniqueness:
#' `duplicated(x)` determines which relations of a `Sets` are duplicates of relations with smaller subscripts, and returns a logical vector indicating which relations are duplicates.
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
setMethod("duplicated", "Sets", function(x, incomparables = FALSE, ...) {
    duplicated(as.data.frame(x))
})

# unique() ----

#' @rdname Sets-methods
#' @aliases unique,Sets-method
#'
#' @section Duplication and uniqueness:
#'
#' `unique(x)` returns a `Sets` like `x` but with duplicate relations removed.
#'
#' @export
#' @importMethodsFrom BiocGenerics unique
#'
#' @examples
#' unique(bs1)
setMethod("unique", "Sets", function(x, incomparables = FALSE, ...)  {
    i <- !duplicated(x, incomparables, ...)
    x[i, drop=FALSE] # by definition, unique would never drop elements nor sets
})

# union() ----

#' @rdname Sets-methods
#' @aliases union,Sets-method union.Sets
#' @param y An object of class inheriting from [`Sets`].
#'
#' @section Duplication and uniqueness:
#'
#' `union(x)` returns a `Sets` composed of the union of relations in `x` and `y`.
#'
#' @export
#' @importMethodsFrom BiocGenerics union
#'
#' @examples
#' bs1 <- union(bs, bs)
union.Sets <- function(x, y, ...) {
    union(x, y, ...)
}

setMethod("union", "Sets", function (x, y, ...) {
    .local <- function (x, y)
        unique(c(x, y))
    .local(x, y, ...)
})

# as.data.frame.Sets() ----

#' @rdname Sets-methods
#' @aliases as.DataFrame.Sets as.DataFrame
#'
#' @section Coercion from Sets:
#' `as(object, "DataFrame")` and `as.DataFrame(object)` return a nested `DataFrame` including columns `"element"`, `"set"`, `"relationData"`, `"elementInfo"`, and `"setInfo"`.
#'
#' @importFrom methods as
#' @export
#'
#' @examples
#'
#' # Coercion from Sets ----
#'
#' DF1 <- as(bs, "DataFrame")
as.DataFrame.Sets <- function(object, ...) {
    # Combine elementInfo, setInfo, and relations into a single DataFrame
    element <- elementInfo(object)[from(relations(object))]
    elementInfo <- mcols(element)
    mcols(element) <- NULL # avoid metadata columns
    set <- setInfo(object)[to(relations(object))]
    setInfo <- mcols(set)
    mcols(set) <- NULL # avoid metadata columns
    out <- DataFrame(element=element, set=set, row.names=NULL)
    out[["relationData"]] <- mcols(relations(object))
    out[["elementInfo"]] <- elementInfo
    out[["setInfo"]] <- setInfo
    out
}

setAs("Sets", "DataFrame", function(from) {
    as.DataFrame.Sets(from)
})

#' @rdname Sets-methods
#' @aliases as.data.frame.Sets as.data.frame
#'
#' @section Coercion from Sets:
#' `as(x, "data.frame")` and `as.data.frame(x)`  return a flattened `data.frame` including `"element"`, `"set"`, and columns in `mcols(relations(x))` if any.
#'
#' @importFrom methods as
#' @export
#'
#' @examples
#'
#' df1 <- as.data.frame(bs)
as.data.frame.Sets <- function(x, ...) {
    out <- as(x, "DataFrame")
    out <- data.frame(
        out[, c("element", "set")],
        as.data.frame(out$relationData)
    )
    out
}

setAs("Sets", "data.frame", function(from) {
    as.data.frame.Sets(from)
})

# as.list() ----

#' @rdname Sets-methods
#' @aliases as.list.Sets as.list
#'
#' @section Coercion from Sets:
#' `as(x, "list")` and `as.list(x)` return a named `list`.
#' Names are set identifiers, and values are character vectors of element identifiers.
#'
#' @importFrom methods as
#' @export
#'
#' @examples
#'
#' l1 <- as(bs, "list")
as.list.Sets <- function(x, ...) {
    out <- as(x, "DataFrame")
    split(out$element, out$set)
}

setAs("Sets", "list", function(from) {
    as.list.Sets(from)
})

# as.matrix() ----

#' @rdname Sets-methods
#' @aliases as.matrix.Sets as.matrix
#'
#' @section Coercion from Sets:
#' `as(x, "matrix")` and `as.matrix(x)` return a `matrix` with elements as rows, sets as columns, and a `logical` value to indicate membership.
#'
#' @importFrom methods as
#' @export
#'
#' @examples
#'
#' m1 <- as(bs, "matrix")
as.matrix.Sets <- function(x, ...) {
    out <- as.data.frame(x)
    out[["value"]] <- TRUE
    out <- acast(out, element~set, value.var="value", fun.aggregate=any, fill=FALSE)
    out
}

#' @importFrom reshape2 acast
setAs("Sets", "matrix", function(from) {
    as.matrix.Sets(from)
})

# as.FuzzySets.Sets() ----

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases as.FuzzySets.Sets
#'
#' @importFrom methods new
#'
#' @examples
#'
#' bs1 <- bs
#' mcols(relations(bs1))[["membership"]] <- runif(length(bs1))
#' fs <- as(bs1, "FuzzySets")
setAs("Sets", "FuzzySets", function(from) {
    relations(from) <- as(relations(from), "FuzzyHits")
    to <- new("FuzzySets", from)
    to
})

# as.GOSets.Sets() ----

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases as.GOSets.Sets
#'
#' @importFrom methods new
#'
#' @examples
#'
#' # Fetch a sample of GO annotations
#' library(org.Hs.eg.db)
#' gs <- import(org.Hs.egGO)
#' bs1 <- as(gs, "Sets")
#' gs1 <- as(bs1, "GOSets")
setAs("Sets", "GOSets", function(from) {
    relations(from) <- as(relations(from), "GOHits")
    to <- new("GOSets", from)
    to
})

# as.Sets.list() ----

#' @rdname Sets-methods
#' @aliases as.Sets.list as.Sets
#'
#' @param list A `list` of named character vectors.
#' The names are taken as the set identifiers.
#' The character vectors are taken as identifiers of elements that are member of each set.
#'
#' @section Coercion to Sets:
#' `as(list, "Sets")` and `as.Sets(object)` return a `Sets` from a list of character vectors.
#'
#' @importFrom methods as
#' @importFrom S4Vectors DataFrame
#' @export
#'
#' @examples
#'
#' # Coercion to Sets ----
#'
#' # list
#' bs1 <- as(list(set1=c("A", "B"), set2=c("B", "C")), "Sets")
as.Sets.list <- function(list, ...) {
    stopifnot(!is.null(names(list)))
    relations <- DataFrame(
        element=unlist(list, use.names=FALSE),
        set=rep(names(list), lengths(list))
    )
    Sets(relations)
}

setAs("list", "Sets", function(from) {
    as.Sets.list(from)
})

# as.Sets.matrix() ----

#' @rdname Sets-methods
#' @aliases as.Sets.matrix as.Sets
#'
#' @param matrix A `matrix`.
#' The matrix will be coerced to `logical` type and relations indicating `TRUE` will be stored in the `Sets`.
#'
#' @section Coercion to Sets:
#' `as(matrix, "Sets")` and `as.Sets(object)` return a `Sets` from an incidence matrix.
#'
#' @importFrom methods as
#' @export
#'
#' @examples
#' # matrix
#' bs1 <- as(m1, "Sets")
as.Sets.matrix <- function(matrix, ...) {
    storage.mode(matrix) <- "logical"
    out <- melt(matrix, varnames=c("element", "set"), as.is=TRUE)
    out <- out[which(out$value), c("element", "set"), drop=FALSE]
    out <- DataFrame(out)
    Sets(out)
}

#' @importFrom reshape2 melt
#' @importFrom S4Vectors DataFrame
setAs("matrix", "Sets", function(from) {
    as.Sets.matrix(from)
})

# setValidity ----

#' @importFrom methods slot
setValidity("Sets", function(object) {

    errors <- c()

    elementInfo <- elementInfo(object)
    setInfo <- setInfo(object)

    if (any(duplicated(ids(elementInfo)))) {
        error <- 'duplicated values in ids(elementInfo(object))'
        errors <- c(errors, error)
    }

    if (any(duplicated(ids(setInfo)))) {
        error <- 'duplicated values in ids(setInfo(object))'
        errors <- c(errors, error)
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})
