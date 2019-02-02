# ids() ----

#' @rdname IdVector-methods
#' @aliases ids,IdVector-method
#'
#' @section Accessors:
#' `ids(x)` returns a `character` vector of element identifiers.
#' `names(x)` is a synonym for compatibility with S4 methods such as `mcols(x, use.names = TRUE, ...) `.
#'
#' @importFrom methods slot
setMethod("ids", "IdVector", function(x) {
    slot(x, "ids")
})

# ids<-() ----

#' @rdname IdVector-methods
#' @aliases ids<-,IdVector-method
setReplaceMethod("ids", "IdVector",
    function(x, value)
    {
        slot(x, "ids") <- value
        x
    }
)

# names() ----

#' @rdname IdVector-methods
#' @aliases names,IdVector-method
setMethod("names", "IdVector", function(x) {
    ids(x)
})

# names<-() ----

#' @rdname IdVector-methods
#' @aliases names<-,IdVector-method
#'
#' @importFrom methods slot<-
setReplaceMethod("names", "IdVector",
    function(x, value)
    {
        ids(x) <- value
        x
    }
)

# length() ----

#' @rdname IdVector-methods
#' @aliases length,IdVector-method
#'
#' @section Dimensions:
#' `length(x)` returns the number of elements in `x`.
#'
#' @importFrom methods slot
setMethod("length", "IdVector", function(x) {
    length(slot(x, "ids"))
})

# [ ----

#' @rdname IdVector-methods
#' @aliases [,IdVector-method
#'
#' @section Subsetting:
#' `x[i]` returns new [`IdVector`][IdVector-class] object of the same class as `x` made of the elements selected by `i`. `i` can be missing; an NA-free logical, numeric, or character vector or factor (as ordinary vector or [`Rle`] object); or an [`IntegerRanges`][IntegerRanges-class] object.
#'
#' @param i index specifying elements to extract or replace.
#' @param j,...,drop Ignored.
#'
#' @importFrom methods callNextMethod
#' @importClassesFrom IRanges IntegerRanges
setMethod("[", "IdVector", function(x, i, j, ..., drop = TRUE) {
    callNextMethod()
})

# show() ----

#' @importFrom S4Vectors mcols
setMethod("show", "IdVector", function(object) {
    ne <- length(object)
    nu <- length(unique(slot(object, "ids")))
    cat(
        class(object), " of length ", ne, " with ",
        nu, " unique ", ifelse(ne == 1, "identifier", "identifiers"), "\n",
        sep = ""
    )
    # Preview of identifiers if any
    if (length(object) > 0) {
        ids <- paste(head(ids(object), 4), collapse=", ")
        if (ne > 4) {
            ids <- paste0(ids, ", ...")
        }
        ids <- paste0("Ids: ", ids)
        cat(ids, "\n", sep = "")
    }
    # Preview of metadata if any
    if (!is.null(mcols(object))) {
        nem <- ncol(mcols(object))
        metadata <- paste(head(colnames(mcols(object)), 4), collapse=", ")
        if (nem > 4) {
            metadata <- paste0(metadata, ", ...")
        }
        metadata <- paste0(
            "Metadata: ", metadata, " (", nem,
            ifelse(nem == 1, " column", " columns"), ")")
        cat(metadata, "\n", sep = "")
    }
    invisible(ids(object))
})

# showAsCell() ----

#' @importFrom methods slot
setMethod("showAsCell", "IdVector", function(object) {
    slot(object, "ids")
})

# NSBS ----

setMethod("NSBS", "IdVector", function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
{
    i <- match(ids(i), rownames(x))
    i
})

# split() ----

setMethod("split", c("IdVector", "IdVector"), function(x, f, drop = FALSE, ...)  {
    split(x, as.vector(f), drop=drop, ...)
})

# pcompare() ----

setMethod("pcompare", c("IdVector", "IdVector"), function(x, y)
{
    ids(x) == ids(y)
})

setMethod("pcompare", c("ANY", "IdVector"), function(x, y)
{
    x == ids(y)
})

setMethod("pcompare", c("IdVector", "ANY"), function(x, y)
{
    ids(x) == y
})

# as.vector.IdVector() ----

#' @rdname IdVector-methods
#' @aliases as.vector.IdVector as.vector
#'
#' @param mode Ignored. The vector will be coerced to `character` mode, unless requested otherwise.
#'
#' @section Coercion:
#' `as(x, "vector")` and `as.vector(x)` return an atomic vector of identifiers contained in `x`.
#'
#' @importFrom methods as
#' @export
as.vector.IdVector <- function(x, mode="character") {
    as.vector(ids(x), mode)
}

setAs("IdVector", "vector", function(from) {
    as.vector.IdVector(from)
})

# as.character() ----

#' @rdname IdVector-methods
#' @aliases as.character.IdVector as.character
#'
#' @section Coercion:
#' `as(x, "vector")` and `as.vector(x)` return a `character` vector of identifiers contained in `x`.
#'
#' @importFrom methods as
#' @export
as.character.IdVector <- function(x, ...) {
    as.vector.IdVector(x, "character")
}

setAs("IdVector", "character", function(from) {
    as.character.IdVector(from)
})

# as.IdVector.default() ----

#' @rdname IdVector-methods
#' @aliases as.IdVector.default as.IdVector
#'
#' @param ids An atomic vector of identifiers.
#'
#' @section Coercion:
#' `as(x, "IdVector")` and `as.IdVector(x)` return an `IdVector` from the given atomic vector of identifiers.
#'
#' @importFrom methods as
#' @export
as.IdVector.default <- function(ids, ...) {
    IdVector(as.character(ids, ...))
}

setAs("ANY", "IdVector", function(from) {
    as.IdVector.default(as.character(from))
})

