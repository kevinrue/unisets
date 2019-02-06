# ids() ----

#' @rdname IdVector-methods
#' @aliases ids,IdVector-method
#'
#' @section Accessors:
#' `ids(object)` returns a `character` vector of element identifiers.
#' `names(object)` is a synonym for compatibility with S4 methods such as `mcols(object, use.names = TRUE, ...) `.
#'
#' @importFrom methods slot
#' @importMethodsFrom GSEABase ids
#' @export
#'
#' @examples
#' # Constructor ----
#'
#' iv <- IdVector(ids=head(LETTERS, 6))
#' mcols(iv) <- DataFrame(row.names = ids(iv), field1=runif(length(iv)))
#' iv
#'
#' # Accessors ----
#'
#' ids(iv)
setMethod("ids", "IdVector", function(object) {
    slot(object, "ids")
})

# ids<-() ----

#' @rdname IdVector-methods
#' @aliases ids<-,IdVector-method
#'
#' @export
#'
#' @examples
#' iv1 <- iv
#' ids(iv1)[1] <- "gene1"
setReplaceMethod("ids", "IdVector",
    function(object, value)
    {
        slot(object, "ids") <- value
        object
    }
)

# names() ----

#' @rdname IdVector-methods
#' @aliases names,IdVector-method
#'
#' @examples
#'
#' names(iv)
setMethod("names", "IdVector", function(x) {
    ids(x)
})

# names<-() ----

#' @rdname IdVector-methods
#' @aliases names<-,IdVector-method
#'
#' @importFrom methods slot<-
#'
#' @examples
#' iv1 <- iv
#' names(iv1)[1] <- "GENE001"
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
#'
#' @examples
#'
#' # Dimensions ----
#'
#' length(iv)
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
#'
#' @examples
#'
#' # Subsetting ----
#'
#' iv1 <- iv[1:5]
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
        nu, " unique ", ifelse(nu == 1, "identifier", "identifiers"), "\n",
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
    split(x, as.character(f), drop=drop, ...)
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
#'
#' @examples
#'
#' # Coercion from IdVector ----
#'
#' v1 <- as(iv, "vector")
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
#'
#' @examples
#' c1 <- as(iv, "character")
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
#' `as(object, "IdVector")` and `as.IdVector(object)` return an `IdVector` from the given atomic vector of identifiers.
#'
#' @importFrom methods as
#' @export
as.IdVector.default <- function(ids, ...) {
    IdVector(as.character(ids, ...))
}

setAs("ANY", "IdVector", function(from) {
    as.IdVector.default(as.character(from))
})

# setValidity ----

#' @importFrom methods slot
#' @importMethodsFrom S4Vectors mcols split
setValidity("IdVector", function(object) {

    errors <- c()

    object.mcols <- mcols(object)

    if (any(duplicated(ids(object)))) {
        if (!is.null(object.mcols)) {
            metadataById <- split(object.mcols, rownames(object.mcols))
            uniqueRowsById <- vapply(metadataById, function(x){ nrow(unique(x)) }, integer(1))
            nonUniqueIds <- names(which(uniqueRowsById > 1))
            if (length(nonUniqueIds) > 0) {
                # print(uniqueRowsById[uniqueRowsById > 1])
                textIds <- paste(head(nonUniqueIds, 4), collapse = ", ")
                if (length(nonUniqueIds) > 4) {
                    textIds <- paste0(textIds, ", ...")
                }
                error <- paste0("some identifiers do not have unique metadata: ", textIds)
                errors <- c(errors, error)
            }
        }
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})
