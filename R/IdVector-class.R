# id() ----

#' @param x An object that inherits from `IdVector`.
#'
#' @rdname IdVector-class
#' @aliases id,IdVector-method
setMethod("id", "IdVector", function(x) {
    slot(x, "id")
})

# id<-() ----

#' @param value An object of a class specified in the S4 method signature.
#'
#' @rdname IdVector-class
#' @aliases id<-,IdVector-method
#' @importFrom methods slot<-
setMethod("id<-", "IdVector", function(x, value) {
    slot(x, "id") <- value
    x
})

# length() ----

#' @rdname IdVector-class
#' @aliases length,IdVector-method
setMethod("length", "IdVector", function(x) {
    length(slot(x, "id"))
})

# [ ----

#' @param i index specifying elements to extract or replace
#' @param j,...,drop Ignored
#'
#' @rdname IdVector-class
#' @aliases [,IdVector-method
setMethod("[", "IdVector", function(x, i, j, ..., drop = TRUE) {
    callNextMethod()
})

# show() ----

setMethod("show", "IdVector", function(object) {
    ne <- length(object)
    nu <- length(unique(slot(object, "id")))
    cat(
        class(object), " of length ", ne, " with ",
        nu, " unique ", ifelse(ne == 1, "identifier", "identifiers"), "\n",
        sep = ""
    )
    # Preview of identifiers if any
    if (length(object) > 0) {
        ids <- paste(head(id(object), 4), collapse=", ")
        if (ne > 4) {
            ids <- paste0(ids, ", ...")
        }
        ids <- paste0("Ids: ", ids)
        cat(ids, "\n", sep = "")
    }
    # Preview of metadata if any
    if (!is.null(elementMetadata(object))) {
        nem <- ncol(elementMetadata(object))
        metadata <- paste(head(colnames(elementMetadata(object)), 4), collapse=", ")
        if (nem > 4) {
            metadata <- paste0(metadata, ", ...")
        }
        metadata <- paste0(
            "Metadata: ", metadata, " (", nem,
            ifelse(nem == 1, " column", " columns"), ")")
        cat(metadata, "\n", sep = "")
    }
    invisible(id(object))
})

# showAsCell() ----

setMethod("showAsCell", "IdVector", function(object) {
    slot(object, "id")
})

# unique() ----

unique.IdVector <- function(object) {
    unique(id(object))
}

# NSBS ----

setMethod("NSBS", "IdVector", function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
{
    i <- match(id(i), rownames(x))
    i
})

# as.vector() ----

setMethod("as.vector", "IdVector", function(x, mode = "any")
{
    as.vector(id(x))
})

# pcompare() ----

setMethod("pcompare", c("IdVector", "IdVector"), function(x, y)
{
    id(x) == id(y)
})

setMethod("pcompare", c("ANY", "IdVector"), function(x, y)
{
    x == id(y)
})

setMethod("pcompare", c("IdVector", "ANY"), function(x, y)
{
    id(x) == y
})

# as.IdVector() ----

setAs("ANY", "IdVector", function(from) {
    IdVector(as.character(from))
})

#' @aliases as.IdVector.character as.IdVector
#' @importFrom methods as
as.IdVector.character <- function(x, ...) {
    as(x, "IdVector")
}

# as.character() ----

setAs("IdVector", "character", function(from) {
    id(from)
})

#' @aliases as.character.IdVector as.character
#' @importFrom methods as
as.character.IdVector <- function(x) {
    as(x, "character")
}
