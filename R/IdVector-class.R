# ids() ----

#' @param x An object that inherits from `IdVector`.
#'
#' @rdname IdVector-class
#' @aliases ids,IdVector-method
setMethod("ids", "IdVector", function(x) {
    slot(x, "ids")
})

# ids<-() ----

#' @param value An object of a class specified in the S4 method signature.
#'
#' @rdname IdVector-class
#' @aliases ids<-,IdVector-method
#' @importFrom methods slot<-
setReplaceMethod("ids", "IdVector",
    function(x, value)
    {
        slot(x, "ids") <- value
        x
    }
)

# names() ----

#' @rdname IdVector-class
#' @aliases names,IdVector-method
setMethod("names", "IdVector", function(x) {
    ids(x)
})

# names<-() ----

#' @rdname IdVector-class
#' @aliases names<-,IdVector-method
#' @importFrom methods slot<-
setReplaceMethod("names", "IdVector",
    function(x, value)
    {
        ids(x) <- value
        x
    }
)

# length() ----

#' @rdname IdVector-class
#' @aliases length,IdVector-method
setMethod("length", "IdVector", function(x) {
    length(slot(x, "ids"))
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

setMethod("showAsCell", "IdVector", function(object) {
    slot(object, "ids")
})

# unique() ----

unique.IdVector <- function(object) {
    unique(ids(object))
}

# NSBS ----

setMethod("NSBS", "IdVector", function(i, x, exact=TRUE, strict.upper.bound=TRUE, allow.NAs=FALSE)
{
    i <- match(ids(i), rownames(x))
    i
})

# as.vector() ----

setMethod("as.vector", "IdVector", function(x, mode = "any")
{
    as.vector(ids(x))
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
    ids(from)
})

#' @aliases as.character.IdVector as.character
#' @importFrom methods as
as.character.IdVector <- function(x) {
    as(x, "character")
}
