
# Accessors ----

#' @rdname FuzzyHits-methods
#' @aliases membership,FuzzyHits-method
#'
#' @section Accessors:
#' `membership(x)` returns a `numeric` vector of membership function for each relation.
#'
#' @importFrom methods slot
#'
#' @examples
#'
#' # Accessors ----
#'
#' membership(fh)
setMethod("membership", "FuzzyHits", function(x) {
    mcols(x)[["membership"]]
})

#' @rdname FuzzyHits-methods
#' @aliases membership<-,FuzzyHits-method
#'
#' @importFrom methods validObject
#'
#' @examples
#' fh1 <- fh
#' membership(fh1)[1] <- 0
setReplaceMethod("membership", "FuzzyHits",
    function(x, value)
    {
        mcols(x)[["membership"]] <- value
        x
    }
)

# show() ----

#' @importFrom S4Vectors mcols
setMethod("show", "FuzzyHits", function(object) {
    mcols(object) <- mcols(object)[, "membership", drop=FALSE]
    S4Vectors:::showHits(object, margin="  ", print.classinfo=TRUE, print.nnode=TRUE)
})

# as(Hits, "FuzzyHits") ----

#' @importFrom methods new
#' @importFrom S4Vectors mcols
setAs("Hits", "FuzzyHits", function(from) {
    if (! "membership" %in% colnames(mcols(from))) {
        mcols(from)[["membership"]] <- rep(1, length(from))
    }
    from <- subset(from, !is.na(membership))
    to <- new("FuzzyHits", from)
    to
})

# setValidity ----

#' @importFrom methods slot
#' @importFrom S4Vectors mcols
setValidity("FuzzyHits", function(object) {
    errors <- c()

    if (! "membership" %in% colnames(mcols(object))) {
        error <- "membership column missing in mcols(object)"
        return(error)
    }

    membership <- mcols(object)[["membership"]]

    if (!is.numeric(membership)) {
        error <- "membership function must be numeric"
        return(error)
    }

    if (any(is.na(membership) | membership < 0 | membership > 1)) {
        error <- "membership function must be in the interval [0,1]"
        errors <- c(errors, error)
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})
