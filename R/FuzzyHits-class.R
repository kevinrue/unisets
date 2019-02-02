
# Accessors ----

#' @rdname FuzzyHits-methods
#' @aliases membership,FuzzyHits-method
#'
#' @section Accessors:
#' `membership(x)` returns a `numeric` vector of membership function for each relation.
#'
#' @importFrom methods slot
setMethod("membership", "FuzzyHits", function(x) {
    mcols(x)[["membership"]]
})

#' @rdname FuzzyHits-methods
#' @aliases membership<-,FuzzyHits-method
#'
#' @importFrom methods validObject
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
