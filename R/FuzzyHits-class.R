# membership() ----

#' @param x An object that inherits from `FuzzyHits`.
#'
#' @rdname FuzzyHits-class
#' @aliases membership,FuzzyHits-method
#' @importFrom methods slot
setMethod("membership", "FuzzyHits", function(x) {
    mcols(x)[["membership"]]
})

#' @param value An object of a class specified in the S4 method signature or as outlined in 'Slots'.
#'
#' @rdname FuzzyHits-class
#' @aliases membership<-,FuzzyHits-method
#' @importFrom methods validObject
setReplaceMethod("membership", "FuzzyHits",
    function(x, value)
    {
        mcols(x)[["membership"]] <- value
        x
    }
)

# show() ----

setMethod("show", "FuzzyHits", function(object) {
    mcols(object) <- mcols(object)[, "membership", drop=FALSE]
    S4Vectors:::showHits(object, margin = "  ", print.classinfo = TRUE, print.nnode = TRUE)
})

# as(Hits, "FuzzyHits") ----

#' @importFrom methods new
setAs("Hits", "FuzzyHits", function(from) {
    if (! "membership" %in% colnames(mcols(from))) {
        mcols(from)[["membership"]] <- rep(1, length(from))
    }
    from <- subset(from, !is.na(membership))
    to <- new("FuzzyHits", from)
    to
})

