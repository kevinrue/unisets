# membership() ----

#' @param x An object that inherits from `FuzzyHits`.
#'
#' @rdname FuzzyHits-class
#' @aliases membership,FuzzyHits-method
#' @importFrom methods slot
setMethod("membership", "FuzzyHits", function(x) {
    elementMetadata(x)[["membership"]]
})

#' @param value An object of a class specified in the S4 method signature or as outlined in 'Slots'.
#'
#' @rdname FuzzyHits-class
#' @aliases membership<-,FuzzyHits-method
#' @importFrom methods validObject
setMethod("membership<-", "FuzzyHits", function(x, value) {
    elementMetadata(x)[["membership"]] <- value
    x
})

# show() ----

setMethod("show", "FuzzyHits", function(object) {
    elementMetadata(object) <- elementMetadata(object)[, "membership", drop=FALSE]
    S4Vectors:::showHits(object, margin = "  ", print.classinfo = TRUE, print.nnode = TRUE)
})

# as(Hits, "FuzzyHits") ----

#' @importFrom methods new
setAs("Hits", "FuzzyHits", function(from) {
    if (! "membership" %in% colnames(elementMetadata(from))) {
        elementMetadata(from)[["membership"]] <- rep(1, length(from))
    }
    from <- subset(from, !is.na(membership))
    to <- new("FuzzyHits", from)
    to
})

