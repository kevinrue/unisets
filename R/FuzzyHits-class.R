# membership() ----

#' @param x An object that inherits from `FuzzyHits`.
#'
#' @rdname FuzzyHits-class
#' @aliases membership,FuzzyHits-method
#' @importFrom methods slot
setMethod("membership", "FuzzyHits", function(x) {
    slot(x, "membership")
})

#' @param value An object of a class specified in the S4 method signature or as outlined in 'Slots'.
#'
#' @rdname FuzzyHits-class
#' @aliases membership<-,FuzzyHits-method
#' @importFrom methods validObject
setMethod("membership<-", "FuzzyHits", function(x, value) {
    x@membership <- value
    validObject(x)
    x
})

# show() ----

setMethod("show", "FuzzyHits", function(object) {
    elementMetadata(object) <- DataFrame(membership=membership(object))
    S4Vectors:::showHits(object, margin = "  ", print.classinfo = TRUE, print.nnode = TRUE)
})

# as(Hits, "FuzzyHits") ----

#' @importFrom methods new
setAs("Hits", "FuzzyHits", function(from) {
    new("FuzzyHits", from, membership=rep(1, length(from)))
})

