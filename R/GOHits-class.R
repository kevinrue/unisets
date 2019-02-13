
# Accessors ----

#' @rdname GOHits-methods
#' @aliases evidence,GOHits-method
#'
#' @section Accessors:
#' `evidence(object)` returns a factor indicating the evidence code for each relation.
#'
#' @importFrom methods slot
#'
#' @examples
#' # Constructor ----
#'
#' from <- c(5, 2, 3, 3, 3, 2)
#' to <- c(11, 15, 5, 4, 5, 11)
#' ontology <- factor(c("BP", "BP", "BP", "MF", "MF", "CC"))
#' evidence <- factor(c("IEA", "IDA", "IEA", "IDA", "IEA", "IDA"))
#'
#' gh <- GOHits(from, to, evidence, ontology, 7, 15)
#' gh
#'
#' # Accessors ----
#'
#' evidence(gh)
setMethod("evidence", "GOHits", function(object) {
    mcols(object)[["evidence"]]
})

#' @rdname GOHits-methods
#' @aliases evidence<-,GOHits-method
#'
#' @importFrom methods validObject
#'
#' @examples
#' gh1 <- gh
#' evidence(gh1)[1] <- "HTP"
setReplaceMethod("evidence", "GOHits",
    function(object, value)
    {
        value <- factor(value, names(GOEvidenceCodes))
        mcols(object)[["evidence"]] <- value
        validObject(object)
        object
    }
)

#' @rdname GOHits-methods
#' @aliases ontology,GOHits-method
#'
#' @section Accessors:
#' `ontology(object)` returns a factor indicating the ontology code for each relation.
#'
#' @importFrom methods slot
#'
#' @examples
#' ontology(gh)
setMethod("ontology", "GOHits", function(object) {
    mcols(object)[["ontology"]]
})

#' @rdname GOHits-methods
#' @aliases ontology<-,GOHits-method
#'
#' @importFrom methods validObject
#'
#' @examples
#' gh1 <- gh
#' ontology(gh1)[1] <- "MF"
setReplaceMethod("ontology", "GOHits",
    function(object, value)
    {
        value <- factor(value, names(GOOntologyCodes))
        mcols(object)[["ontology"]] <- value
        validObject(object)
        object
    }
)

# show() ----

#' @importFrom S4Vectors mcols
setMethod("show", "GOHits", function(object) {
    callNextMethod()
})

# as(Hits, "GOHits") ----

#' @importFrom methods new
#' @importFrom S4Vectors mcols
setAs("Hits", "GOHits", function(from) {
    # stopifnot("evidence" %in% colnames(mcols(from)))
    # stopifnot("ontology" %in% colnames(mcols(from)))
    evidence <- mcols(from)[["evidence"]]
    ontology <- mcols(from)[["ontology"]]
    # Coercing to factor if necessary
    if (!is.factor(evidence)) {
        message("Coercing evidence to factor")
    }
    evidence <- factor(evidence, names(GOEvidenceCodes))
    if (!is.factor(ontology)) {
        message("Coercing ontology to factor")
    }
    ontology <- factor(ontology, names(GOOntologyCodes))
    mcols(from)[["evidence"]] <- evidence
    mcols(from)[["ontology"]] <- ontology
    # NA is not allowed for evidence and ontology
    to <- new("GOHits", from)
    validObject(to)
    to
})

# setValidity ----

#' @importFrom methods slot
#' @importFrom S4Vectors mcols
setValidity("GOHits", function(object) {
    errors <- c()

    if (! "evidence" %in% colnames(mcols(object))) {
        error <- "evidence column missing in mcols(object)"
        errors <- c(errors, error)
    }

    if (! "ontology" %in% colnames(mcols(object))) {
        error <- "evidence column missing in mcols(object)"
        errors <- c(errors, error)
    }

    if (length(errors > 0)){
        return(errors)
    }

    evidence <- mcols(object)[["evidence"]]
    ontology <- mcols(object)[["ontology"]]

    if (!is.factor(evidence)) {
        error <- "evidence(object) function must be factor"
        errors <- c(errors, error)
    }

    if (!is.factor(ontology)) {
        error <- "ontology(object) function must be factor"
        errors <- c(errors, error)
    }

    if (length(errors > 0)){
        return(errors)
    }

    if (any(is.na(evidence))) {
        warn <- "invalid evidence code, NA generated (See ?GOEvidenceCodes)"
        warning(warn)
    }

    if (any(is.na(ontology))) {
        warn <- "invalid ontology code, NA generated (See ?GOOntologyCodes)"
        warning(warn)
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})
