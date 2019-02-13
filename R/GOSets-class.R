# Direct pointers ----

#' @rdname GOSets-methods
#' @aliases evidence,GOSets-method
#'
#' @section Accessors:
#' `evidence(object)` returns a `numeric` vector of evidence function for each relation.
#'
#' @importFrom S4Vectors DataFrame
#'
#' @examples
#' # Constructor ----
#'
#' # Fetch a sample of GO annotations
#' library(org.Hs.eg.db)
#' base_sets <- import(org.Hs.egGO)
#' relations <- as.data.frame(head(base_sets))
#'
#' gs <- GOSets(relations)
#'
#' # Accessors ----
#'
#' evidence(gs)
setMethod("evidence", "GOSets", function(object) {
    evidence(relations(object))
})

#' @rdname GOSets-methods
#' @aliases evidence<-,GOSets-method
#'
#' @importFrom methods validObject
#'
#' @examples
#' gs1 <- gs
#' evidence(gs1)[1] <- "EXP"
setReplaceMethod("evidence", "GOSets",
    function(object, value)
    {
        evidence(relations(object)) <- value
        validObject(object)
        object
    }
)

#' @rdname GOSets-methods
#' @aliases ontology,GOSets-method
#'
#' @section Accessors:
#' `ontology(object)` returns a `numeric` vector of ontology function for each relation.
#'
#' @importFrom S4Vectors DataFrame
#'
#' @examples
#' ontology(gs)
setMethod("ontology", "GOSets", function(object) {
    ontology(relations(object))
})

#' @rdname GOSets-methods
#' @aliases ontology<-,GOSets-method
#'
#' @importFrom methods validObject
#'
#' @examples
#' gs1 <- gs
#' ontology(gs1)[1] <- "CC"
setReplaceMethod("ontology", "GOSets",
    function(object, value)
    {
        ontology(relations(object)) <- value
        validObject(object)
        object
    }
)

# subset ----

#' @rdname GOSets-methods
#' @aliases subset.GOSets subset,GOSets-method
#'
#' @param ... Additional arguments passed to and from other methods.
#'
#' @section Subsetting:
#' `subset(x, subset, ...)` returns subsets of relations which meet conditions.
#' For `GOSets` objects, the `subset` argument should be a logical expression referring to any of `"element"`, `"set"`, `"evidence"`, `"ontology"` and other available relation metadata indicating elements or rows to keep: missing values are taken as false.
#' In addition, metadata for elements and sets that are not represented in the remaining relations are also dropped.
#'
#' @importFrom methods as
#' @importFrom BiocGenerics eval unique
#' @importFrom S4Vectors from to subset
#' @method subset GOSets
#' @export
#'
#' @examples
#'
#' # Subsetting ----
#'
#' gs1 <- subset(gs, ontology == "BP" & evidence == "TAS")
#' gs1
subset.GOSets <- function(x, ...) subset(x, ...)

setMethod("subset", "GOSets", function(x, ...) {
    out <- callNextMethod()

    out <- as(out, "GOSets")
    out
})

# setValidity ----

#' @importFrom methods slot
setValidity("GOSets", function(object) {

    errors <- c()

    protectedRelationMetadata <- c("evidence", "ontology")
    .requireRelationsMetadataColnames(protectedRelationMetadata, colnames(mcols(relations(object))))

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})
