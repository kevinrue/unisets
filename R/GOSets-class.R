# Direct pointers ----

#' @rdname GOSets-methods
#' @aliases evidence,GOSets-method
#'
#' @section Accessors:
#' `evidence(object)` returns a factor indicating the evidence code for each relation.
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
#' `ontology(object)` returns a factor indicating the ontology code for each relation.
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
