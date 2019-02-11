# Direct pointers ----

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases relations
#' @exportMethod relations
setGeneric("relations", function(object) standardGeneric("relations"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases elements
#' @exportMethod elements
setGeneric("elements", function(object) standardGeneric("elements"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases elementIds
#' @exportMethod elementIds
setGeneric("elementIds", function(object) standardGeneric("elementIds"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases elementIds<-
#' @exportMethod elementIds<-
setGeneric("elementIds<-", function(object, value) standardGeneric("elementIds<-"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases elementData
#' @exportMethod elementData
setGeneric("elementData", function(object) standardGeneric("elementData"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases sets
#' @exportMethod sets
setGeneric("sets", function(object) standardGeneric("sets"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases setIds
#' @exportMethod setIds
setGeneric(
    "setIds",
    function(object) standardGeneric("setIds")
)

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases setIds<-
#' @exportMethod setIds<-
setGeneric(
    "setIds<-",
    function(object, value) standardGeneric("setIds<-")
)

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases setData
#' @exportMethod setData
setGeneric(
    "setData",
    function(object) standardGeneric("setData")
)

#' @name FuzzyHits-methods
#' @rdname FuzzyHits-methods
#' @aliases membership
#' @exportMethod membership
setGeneric(
    "membership",
    function(object) standardGeneric("membership")
)

#' @name FuzzyHits-methods
#' @rdname FuzzyHits-methods
#' @aliases membership<-
#' @exportMethod membership<-
setGeneric(
    "membership<-",
    function(object, value) standardGeneric("membership<-")
)

#' @name GOHits-methods
#' @rdname GOHits-methods
#' @aliases ontology
#' @exportMethod ontology
setGeneric(
    "ontology",
    function(object) standardGeneric("ontology")
)

#' @name GOHits-methods
#' @rdname GOHits-methods
#' @aliases ontology<-
#' @exportMethod ontology<-
setGeneric(
    "ontology<-",
    function(object, value) standardGeneric("ontology<-")
)

#' @name GOHits-methods
#' @rdname GOHits-methods
#' @aliases evidence
#' @exportMethod evidence
setGeneric(
    "evidence",
    function(object) standardGeneric("evidence")
)

#' @name GOHits-methods
#' @rdname GOHits-methods
#' @aliases evidence<-
#' @exportMethod evidence<-
setGeneric(
    "evidence<-",
    function(object, value) standardGeneric("evidence<-")
)

#' @name IdVector-methods
#' @rdname IdVector-methods
#' @aliases ids<-
#' @exportMethod ids<-
setGeneric("ids<-", function(object, value) standardGeneric("ids<-"))

# Dimensions ----

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases nElements
#' @exportMethod nElements
setGeneric("nElements", function(object) standardGeneric("nElements"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases nSets
#' @exportMethod nSets
setGeneric("nSets", function(object) standardGeneric("nSets"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases setLengths
#' @exportMethod setLengths
setGeneric("setLengths", function(object) standardGeneric("setLengths"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases elementLengths
#' @exportMethod elementLengths
setGeneric("elementLengths", function(object) standardGeneric("elementLengths"))
