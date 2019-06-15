# Direct pointers ----

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases relations
#' @exportMethod relations
setGeneric("relations", function(object) standardGeneric("relations"))

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases relations<-
#' @exportMethod relations<-
setGeneric("relations<-", function(object, value) standardGeneric("relations<-"))

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases elements
#' @exportMethod elements
setGeneric("elements", function(object) standardGeneric("elements"))

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases elementData
#' @exportMethod elementData
setGeneric("elementData", function(object) standardGeneric("elementData"))

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases elementData<-
#' @exportMethod elementData<-
setGeneric("elementData<-", function(object, value) standardGeneric("elementData<-"))

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases sets
#' @exportMethod sets
setGeneric("sets", function(object) standardGeneric("sets"))

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases setData
#' @exportMethod setData
setGeneric(
    "setData",
    function(object) standardGeneric("setData")
)

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases setData<-
#' @exportMethod setData<-
setGeneric("setData<-", function(object, value) standardGeneric("setData<-"))

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

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases nElements
#' @exportMethod nElements
setGeneric("nElements", function(object) standardGeneric("nElements"))

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases nSets
#' @exportMethod nSets
setGeneric("nSets", function(object) standardGeneric("nSets"))

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases setLengths
#' @exportMethod setLengths
setGeneric("setLengths", function(object) standardGeneric("setLengths"))

#' @name Sets-methods
#' @rdname Sets-methods
#' @aliases elementLengths
#' @exportMethod elementLengths
setGeneric("elementLengths", function(object) standardGeneric("elementLengths"))
