# Direct pointers ----

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases relations
#' @exportMethod relations
setGeneric("relations", function(x) standardGeneric("relations"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases elements
#' @exportMethod elements
setGeneric("elements", function(x) standardGeneric("elements"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases elementIds
#' @exportMethod elementIds
setGeneric("elementIds", function(x) standardGeneric("elementIds"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases elementIds<-
#' @exportMethod elementIds<-
setGeneric("elementIds<-", function(x, value) standardGeneric("elementIds<-"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases elementData
#' @exportMethod elementData
setGeneric("elementData", function(x) standardGeneric("elementData"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases sets
#' @exportMethod sets
setGeneric("sets", function(x) standardGeneric("sets"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases setIds
#' @exportMethod setIds
setGeneric(
    "setIds",
    function(x) standardGeneric("setIds")
)

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases setIds<-
#' @exportMethod setIds<-
setGeneric(
    "setIds<-",
    function(x, value) standardGeneric("setIds<-")
)

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases setData
#' @exportMethod setData
setGeneric(
    "setData",
    function(x) standardGeneric("setData")
)

#' @name FuzzyHits-methods
#' @rdname FuzzyHits-methods
#' @aliases membership
#' @exportMethod membership
setGeneric(
    "membership",
    function(x) standardGeneric("membership")
)

#' @name FuzzyHits-methods
#' @rdname FuzzyHits-methods
#' @aliases membership<-
#' @exportMethod membership<-
setGeneric(
    "membership<-",
    function(x, value) standardGeneric("membership<-")
)

#' @name IdVector-methods
#' @rdname IdVector-methods
#' @aliases ids
#' @exportMethod ids
setGeneric("ids", function(x) standardGeneric("ids"))

#' @name IdVector-methods
#' @rdname IdVector-methods
#' @aliases ids<-
#' @exportMethod ids<-
setGeneric("ids<-", function(x, value) standardGeneric("ids<-"))

# Dimensions ----

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases nElements
#' @exportMethod nElements
setGeneric("nElements", function(x) standardGeneric("nElements"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases nSets
#' @exportMethod nSets
setGeneric("nSets", function(x) standardGeneric("nSets"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases setLengths
#' @exportMethod setLengths
setGeneric("setLengths", function(x) standardGeneric("setLengths"))

#' @name BaseSets-methods
#' @rdname BaseSets-methods
#' @aliases elementLengths
#' @exportMethod elementLengths
setGeneric("elementLengths", function(x) standardGeneric("elementLengths"))
