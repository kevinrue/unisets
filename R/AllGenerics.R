# Direct pointers ----

#' Method relations
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases relations
#' @exportMethod relations
setGeneric(
    "relations",
    function(x) standardGeneric("relations")
)

#' Method elements
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases elements
#' @exportMethod elements
setGeneric(
    "elements",
    function(x) standardGeneric("elements")
)

#' Method elementIds
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases elementIds
#' @exportMethod elementIds
setGeneric(
    "elementIds",
    function(x) standardGeneric("elementIds")
)

#' Method elementIds<-
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases elementIds<-
#' @exportMethod elementIds<-
setGeneric(
    "elementIds<-",
    function(x, value) standardGeneric("elementIds<-")
)

#' Method elementData
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases elementData
#' @exportMethod elementData
setGeneric(
    "elementData",
    function(x) standardGeneric("elementData")
)

#' Method sets
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases sets
#' @exportMethod sets
setGeneric(
    "sets",
    function(x) standardGeneric("sets")
)

#' Method setIds
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases setIds
#' @exportMethod setIds
setGeneric(
    "setIds",
    function(x) standardGeneric("setIds")
)

#' Method setIds<-
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases setIds<-
#' @exportMethod setIds<-
setGeneric(
    "setIds<-",
    function(x, value) standardGeneric("setIds<-")
)

#' Method setData
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases setData
#' @exportMethod setData
setGeneric(
    "setData",
    function(x) standardGeneric("setData")
)

#' Method membership
#' @name FuzzySets-class
#' @rdname FuzzySets-class
#' @aliases membership
#' @exportMethod membership
setGeneric(
    "membership",
    function(x) standardGeneric("membership")
)

#' Method membership<-
#' @name FuzzySets-class
#' @rdname FuzzySets-class
#' @aliases membership<-
#' @exportMethod membership<-
setGeneric(
    "membership<-",
    function(x, value) standardGeneric("membership<-")
)

#' Method id
#' @name IdVector-class
#' @rdname IdVector-class
#' @aliases id
#' @exportMethod id
setGeneric(
    "id",
    function(x) standardGeneric("id")
)

#' Method id<-
#' @name IdVector-class
#' @rdname IdVector-class
#' @aliases id<-
#' @exportMethod id<-
setGeneric(
    "id<-",
    function(x, value) standardGeneric("id<-")
)

# Dimensions ----

#' Method nRelations
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases nRelations
#' @exportMethod nRelations
setGeneric(
    "nRelations",
    function(x) standardGeneric("nRelations")
)

#' Method nElements
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases nElements
#' @exportMethod nElements
setGeneric(
    "nElements",
    function(x) standardGeneric("nElements")
)

#' Method nSets
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases nSets
#' @exportMethod nSets
setGeneric(
    "nSets",
    function(x) standardGeneric("nSets")
)

#' Method setLengths
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases setLengths
#' @exportMethod setLengths
setGeneric(
    "setLengths",
    function(x) standardGeneric("setLengths")
)

#' Method elementLengths
#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases elementLengths
#' @exportMethod elementLengths
setGeneric(
    "elementLengths",
    function(x) standardGeneric("elementLengths")
)
