
#' Validity Method for BaseSet Objects
#'
#' @rdname INTERNAL_valid_BaseSet
#'
#' @param object An object that inherits from `BaseSet`.
#'
#' @return If the object is valid, \code{TRUE};
#' otherwise, a character vector describing all the validity failures encountered.
#' @importFrom methods slot
.valid.BaseSet <- function(object){

    errors <- c()

    slot.map <- slot(object, "map")
    if (!identical(colnames(slot.map), c("element", "set"))) {
        error <- "colnames(object@map) must be c(\"element\", \"set\")"
        return(error)
    }

    # things to compute once
    uniqueElements <- sort(unique(slot.map$element))
    uniqueSets <- sort(unique(slot.map$set))

    # TODO: rownames of metadata tables cannot be NULL!

    if (!identical(uniqueElements, sort(rownames(slot(object, "elementData"))))) {
        error <- "Some elements differ between the 'map' and 'elementData' slots"
        errors <- c(errors, error)
    }

    if (!identical(uniqueSets, sort(rownames(slot(object, "setData"))))) {
        error <- "Some sets differ between the 'map' and 'elementData' slots"
        errors <- c(errors, error)
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
}

#' BaseSet Class
#'
#' The `BaseSet` class implements a minimal container to describe distinct objects that make up sets.
#'
#' @slot map DataFrame. Two columns provide mapping relationships between `"element"` and `"set"`.
#' @slot elementData DataFrame. Provide metadata for each unique element in `map$element`.
#' @slot setData DataFrame. Provide metadata for each unique element in `map$set`.
#'
#' @exportClass BaseSet
#' @export
setClass(
    "BaseSet",
    slots=c(
        map="DataFrame",
        elementData="DataFrame",
        setData="DataFrame"
        ),
    prototype=list(
        map=DataFrame(
            element=character(0),
            set=character(0)
            ),
        elementData=DataFrame(
            row.names=character(0)
            ),
        setData=DataFrame(
            row.names=character(0)
            )
        ),
    validity=.valid.BaseSet
    )

#' Constructor for the `BaseSet` class
#'
#' @param map DataFrame. Two columns provide mapping relationships between `"element"` and `"set"`.
#' @param elementData DataFrame. Provide metadata for each unique element in `map$element`.
#' @param setData DataFrame. Provide metadata for each unique element in `map$set`.
#'
#' @return A `BaseSet` object.
#' @export
#' @importFrom S4Vectors DataFrame
#' @importFrom methods new
#'
#' @examples
#' # Visually intuitive definition of sets
#' sets <- list(
#'   set1=c("A", "B"),
#'   set2=c("C", "D"))
#'
#' # Reformat as a table
#' map <- DataFrame(
#'   element=unlist(sets),
#'   set=rep(names(sets), lengths(sets))
#' )
#'
#' bs <- BaseSet(map)
BaseSet <- function(map, elementData, setData) {

    if (!is.null(rownames(map))) {
        message("Setting rownames(map) to NULL")
        rownames(map) <- NULL
    }

    if (missing(elementData)) {
        elementData <- DataFrame(row.names=sort(unique(map$element)))
    }
    if (missing(setData)) {
        setData <- DataFrame(row.names=sort(unique(map$set)))
    }

    new("BaseSet", map=map, elementData=elementData, setData=setData)
}
