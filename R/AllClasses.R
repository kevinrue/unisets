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
        error <- "Mismatch between map$element and rownames(elementData)"
        errors <- c(errors, error)
    }

    if (!identical(uniqueSets, sort(rownames(slot(object, "setData"))))) {
        error <- "Mismatch between map$set and rownames(setData)"
        errors <- c(errors, error)
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
}

#' BaseSet Class
#'
#' The `BaseSet` class implements a container to describe distinct objects that make up sets, along with element metadata and set metadata.
#'
#' @slot map DataFrame. Two columns provide mapping relationships between `"element"` and `"set"`.
#' @slot elementData DataFrame. Provide metadata for each unique element in `map$element`.
#' @slot setData DataFrame. Provide metadata for each unique element in `map$set`.
#'
#' @return A `BaseSet` object.
#' @export
#' @exportClass BaseSet
#'
#' @examples
#' # Constructor ----
#'
#' # Visually intuitive definition of sets
#' sets <- list(
#'   set1=c("A", "B"),
#'   set2=c("C", "D"),
#'   set3=c("E"))
#'
#' # Reformat as a table
#' map <- DataFrame(
#'   element=unlist(sets),
#'   set=rep(names(sets), lengths(sets))
#' )
#'
#' bs <- BaseSet(map)
#'
#' # Subsetting ----
#'
#' bs1 <- subset(bs, set == "set1" | element == "E")
#'
#' # Coercing to list ----
#' ls1 <- as(bs, "list")
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

#' @param map DataFrame. Two columns provide mapping relationships between `"element"` and `"set"`.
#' @param elementData DataFrame. Provide metadata for each unique element in `map$element`.
#' @param setData DataFrame. Provide metadata for each unique element in `map$set`.
#'
#' @rdname BaseSet-class
#' @aliases BaseSet
#' @export
#' @importFrom S4Vectors DataFrame
#' @importFrom methods new
BaseSet <- function(map, elementData, setData) {
    # Drop names if present
    if (!is.null(rownames(map))) {
        message("Setting rownames(map) to NULL")
        rownames(map) <- NULL
    }
    if (!is.null(names(map$element))) {
        message("Setting names(map$element) to NULL")
        names(map$element) <- NULL
    }
    if (!is.null(names(map$set))) {
        message("Setting names(map$set) to NULL")
        names(map$set) <- NULL
    }

    if (missing(elementData)) {
        elementData <- DataFrame(row.names=sort(unique(map$element)))
    }
    if (missing(setData)) {
        setData <- DataFrame(row.names=sort(unique(map$set)))
    }

    new("BaseSet", map=map, elementData=elementData, setData=setData)
}

#' Validity Method for BaseSet Objects
#'
#' @rdname INTERNAL_valid_FuzzySet
#'
#' @param object An object that inherits from `FuzzySet`.
#'
#' @return If the object is valid, \code{TRUE};
#' otherwise, a character vector describing all the validity failures encountered.
.valid.FuzzySet <- function(object) {

    errors <- c()

    # things to compute once
    slot.map <- slot(object, "map")
    slot.membership <- slot(object, "membership")

    if (!identical(length(slot.membership), nrow(slot.map))) {
        error <- "length(membership) must be equal to nrow(map)"
        errors <- c(errors, error)
    }

    if (any(slot.membership > 1 | slot.membership < 0)) {
        error <- "membership function must be in the interval [0,1]"
        errors <- c(errors, error)
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
}

#' FuzzySet Class
#'
#' The `FuzzySet` class extends the [`BaseSet`] class to implement a container that also describe different grades of membershipin the interval `[0,1]`.
#'
#' @slot membership numeric. Membership function.
#'
#' @return A `FuzzySet` object.
#' @export
#' @exportClass FuzzySet
#'
#' @examples
#' # Constructor ----
#'
#' # Visually intuitive definition of sets
#' sets <- list(
#'   set1=c("A", "B"),
#'   set2=c("C", "D"),
#'   set3=c("E"))
#'
#' # Reformat as a table
#' map <- DataFrame(
#'   element=unlist(sets),
#'   set=rep(names(sets), lengths(sets))
#' )
#'
#' # Generate random values for the membership function
#' membership <- runif(nrow(map))
#'
#' fs <- FuzzySet(map=map, membership=membership)
#'
#' # Subsetting ----
#'
#' fs1 <- subset(fs, set == "set1" | membership > 0.5)
#'
#' # Coercing to list ----
#' ls1 <- as(fs, "list")
setClass(
    "FuzzySet",
    slots=c(
        membership="numeric"
        ),
    prototype=list(
       membership=numeric(0)
        ),
    contains="BaseSet",
    validity=.valid.FuzzySet
)

#' @param ... Arguments to pass to the [BaseSet()] constructor.
#' @param membership Numeric. Vector of membership in the range `[0,1]`
#'
#' @rdname FuzzySet-class
#' @aliases FuzzySet
#' @export
#' @importFrom methods new
FuzzySet <- function(..., membership) {
    fs <- BaseSet(...)

    if (!is.null(names(membership))) {
        message("Setting names(membership) to NULL")
        names(membership) <- NULL
    }

    fs <- new("FuzzySet", fs, membership=membership)
    fs
}
