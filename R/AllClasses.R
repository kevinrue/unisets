#' BaseSets Class
#'
#' The `BaseSets` class implements a container to describe distinct objects that make up sets, along with element metadata and set metadata.
#'
#' @slot relations DataFrame. Two columns provide mapping relationships between `"element"` and `"set"`.
#' @slot elementData DataFrame. Provide metadata for each unique element in `relations$element`.
#' @slot setData DataFrame. Provide metadata for each unique element in `relations$set`.
#'
#' @return A `BaseSets` object.
#' @export
#' @exportClass BaseSets
#'
#' @examples
#' # Constructor ----
#'
#' # Visually intuitive definition of sets
#' sets <- list(
#'   set1=c("A", "B"),
#'   set2=c("B", "C", "D"),
#'   set3=c("E"))
#'
#' # Reformat as a table
#' relations <- DataFrame(
#'   element=unlist(sets),
#'   set=rep(names(sets), lengths(sets))
#' )
#'
#' bs <- BaseSets(relations)
#'
#' # Subsetting ----
#'
#' bs1 <- subset(bs, set == "set1" | element == "E")
#'
#' # Coercing ----
#'
#' # to list (gene sets)
#' ls1 <- as(bs, "list")
#' # to matrix (logical membership)
#' m1 <- as(bs, "matrix")
#'
#' # Read-only getters ----
#'
#' nRelations(bs)
#' nElements(bs)
#' nSets(bs)
#' elementIds(bs)
#' setIds(bs)
#'
#' setLengths(bs)
#' elementLengths(bs)
#'
#' # Getters/Setters ----
#'
#' relations(bs)
#' elementData(bs)
#' setData(bs)
#'
#' bs1 <- bs
#' relations(bs1) <- relations(bs1)[sample(nRelations(bs1)), , drop=FALSE]
#' elementData(bs1) <- elementData(bs1)[sample(nElements(bs1)), , drop=FALSE]
#' setData(bs1) <- setData(bs1)[sample(nSets(bs1)), , drop=FALSE]
#' elementIds(bs) <- paste0("gene", seq_len(nElements(bs)))
#' setIds(bs) <- paste0("geneset", seq_len(nSets(bs)))
#'
setClass(
    "BaseSets",
    slots=c(
        relations="DataFrame",
        elementData="DataFrame",
        setData="DataFrame"
        ),
    prototype=list(
        relations=DataFrame(
            element=character(0),
            set=character(0)
            ),
        elementData=DataFrame(
            row.names=character(0)
            ),
        setData=DataFrame(
            row.names=character(0)
            )
        )
)

#' @importFrom methods slot
setValidity("BaseSets", function(object) {

    errors <- c()

    slot.relations <- slot(object, "relations")
    if (!identical(colnames(slot.relations), c("element", "set"))) {
        error <- "colnames(relations(object)) must be c(\"element\", \"set\")"
        return(error)
    }

    # things to compute once
    uniqueElements <- sort(unique(slot.relations$element))
    uniqueSets <- sort(unique(slot.relations$set))

    # TODO: rownames of metadata tables cannot be NULL!

    if (!identical(uniqueElements, sort(rownames(slot(object, "elementData"))))) {
        error <- "Mismatch between relations$element and rownames(elementData)"
        errors <- c(errors, error)
    }

    if (!identical(uniqueSets, sort(rownames(slot(object, "setData"))))) {
        error <- "Mismatch between relations$set and rownames(setData)"
        errors <- c(errors, error)
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})

#' @param relations DataFrame. Two columns provide mapping relationships between `"element"` and `"set"`.
#' @param elementData DataFrame. Provide metadata for each unique element in `relations$element`.
#' @param setData DataFrame. Provide metadata for each unique element in `relations$set`.
#'
#' @rdname BaseSets-class
#' @aliases BaseSets
#' @export
#' @importFrom S4Vectors DataFrame
#' @importFrom methods new
BaseSets <- function(relations, elementData, setData) {
    # Drop names if present
    if (!is.null(rownames(relations))) {
        message("Setting rownames(relations) to NULL")
        rownames(relations) <- NULL
    }
    if (!is.null(names(relations$element))) {
        message("Setting names(relations$element) to NULL")
        names(relations$element) <- NULL
    }
    if (!is.null(names(relations$set))) {
        message("Setting names(relations$set) to NULL")
        names(relations$set) <- NULL
    }

    # Add missing metadata
    if (missing(elementData)) {
        elementData <- DataFrame(row.names=sort(unique(relations$element)))
    }
    if (missing(setData)) {
        setData <- DataFrame(row.names=sort(unique(relations$set)))
    }

    new("BaseSets", relations=relations, elementData=elementData, setData=setData)
}

#' FuzzySets Class
#'
#' The `FuzzySets` class extends the [`BaseSets`] class to implement a container that also describe different grades of membershipin the interval `[0,1]`.
#'
#' @slot membership numeric. Membership function.
#'
#' @return A `FuzzySets` object.
#' @export
#' @exportClass FuzzySets
#'
#' @seealso [`BaseSets`]
#'
#' @examples
#' # Constructor ----
#'
#' # Visually intuitive definition of sets
#' sets <- list(
#'   set1=c("A", "B"),
#'   set2=c("B", "C", "D"),
#'   set3=c("E"))
#'
#' # Reformat as a table
#' relations <- DataFrame(
#'   element=unlist(sets),
#'   set=rep(names(sets), lengths(sets))
#' )
#'
#' # Generate random values for the membership function
#' membership <- round(runif(nrow(relations)), 2)
#'
#' fs <- FuzzySets(relations=relations, membership=membership)
#'
#' # Subsetting ----
#'
#' fs1 <- subset(fs, set == "set1" | membership > 0.5)
#'
#' # Coercing ----
#'
#' # to list (gene sets)
#' ls1 <- as(fs, "list")
#' # to matrix (continuous membership)
#' ls1 <- as(fs, "matrix")
#'
#' # Getters/Setters ----
#'
#' membership(fs)
#'
#' fs1 <- fs
#' membership(fs1) <- runif(nRelations(fs1))
#'
setClass(
    "FuzzySets",
    slots=c(
        membership="numeric"
        ),
    prototype=list(
       membership=numeric(0)
        ),
    contains="BaseSets"
)

#' @importFrom methods slot
setValidity("FuzzySets", function(object) {

    errors <- c()

    # things to compute once
    slot.relations <- slot(object, "relations")
    slot.membership <- slot(object, "membership")

    if (!identical(length(slot.membership), nrow(slot.relations))) {
        error <- "length(membership) must be equal to nrow(relations)"
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
})

#' @param ... Arguments to pass to the [BaseSets()] constructor,
#' or to and rom other functions.
#' @param membership Numeric. Vector of membership in the range `[0,1]`
#'
#' @rdname FuzzySets-class
#' @aliases FuzzySets
#' @export
#' @importFrom methods new
FuzzySets <- function(..., membership) {
    # Drop names if present
    if (!is.null(names(membership))) {
        message("Setting names(membership) to NULL")
        names(membership) <- NULL
    }
    # Pass basic arguments to BaseSets constructor
    fs <- BaseSets(...)
    fs <- new("FuzzySets", fs, membership=membership)
    # Remove relations with membership function equal to 0, to respect the inheritance from BaseSets
    fs <- subset(fs, membership > 0)
    fs
}
