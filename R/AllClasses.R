#' IdVector Class
#'
#' The `IdVector` class extends the [`Vector`] class to implement a container that hold a vector of Entrez gene character identifiers.
#'
#' @slot id character. Entrez gene identifiers.
#'
#' @return A `IdVector` object.
#' @export
#' @exportClass IdVector
#' @importClassesFrom S4Vectors Vector
#'
#' @seealso [`Vector`]
#'
#' @examples
#' # Constructor ----
#'
#' tv <- IdVector(id=rep(head(LETTERS, 3), each=2))
#'
#' # Subsetting ----
#'
#' tv1 <- tv[1:5]
#'
setClass("IdVector",
         contains="Vector",
         representation(
             id="character"
         ),
         prototype(
             id=character(0)
         )
)

#' @importFrom methods callNextMethod
setMethod("parallelSlotNames", "IdVector", function(x) {
    c("id", callNextMethod())
})

#' @importFrom methods slot
setValidity("IdVector", function(object) {

    errors <- c()

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})

#' @param id character. Entrez gene identifiers.
#'
#' @rdname IdVector-class
#' @aliases IdVector
#' @export
#' @importFrom methods new
IdVector <- function(id=character(0)) {
    # Drop names if present
    if (!is.null(names(id))) {
        message("Setting names(id) to NULL")
        names(id) <- NULL
    }

    new("IdVector", id=id)
}

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
#' @importClassesFrom S4Vectors Hits
#' @importFrom S4Vectors Hits
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
#' relations(bs)
#' elementData(bs)
#' setData(bs)
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
#' bs1 <- bs
#' elementIds(bs1) <- paste0("gene", seq_len(nElements(bs)))
#' setIds(bs1) <- paste0("geneset", seq_len(nSets(bs)))
#'
setClass(
    "BaseSets",
    slots=c(
        relations="Hits",
        elementData="IdVector",
        setData="IdVector"
        ),
    prototype=list(
        relations=Hits(),
        elementData=IdVector(),
        setData=IdVector()
        )
)

#' @importFrom methods slot
setValidity("BaseSets", function(object) {

    errors <- c()

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
BaseSets <- function(relations=DataFrame(), elementData, setData) {
    relations <- as(relations, "DataFrame")
    if (!identical(colnames(relations), c("element", "set"))){
        stop('colnames(relations) must be c("element", "set")')
    }

    # Add missing metadata
    if (missing(elementData)) {
        elementData <- IdVector(sort(unique(relations$element)))
        elementMetadata(elementData) <- DataFrame(row.names=id(elementData))
    }
    if (missing(setData)) {
        setData <- IdVector(sort(unique(relations$set)))
        elementMetadata(setData) <- DataFrame(row.names=id(setData))
    }

    # Drop metadata for elements and sets not represented in relations
    elementKeep <- (id(elementData) %in% relations$element)
    if (!all(elementKeep)) {
        message("Dropping elementData missing from relations$element")
        elementData <- elementData[elementKeep]
    }
    setKeep <- (id(setData) %in% relations$set)
    if (!all(setKeep)) {
        message("Dropping setData missing from relations$set")
        setData <- setData[setKeep]
    }

    elementIdx <- match(relations$element, id(elementData))
    if (any(is.na(elementIdx))) {
        stop("relations$element missing from id(elementData)")
    }
    setIdx <- match(relations$set, id(setData))
    if (any(is.na(setIdx))) {
        stop("relations$set missing from id(setData)")
    }

    h <- Hits(
        from=elementIdx,
        to=setIdx,
        nLnode=length(elementData),
        nRnode=length(setData))

    new("BaseSets", relations=h, elementData=elementData, setData=setData)
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

    if (!identical(length(slot.membership), length(slot.relations))) {
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

#' @rdname IdVector-class
#' @export
#' @exportClass EntrezIdVector
#'
#' @seealso [`IdVector`]
#'
#' @examples
#' # EntrezIdVector ----
#'
#' library(org.Hs.eg.db)
#' ev <- EntrezIdVector(keys(org.Hs.eg.db))
#'
setClass("EntrezIdVector",
         contains="IdVector"
)

#' @rdname IdVector-class
#' @aliases EntrezIdVector
#' @export
EntrezIdVector <- function(id) {
    # Pass basic arguments to IdVector constructor
    iv <- IdVector(id)
    iv <- new("EntrezIdVector", iv)
    iv
}
