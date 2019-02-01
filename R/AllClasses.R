#' IdVector Class
#'
#' The `IdVector` class extends the [`Vector`] class to implement a container that hold a vector of Entrez gene character identifiers.
#'
#' @slot ids character. Entrez gene identifiers.
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
#' tv <- IdVector(ids=head(LETTERS, 6))
#' mcols(tv) <- DataFrame(row.names = ids(tv), field1=runif(length(tv)))
#'
#' # Subsetting ----
#'
#' tv1 <- tv[1:5]
#'
setClass("IdVector",
    contains="Vector",
    slots=c(
        ids="character"
    ),
    prototype= list(
        ids=character(0)
    )
)

#' @importFrom methods callNextMethod
setMethod("parallelSlotNames", "IdVector", function(x) {
    c("ids", callNextMethod())
})

#' @importFrom methods slot
setValidity("IdVector", function(object) {

    errors <- c()

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})

#' @param ids character. Entrez gene identifiers.
#'
#' @rdname IdVector-class
#' @aliases IdVector
#' @export
#' @importFrom methods new
IdVector <- function(ids=character(0)) {
    # Drop names if present
    if (!is.null(names(ids))) {
        message("Setting names(ids) to NULL")
        names(ids) <- NULL
    }

    new("IdVector", ids=ids)
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
#' length(bs)
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
setClass("BaseSets",
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
BaseSets <- function(
    relations=DataFrame(element=character(0), set=character(0)),
    elementData, setData
) {
    relations <- as(relations, "DataFrame")

    if (!is.null(rownames(relations))) {
        message("Setting rownames(relations) to NULL")
        rownames(relations) <- NULL
    }

    protectedFields <- c("element", "set")

    if (!all(protectedFields %in% colnames(relations))){
        stop('colnames(relations) must include c("element", "set")')
    }

    extraFields <- setdiff(colnames(relations), protectedFields)

    # Add missing metadata
    if (missing(elementData)) {
        elementData <- IdVector(sort(unique(relations$element)))
    }
    if (missing(setData)) {
        setData <- IdVector(sort(unique(relations$set)))
    }
    # Add missing mcols
    if (is.null(mcols(elementData))) {
        mcols(elementData) <- DataFrame(row.names=ids(elementData))
    }
    if (is.null(mcols(setData))) {
        mcols(setData) <- DataFrame(row.names=ids(setData))
    }

    # Drop metadata for elements and sets not represented in relations
    elementKeep <- (ids(elementData) %in% relations$element)
    if (!all(elementKeep)) {
        message("Dropping elementData missing from relations$element")
        elementData <- elementData[elementKeep]
    }
    setKeep <- (ids(setData) %in% relations$set)
    if (!all(setKeep)) {
        message("Dropping setData missing from relations$set")
        setData <- setData[setKeep]
    }

    elementIdx <- match(relations$element, ids(elementData))
    if (any(is.na(elementIdx))) {
        stop("relations$element missing from ids(elementData)")
    }
    setIdx <- match(relations$set, ids(setData))
    if (any(is.na(setIdx))) {
        stop("relations$set missing from ids(setData)")
    }

    h <- Hits(
        from=elementIdx,
        to=setIdx,
        nLnode=length(elementData),
        nRnode=length(setData))
    mcols(h) <- relations[, extraFields, drop=FALSE]

    new("BaseSets", relations=h, elementData=elementData, setData=setData)
}

#' FuzzyHits Class
#'
#' The `FuzzyHits` class extends the [`Hits`] class to represent hits that are associated with different grades of membershipin the interval `[0,1]`.
#'
#' @return A `FuzzyHits` object.
#' @export
#' @exportClass FuzzyHits
#'
#' @seealso [`Hits`], [`FuzzySets`]
#'
#' @examples
#' # Constructor ----
#'
#' from <- c(5, 2, 3, 3, 3, 2)
#' to <- c(11, 15, 5, 4, 5, 11)
#' membership <- c(0, 0.1, 0.2, 0.3, 0.6, 0.8)
#'
#' fh <- FuzzyHits(from, to, membership, 7, 15)
#'
setClass("FuzzyHits",
    contains="Hits"
)

#' @importFrom methods slot
setValidity("FuzzyHits", function(object) {
    errors <- c()

    if (! "membership" %in% colnames(mcols(object))) {
        error <- "membership column missing in mcols(object)"
        return(error)
    }

    membership <- mcols(object)[["membership"]]

    if (!is.numeric(membership)) {
        error <- "membership function must be numeric"
        return(error)
    }

    if (any(is.na(membership) | membership < 0 | membership > 1)) {
        error <- "membership function must be in the interval [0,1]"
        errors <- c(errors, error)
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})

#' @param from,to Two integer vectors of the same length.
#' The values in `from` must be >= 1 and <= `nLnode`.
#' The values in `to` must be >= 1 and <= `nRnode`.
#' @param membership Numeric. Vector of membership in the range `[0,1]`
#' @param nLnode,nRnode Number of left and right nodes.
#' @param ... Arguments metadata columns to set on the `FuzzyHits` object.
#' All the metadata columns must be vector-like objects of the same length as `from`, `to`, and `membership`.
#'
#' @rdname FuzzyHits-class
#' @aliases FuzzyHits
#' @export
#' @importFrom methods new
FuzzyHits <- function(
    from=integer(0), to=integer(0), membership=numeric(0), nLnode=0L, nRnode=0L,...
) {
    # Drop names if present
    if (!is.null(names(membership))) {
        message("Setting names(membership) to NULL")
        names(membership) <- NULL
    }
    # Pass basic arguments to BaseSets constructor
    fh <- Hits(from, to, nLnode, nRnode, membership=membership, ...)
    fh <- as(fh, "FuzzyHits")
    fh
}

#' FuzzySets Class
#'
#' The `FuzzySets` class extends the [`BaseSets`] class to implement a container that also describe different grades of membershipin the interval `[0,1]`.
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
#' # Visually intuitive definition of sets, elements, and membership
#' sets <- list(
#'   set1=c("A"=0.1, "B"=0.2),
#'   set2=c("B"=0.3, "C"=0.4, "D"=0.5),
#'   set3=c("E"=0.8))
#'
#' # unlist the set names
#' unlistSets <- rep(names(sets), lengths(sets))
#' # unlist the element names
#' unlistElements <- unlist(sapply(sets, names))
#' # unlist the membership values
#' unlistMembership <- unlist(sets)
#'
#' # Reformat as a table
#' relations <- DataFrame(
#'   element=unlistElements,
#'   set=unlistSets,
#'   membership=unlistMembership
#' )
#'
#' fs <- FuzzySets(relations=relations)
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
#' membership(fs1) <- runif(length(fs1))
#'
setClass("FuzzySets",
    slots=c(
        relations="FuzzyHits"
    ),
    prototype=list(
        relations=FuzzyHits()
    ),
    contains="BaseSets"
)

#' @importFrom methods slot
setValidity("FuzzySets", function(object) {

    errors <- c()

    if (!all(c("membership") %in% colnames(relations(object)))){
        error <- 'colnames(relations(object)) must include c("membership")'
        return(error)
    }


    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
})

#' @param relations DataFrame. At least 3 columns provide mapping relationships between `"element"` and `"set"` with `"membership"` function in the range `[0,1]`.
#' @param ... Arguments passed to the [`BaseSets()`] constructor and other functions.
#'
#' @rdname FuzzySets-class
#' @aliases FuzzySets
#' @export
#' @importFrom methods new
#'
#' @seealso [`BaseSets`]
#'
FuzzySets <- function(
    relations=DataFrame(element=character(0), set=character(0), membership=numeric(0)),
    ...
) {
    if (!"membership" %in% colnames(relations)) {
        stop('colnames(relations) must include c("membership")')
    }

    # Pass basic arguments to BaseSets constructor
    object <- BaseSets(relations, ...)

    # Coerce to FuzzySets
    object <- as(object, "FuzzySets")
    object
}

#' @rdname IdVector-class
#' @export
#' @exportClass EntrezIdVector
#'
#' @seealso [`IdVector`]
#'
#' @examples
#' # Constructor ----
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
EntrezIdVector <- function(ids) {
    # Pass basic arguments to IdVector constructor
    iv <- IdVector(ids)
    iv <- new("EntrezIdVector", iv)
    iv
}
