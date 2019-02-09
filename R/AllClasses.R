#' IdVector Class
#'
#' The `IdVector` class extends the [`Vector`][Vector-class] class to implement a container that hold a vector of character identifiers.
#' Subclasses of `IdVector` may be defined to enable method dispatch according to the nature of the identifiers (e.g., ENTREZ gene, Gene Ontology term).
#'
#' @slot ids character. Identifiers.
#'
#' @export
#' @exportClass IdVector
#' @importClassesFrom S4Vectors Vector
#'
#' @seealso [`Vector`][Vector-class]
#'
#' @examples
#' # Constructor ----
#'
#' iv <- IdVector(ids=head(LETTERS, 6))
#' mcols(iv) <- DataFrame(row.names = ids(iv), field1=runif(length(iv)))
#' iv
#'
#' # Subsetting ----
#'
#' iv[1:5]
#'
#' # Identifiers/Names ----
#'
#' ids(iv)
#' names(iv)
#'
#' # EntrezIdVector ----
#'
#' library(org.Hs.eg.db)
#' eiv <- EntrezIdVector(keys(org.Hs.eg.db))
#' eiv
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
#' @importMethodsFrom S4Vectors parallelSlotNames
setMethod("parallelSlotNames", "IdVector", function(x) {
    c("ids", callNextMethod())
})

#' @name IdVector-class
#' @rdname IdVector-class
#' @aliases IdVector
#'
#' @param ids character. Identifiers.
#'
#' @return An `IdVector` object.
#'
#' @author Kevin Rue-Albrecht
#'
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
#' @slot relations [`Hits-class`]
#' The _left node_ and _right node_ of each hit stores the index of the `element` and `set` in `elementData` and `setData`, respectively.
#' Metadata for each relation is stored as `mcols(object@relations)`.
#' @slot elementData [`IdVector`].
#' Metadata for each unique element in `relations$element` is stored as `mcols(elementData)`.
#' @slot setData [`IdVector`].
#' Metadata for each unique set in `relations$set` is stored as `mcols(setData)`.
#'
#' @export
#' @exportClass BaseSets
#' @importClassesFrom S4Vectors Hits
#' @importFrom S4Vectors Hits
#'
#' @seealso BaseSets-methods
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
#' bs <- as(sets, "BaseSets")
#'
#' # Coercing ----
#'
#' # to list (gene sets)
#' ls1 <- as(bs, "list")
#' # to matrix (logical membership)
#' m1 <- as(bs, "matrix")
#'
#' # Accessors ----
#'
#' relations(bs)
#' elementData(bs)
#' setData(bs)
#'
#' # Dimensions ----
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
#' bs1
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

#' @name BaseSets-class
#' @rdname BaseSets-class
#' @aliases BaseSets
#'
#' @param relations [`DataFrame-class`].
#' At least two columns that provide mapping relationships between `"element"` and `"set"`.
#' Additional columns are taken as relation metadata.
#' @param elementData [`IdVector`].
#' Metadata for each unique element in `relations$element` is provided as `mcols(elementData)`.
#' @param setData [`IdVector`].
#' Metadata for each unique set in `relations$set` is provided as `mcols(setData)`.
#'
#' @return A `BaseSets` object.
#'
#' @author Kevin Rue-Albrecht
#'
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
        elementData <- IdVector(unique(as.character(relations$element)))
    }
    if (missing(setData)) {
        setData <- IdVector(unique(as.character(relations$set)))
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
#' The `FuzzyHits` class extends the [`Hits`][Hits-class] class to represent hits that are associated with different grades of membership in the interval `[0,1]`.
#'
#' This class does not define any additional slot to the `Hits` class.
#' However, this class defines additional validity checks to ensure that every relation stored in a `FuzzyHits` are associated with a numeric membership funtion in the interval `[0,1]`.
#'
#' @export
#' @exportClass FuzzyHits
#' @importClassesFrom S4Vectors Hits
#'
#' @seealso [`Hits`][Hits-class], [`FuzzySets`][FuzzySets-class]
#'
#' @examples
#' # Constructor ----
#'
#' from <- c(5, 2, 3, 3, 3, 2)
#' to <- c(11, 15, 5, 4, 5, 11)
#' membership <- c(0, 0.1, 0.2, 0.3, 0.6, 0.8)
#'
#' fh <- FuzzyHits(from, to, membership, 7, 15)
#' fh
setClass("FuzzyHits",
    contains="Hits"
)

#' @name FuzzyHits-class
#' @rdname FuzzyHits-class
#' @aliases FuzzyHits
#'
#' @param from,to Two integer vectors of the same length.
#' The values in `from` must be >= 1 and <= `nLnode`.
#' The values in `to` must be >= 1 and <= `nRnode`.
#' @param membership Numeric. Vector of numeric membership function in the range `[0,1]`
#' @param nLnode,nRnode Number of left and right nodes.
#' @param ... Arguments metadata columns to set on the `FuzzyHits` object.
#' All the metadata columns must be vector-like objects of the same length as `from`, `to`, and `membership`.
#'
#' @return A `FuzzyHits` object.
#'
#' @author Kevin Rue-Albrecht
#'
#' @export
#' @importFrom methods new
#' @importFrom S4Vectors Hits
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
#' The `FuzzySets` class extends the [`BaseSets`] class to implement a container that also describe different grades of membership in the interval `[0,1]`.
#'
#' This class does not define any additional slot to the `BaseSets` class.
#' However, this class defines additional validity checks to ensure that every relation stored in a `FuzzySets` are associated with a numeric membership funtion in the interval `[0,1]`.
#'
#' @export
#' @exportClass FuzzySets
#'
#' @seealso [`BaseSets`][BaseSets-class], [`FuzzyHits`][FuzzyHits-class], [`FuzzySets-methods`].
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
#' m1 <- as(fs, "matrix")
#' # to matrix (multiple observations)
#' mm1 <- as.matrix(fs, fun.aggregate=min)
#'
#' # Getters/Setters ----
#'
#' membership(fs)
#'
#' fs1 <- fs
#' membership(fs1) <- runif(length(fs1))
setClass("FuzzySets",
    slots=c(
        relations="FuzzyHits"
    ),
    prototype=list(
        relations=FuzzyHits()
    ),
    contains="BaseSets"
)

#' @name FuzzySets-class
#' @rdname FuzzySets-class
#' @aliases FuzzySets
#'
#' @param relations [`DataFrame-class`].
#' At least 3 columns that provide mapping relationships between `"element"` and `"set"`, with `"membership"` function in the range `[0,1]`.
#' Additional columns are taken as relation metadata.
#' @param ... Arguments passed to the [`BaseSets()`] constructor and other functions.
#'
#' @return A `FuzzySets` object.
#'
#' @author Kevin Rue-Albrecht
#'
#' @export
#' @importFrom methods new
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
#'
#' @export
#' @exportClass EntrezIdVector
#'
#' @examples
#' # EntrezIdVector ----
#'
#' library(org.Hs.eg.db)
#' eiv <- EntrezIdVector(keys(org.Hs.eg.db))
#' eiv
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
