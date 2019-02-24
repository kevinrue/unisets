
#' Check Presence of Required Metadata Columns
#'
#' These function throws an error if required columns are not present in the metadata columns of the `relations` slot.
#' `.requireRelationsColnames` returns an error message referring to the `DataFrame` input for constructor functions.
#' `.requireRelationsMetadataColnames` returns an error message referring to the `Hits` object stored in the `relations` slot.
#'
#' @rdname INTERNAL_checkRelationMetadata
#'
#' @param required Character vector of required column names.
#' @param present Character vector of column names present.
#'
#' @return Those functions are called only for their by-product: an error thrown if any of the required column names is not found.
#'
#' @author Kevin Rue-Albrecht
.requireRelationsColnames <- function(required, present) {
    for (field in required) {
        if (! field %in% present) {
            stop(sprintf('colnames(relations) must include "%s"', field))
        }
    }
}

#' @rdname INTERNAL_checkRelationMetadata
#' @aliases INTERNAL_requireRelationsMetadataColnames
.requireRelationsMetadataColnames <- function(required, present) {
    for (field in required) {
        if (! field %in% present) {
            stop(sprintf('colnames(mcols(relations)) must include "%s"', field))
        }
    }
}

# IdVector ----

#' IdVector Class
#'
#' The `IdVector` class extends the [`Vector-class`] class to implement a container that hold a vector of character identifiers.
#' Subclasses of `IdVector` may be defined to enable method dispatch according to the nature of the identifiers (e.g., ENTREZ gene, Gene Ontology term).
#'
#' @slot ids character. Identifiers.
#'
#' @export
#' @exportClass IdVector
#' @importClassesFrom S4Vectors Vector
#'
#' @seealso [`Vector-class`], [`EntrezIdVector-class`], [`GOIdVector-class`]
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

# BaseSets ----

#' BaseSets Class
#'
#' The `BaseSets` class implements a container to describe distinct objects that make up sets, along with element metadata and set metadata.
#'
#' @slot relations [`Hits-class`]
#' The _left node_ and _right node_ of each hit stores the index of the `element` and `set` in `elementData` and `setData`, respectively.
#' Metadata for each relation is stored as `mcols(relations(object))`.
#' @slot elementData [`IdVector-class`].
#' Metadata for each unique element in `relations$element` is stored as `mcols(elementData)`.
#' @slot setData [`IdVector-class`].
#' Metadata for each unique set in `relations$set` is stored as `mcols(setData)`.
#'
#' @export
#' @exportClass BaseSets
#' @importClassesFrom S4Vectors Hits
#' @importFrom S4Vectors Hits
#'
#' @seealso [`BaseSets-methods`].
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
#' bs
#'
#' # Coercing ----
#'
#' # to list (gene sets)
#' ls1 <- as(bs, "list")
#' ls1
#' # to matrix (logical membership)
#' m1 <- as(bs, "matrix")
#' m1
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
#'
#' setLengths(bs)
#' elementLengths(bs)
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
#' At least two columns that provide mapping relationships between `"element"` and `"set"` identifiers.
#' Additional columns are taken as relation metadata.
#' @param elementData [`IdVector`].
#' Metadata for each unique identifier in `relations$element` is provided as `mcols(elementData)`.
#' @param setData [`IdVector`].
#' Metadata for each unique identifier in `relations$set` is provided as `mcols(setData)`.
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

    elementIdx <- match(as.character(relations$element), ids(elementData))
    if (any(is.na(elementIdx))) {
        stop("relations$element missing from ids(elementData)")
    }
    setIdx <- match(as.character(relations$set), ids(setData))
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

# FuzzyHits ----

#' FuzzyHits Class
#'
#' The `FuzzyHits` class extends the [`Hits-class`] class to represent hits that are associated with different grades of membership in the interval `[0,1]`.
#'
#' This class does not define any additional slot to the `Hits` class.
#' However, this class defines additional validity checks to ensure that every relation stored in a `FuzzyHits` are associated with a numeric membership funtion in the interval `[0,1]`.
#'
#' @export
#' @exportClass FuzzyHits
#' @importClassesFrom S4Vectors Hits
#'
#' @seealso [`Hits-class`], [`FuzzySets-class`].
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

# FuzzySets ----

#' FuzzySets Class
#'
#' The `FuzzySets` class extends the [`BaseSets-class`] class to implement a container that also describe different grades of membership in the interval `[0,1]`.
#'
#' This class does not define any additional slot to the `BaseSets` class.
#' However, this class defines additional validity checks to ensure that every relation stored in a `FuzzySets` are associated with a numeric membership funtion in the interval `[0,1]`.
#'
#' @export
#' @exportClass FuzzySets
#'
#' @seealso [`BaseSets-class`], [`FuzzyHits-class`], [`FuzzySets-methods`].
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
#' At least 3 columns that provide mapping relationships between `"element"` and `"set"` identifiers, with `"membership"` function in the range `[0,1]`.
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
    protectedRelationMetadata <- c("membership")
    .requireRelationsColnames(protectedRelationMetadata, colnames(relations))

    # Pass basic arguments to BaseSets constructor
    object <- BaseSets(relations, ...)

    # Coerce to FuzzySets
    object <- as(object, "FuzzySets")
    object
}

# EntrezIdVector ----

#' @rdname IdVector-class
#' @aliases EntrezIdVector-class
#'
#' @export
#' @exportClass EntrezIdVector
#'
#' @examples
#'
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

# GOIdVector ----

#' @rdname IdVector-class
#' @aliases GOIdVector-class
#'
#' @export
#' @exportClass GOIdVector
#'
#' @examples
#'
#' # GOIdVector ----
#'
#' library(org.Hs.eg.db)
#' giv <- GOIdVector(keys(org.Hs.eg.db, keytype = "GO"))
#' giv
setClass("GOIdVector",
    contains="IdVector"
)

#' @rdname IdVector-class
#' @aliases GOIdVector
#' @export
GOIdVector <- function(ids) {
    # Pass basic arguments to IdVector constructor
    giv <- IdVector(ids)
    giv <- new("GOIdVector", giv)
    giv
}

# GOHits ----

#' @name GOHits-class
#' @rdname GOHits-class
#' @aliases GOEvidenceCodes
#'
#' @section Controlled vocabulary:
#' Gene Ontology evidence codes were obtained from <>
#'
#' @export
#'
#' @format A named vector of length 26.
#' \describe{
#'   \item{names}{Code.}
#'   \item{carat}{Description.}
#'   ...
#' }
#' @source <http://geneontology.org/docs/guide-go-evidence-codes/>
#'
#' @examples
#' # Controlled vocabulary ----
#'
#' GOEvidenceCodes
GOEvidenceCodes <- c(
    "EXP"="Inferred from Experiment",
    "IDA"="Inferred from Direct Assay",
    "IPI"="Inferred from Physical Interaction",
    "IMP"="Inferred from Mutant Phenotype",
    "IGI"="Inferred from Genetic Interaction",
    "IEP"="Inferred from Expression Pattern",
    "HTP"="Inferred from High Throughput Experiment",
    "HDA"="Inferred from High Throughput Direct Assay",
    "HMP"="Inferred from High Throughput Mutant Phenotype",
    "HGI"="Inferred from High Throughput Genetic Interaction",
    "HEP"="Inferred from High Throughput Expression Pattern",
    "IBA"="Inferred from Biological characteristic of Ancestor",
    "IBD"="Inferred from Biological characteristic of Descendant",
    "IKR"="Inferred loss due to absence of Key Residues",
    "IRD"="Inferred loss after Rapid Divergence",
    "ISS"="Inferred from Sequence or structural Similarity",
    "ISO"="Inferred from Sequence Orthology",
    "ISA"="Inferred from Sequence Alignment",
    "ISM"="Inferred from Sequence Model",
    "IGC"="Inferred from Genomic Context",
    "RCA"="Inferred from Reviewed Computational Analysis",
    "TAS"="Traceable Author Statement",
    "NAS"="Non-traceable Author Statement",
    "IC"="Inferred by Curator",
    "ND"="No biological Data available",
    "IEA"="Inferred from Electronic Annotation"
)

#' @name GOHits-class
#' @rdname GOHits-class
#' @aliases GOOntologyCodes
#'
#' @section Controlled vocabulary:
#' Gene Ontology namespaces were obtained from <http://geneontology.org/docs/ontology-documentation/>
#'
#' @export
#'
#' @format A named vector of length 3.
#' \describe{
#'   \item{names}{Code.}
#'   \item{carat}{Description.}
#'   ...
#' }
#' @source <http://geneontology.org/docs/guide-go-evidence-codes/>
#'
#' @examples
#' GOOntologyCodes
GOOntologyCodes <- c(
    "BP"="Biological Process",
    "MF"="Molecular Function",
    "CC"="Cellular Component"
)

#' GOHits Class
#'
#' The `GOHits` class extends the [`Hits-class`] class to represent hits that also describe relations between genes and sets using the Gene Ontology controlled vocabulary.
#'
#' This class does not define any additional slot to the `Hits` class.
#' However, this class defines additional validity checks to ensure that every relation stored in a `GOHits` are respect the Gene Ontology evidence and ontology codes.
#' Refer to [`GOOntologyCodes`] and [`GOEvidenceCodes`] for valid code and vocabulary.
#'
#' @export
#' @exportClass GOHits
#' @importClassesFrom S4Vectors Hits
#'
#' @seealso [`Hits-class`], [`FuzzySets-class`]
#'
#' @examples
#'
#' # Constructor ----
#'
#' from <- c(5, 2, 3, 3, 3, 2)
#' to <- c(11, 15, 5, 4, 5, 11)
#' ontology <- factor(c("BP", "BP", "BP", "MF", "MF", "CC"))
#' evidence <- factor(c("IEA", "IDA", "IEA", "IDA", "IEA", "IDA"))
#'
#' gh <- GOHits(from, to, evidence, ontology, 7, 15)
#' gh
setClass("GOHits",
    contains="Hits"
)

#' @name GOHits-class
#' @rdname GOHits-class
#' @aliases GOHits
#'
#' @param from,to Two integer vectors of the same length.
#' The values in `from` must be >= 1 and <= `nLnode`.
#' The values in `to` must be >= 1 and <= `nRnode`.
#' @param evidence factor. Levels must be values in `names(GOEvidenceCodes)`.
#' @param ontology factor. Levels must be values in `names(GOOntologyCodes)`.
#' @param nLnode,nRnode Number of left and right nodes.
#' @param ... Arguments metadata columns to set on the `GOHits` object.
#' All the metadata columns must be vector-like objects of the same length as `from`, `to`, and `membership`.
#'
#' @return A `GOHits` object.
#'
#' @author Kevin Rue-Albrecht
#'
#' @export
#' @importFrom methods new
#' @importFrom S4Vectors Hits
GOHits <- function(
    from=integer(0), to=integer(0),
    evidence=factor(character(0), names(GOEvidenceCodes)),
    ontology=factor(character(0), names(GOOntologyCodes)),
    nLnode=0L, nRnode=0L,...
) {
    # Drop names if present
    if (!is.null(names(ontology))) {
        message("Setting names(ontology) to NULL")
        names(ontology) <- NULL
    }
    if (!is.null(names(evidence))) {
        message("Setting names(evidence) to NULL")
        names(evidence) <- NULL
    }
    # Pass basic arguments to BaseSets constructor
    gh <- Hits(from, to, nLnode, nRnode, ontology=ontology, evidence=evidence, ...)
    gh <- as(gh, "GOHits")
    gh
}

# GOSets ----

#' GOSets Class
#'
#' The `GOSets` class extends the [`BaseSets-class`] class to implement a container that also describes relations between genes and sets using the Gene Ontology controlled vocabulary.
#' Refer to [`GOOntologyCodes`] and [`GOEvidenceCodes`] for valid vocabulary.
#'
#' @export
#' @exportClass GOSets
#'
#' @seealso [`BaseSets-class`], [`GOHits-class`], [`GOSets-methods`].
#'
#' @examples
#' # Constructor ----
#'
#' # Fetch a sample of GO annotations
#' library(org.Hs.eg.db)
#' base_sets <- import(org.Hs.egGO)
#' relations <- as.data.frame(head(base_sets))
#'
#' gs <- GOSets(relations)
#'
#' # Subsetting ----
#'
#' gs1 <- subset(gs, element == "1" & ontology == "BP" & evidence == "TAS")
#' relations(gs1)
#'
#' # Getters/Setters ----
#'
#' evidence(gs)
#' ontology(gs)
#'
#' gs1 <- gs
#' evidence(gs1)[1] <- "EXP"
#'
#' gs1 <- gs
#' ontology(gs1)[1] <- "CC"
setClass("GOSets",
    slots=c(
        relations="GOHits"
    ),
    prototype=list(
        relations=GOHits()
    ),
    contains="BaseSets"
)

#' @name GOSets-class
#' @rdname GOSets-class
#' @aliases GOSets
#'
#' @param relations [`DataFrame-class`].
#' At least 3 columns that provide mapping relationships between `"element"` and `"set"`, with `"membership"` function in the range `[0,1]`.
#' Additional columns are taken as relation metadata.
#' @param ... Arguments passed to the [`BaseSets()`] constructor and other functions.
#'
#' @return A `GOSets` object.
#'
#' @author Kevin Rue-Albrecht
#'
#' @export
#' @importFrom methods new
GOSets <- function(
    relations=DataFrame(
        element=character(0), set=character(0),
        ontology=factor(character(0), names(GOOntologyCodes)),
        evidence=factor(character(0), names(GOEvidenceCodes))
    ),
    ...
) {
    protectedRelationMetadata <- c("evidence", "ontology")
    .requireRelationsColnames(protectedRelationMetadata, colnames(relations))

    # Pass basic arguments to BaseSets constructor
    object <- BaseSets(relations, ...)

    # Coerce to GOSets
    object <- as(object, "GOSets")
    object
}
