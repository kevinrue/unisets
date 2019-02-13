
# Setup ----

sets <- list(
    set1=c("A", "B"),
    set2=c("B", "C", "D"),
    set3=c("E")
)

# prepare named vectors to check that names are dropped by the constructor
elementsUnlist <- unlist(sets)
setsUnlist <- rep(names(sets), lengths(sets))
names(setsUnlist) <- paste0("x", seq_along(setsUnlist))

relations <- DataFrame(element=elementsUnlist, set=setsUnlist)

# BaseSets() ----

test_that("BaseSets constructor produces valid objects", {

    out <- BaseSets(relations)

    expect_s4_class(out, "BaseSets")

})

test_that("BaseSets validity method identifies issues", {

    # Invalid colnames(object@relations)
    map0 <- relations
    colnames(map0) <- c("A", "B")
    expect_error(
        BaseSets(relations=map0),
        "colnames(relations) must include c(\"element\", \"set\")",
        fixed=TRUE
    )

    # Mismatch between elements and elementData
    expect_error(
        BaseSets(relations, elementData=IdVector("Z")),
        "relations$element missing from ids(elementData)",
        fixed=TRUE
    )

    expect_error(
        BaseSets(relations, setData=IdVector("set999")),
        "relations$set missing from ids(setData)",
        fixed=TRUE
    )

    expect_message(
        BaseSets(relations, elementData=IdVector(unique(c(relations$element, "Z")))),
        "Dropping elementData missing from relations$element",
        fixed=TRUE
    )

    expect_message(
        BaseSets(relations, setData=IdVector(unique(c(relations$set, "set999")))),
        "Dropping setData missing from relations$set",
        fixed=TRUE
    )

    expect_error(
        BaseSets(relations, elementData=IdVector(relations$element)),
        "duplicated values in ids(elementData(object))",
        fixed=TRUE
    )

    expect_error(
        BaseSets(relations, setData=IdVector(relations$set)),
        "duplicated values in ids(setData(object))",
        fixed=TRUE
    )

    # Provide metadata columns without rownames set
    relations0 <- DataFrame(element="element1", set="set1")
    elementData=IdVector("element1")
    mcols(elementData) <- DataFrame(field="elementValue")
    setData=IdVector("set1")
    mcols(setData) <- DataFrame(field="setValue")

    # Check that rownames(mcols(x)) is not NULL (using the default use.names=TRUE)
    # while the actual DataFrame does not store rownames
    out <- BaseSets(relations0, elementData, setData)
    expect_true(!is.null(rownames(mcols(elementData(out)))))
    expect_null(rownames(out@elementData@elementMetadata))
    expect_true(!is.null(rownames(mcols(setData(out)))))
    expect_null(rownames(out@setData@elementMetadata))

})

# relations() ----

test_that("relations(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- relations(bs)
    expect_s4_class(out, "Hits")

})

# length() ----

test_that("length(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- length(bs)
    expect_identical(out, 6L)

})

# elements() ----

test_that("elements(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- elements(bs)
    expect_identical(out, bs@elementData[bs@relations@from])

})

# nElements() ----

test_that("nElements(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- nElements(bs)
    expect_identical(out, 5L)

})

# sets() ----

test_that("elements(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- sets(bs)
    expect_identical(out, bs@setData[bs@relations@to])

})

# nSets() ----

test_that("nSets(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- nSets(bs)
    expect_identical(out, 3L)

})

# elementData() ----

test_that("ids(elementData(BaseSets)) works", {

    bs <- BaseSets(relations)

    out <- elementData(bs)
    expect_s4_class(out, "IdVector")

    out <- ids(elementData(bs))
    expect_identical(out, c("A", "B", "C", "D", "E"))

})

# elementData<-() ----

test_that("elementData(BaseSets) <- value works", {

    bs <- BaseSets(relations)

    ids(elementData(bs)) <- tail(LETTERS, nElements(bs))

    expect_identical(ids(elementData(bs)), tail(LETTERS, nElements(bs)))

})

# setData() ----

test_that("setData(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- setData(bs)
    expect_s4_class(out, "IdVector")

    out <- ids(setData(bs))
    expect_identical(out, c("set1", "set2", "set3"))

})

# setData<-() ----

test_that("setData(BaseSets) <- value works", {

    bs <- BaseSets(relations)

    ids(setData(bs)) <- paste0("geneset", seq_len(nSets(bs)))

    expect_identical(ids(setData(bs)), paste0("geneset", seq_len(nSets(bs))))

})

# subset() ----

test_that("subset(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- subset(bs, set == "set1")

    expect_true(all(ids(setData(out)) == "set1"))
    expect_identical(length(setData(out)), 1L)

    out <- subset.BaseSets(bs, set == "set1")

    expect_true(all(ids(setData(out)) == "set1"))
    expect_identical(length(setData(out)), 1L)

    out <- bs[1:3]
    expect_identical(length(out), 3L)

})

# duplicated() ----

test_that("duplicated(BaseSets) works", {

    bs <- BaseSets(relations)
    bs1 <- bs
    bs1@relations <- rep(bs1@relations, times=2)

    out <- duplicated(bs1)
    expect_identical(out, rep(c(FALSE, TRUE), each=length(bs)))
})

# unique() ----

test_that("unique(BaseSets) works", {

    bs <- BaseSets(relations)
    bs1 <- bs
    bs1@relations <- rep(bs1@relations, times=2)

    out <- unique(bs1)
    expect_identical(out, bs)
})

# show() ----

test_that("show(BaseSets) works", {

    # Small objects fully displayed
    bs <- BaseSets(relations)

    out <- show(bs)
    expect_identical(colnames(out), c("element", "set", "relationData", "elementData", "setData"))
    expect_identical(nrow(out), length(bs)+1L)

    # Large objects partially displayed
    bs <- BaseSets(relations=DataFrame(element=letters, set=LETTERS))

    out <- show(bs)
    expect_identical(colnames(out), c("element", "set", "relationData", "elementData", "setData"))
    expect_identical(nrow(out), unisets:::get_showHeadLines() + unisets:::get_showTailLines() + 2L)

})

# as.DataFrame() ----

test_that("as(BaseSets, \"DataFrame\") works", {

    bs <- BaseSets(relations)

    out <- as(bs, "DataFrame")
    expect_s4_class(out, "DataFrame")
    expect_identical(colnames(out), c("element", "set", "relationData", "elementData", "setData"))
    expect_identical(nrow(out), nrow(relations))

    out <- as.DataFrame.BaseSets(bs)
    expect_s4_class(out, "DataFrame")
    expect_identical(colnames(out), c("element", "set", "relationData", "elementData", "setData"))
    expect_identical(nrow(out), nrow(relations))

})

# as.data.frame() ----

test_that("as(BaseSets, \"data.frame\") works", {

    bs <- BaseSets(relations)

    out <- as(bs, "data.frame")
    expect_identical(colnames(out), c("element", "set"))
    expect_identical(dim(out), dim(relations))

    out <- as.data.frame.BaseSets(bs)
    expect_identical(colnames(out), c("element", "set"))
    expect_identical(dim(out), dim(relations))

})

# as.list() ----

test_that("as(BaseSets, \"list\") works", {

    bs <- BaseSets(relations)

    out <- as(bs, "list")
    expect_identical(lengths(out), c(set1 = 2L, set2 = 3L, set3 = 1L))
    out <- as.list(bs)
    expect_identical(lengths(out), c(set1 = 2L, set2 = 3L, set3 = 1L))

})

# as.matrix() ----

test_that("as(BaseSets, \"matrix\") works", {

    bs <- BaseSets(relations)

    expected.dim <- c(nElements(bs), nSets(bs))

    out <- as(bs, "matrix")
    expect_type(out, "logical")
    expect_identical(dim(out), expected.dim)

    out <- as.matrix(bs)
    expect_type(out, "logical")
    expect_identical(dim(out), expected.dim)

})

# as(list, "BaseSets") ----

test_that("as(list, \"BaseSets\") works", {

    bl <- list(set1=c("A", "B"), set2=c("B", "C"))

    out <- as(bl, "BaseSets")
    expect_s4_class(out, "BaseSets")
    expect_identical(length(out@relations), 4L)

    out <- as.BaseSets.list(bl, "BaseSets")
    expect_s4_class(out, "BaseSets")
    expect_identical(length(out@relations), 4L)

})

# as(matrix, "BaseSets") ----

test_that("as(matrix, \"BaseSets\") works", {

    nGenes <- 3
    nSets <- 2
    bm <- matrix(
        rep(c(TRUE, FALSE), nGenes),
        nrow=nGenes, ncol=nSets,
        dimnames=list(
            gene = paste0("gene", seq_len(nGenes)),
            set  = paste0("set", seq_len(nSets))
            )
        )

    out <- as(bm, "BaseSets")
    expect_s4_class(out, "BaseSets")
    expect_identical(length(out@relations), 3L) # 3 TRUE values above

    out <- as.BaseSets.matrix(bm, "BaseSets")
    expect_s4_class(out, "BaseSets")
    expect_identical(length(out@relations), 3L) # 3 TRUE values above

})

# setLengths() ----

test_that("setLengths(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- setLengths(bs)
    expect_identical(out, c(set1 = 2L, set2 = 3L, set3 = 1L))

})

# elementLengths() ----

test_that("elementLengths(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- elementLengths(bs)
    expect_identical(out, c(A = 1L, B = 2L, C = 1L, D = 1L, E = 1L))

})

