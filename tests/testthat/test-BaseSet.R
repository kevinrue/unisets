
sets <- list(
  set1=c("A", "B"),
  set2=c("C", "D", "E")
)

# prepare named vectors to check that names are dropped by the constructor
elementsUnlist <- unlist(sets)
setsUnlist <- rep(names(sets), lengths(sets))
names(setsUnlist) <- paste0("x", seq_along(setsUnlist))

relations <- DataFrame(element=elementsUnlist, set=setsUnlist)

# BaseSets() ----

test_that("BaseSets constructor produces valid objects", {

    expect_message(BaseSets(relations), "Setting rownames(relations) to NULL", fixed=TRUE)
    expect_message(BaseSets(relations), "Setting names(relations$element) to NULL", fixed=TRUE)
    expect_message(BaseSets(relations), "Setting names(relations$set) to NULL", fixed=TRUE)

    out <- BaseSets(relations)

    expect_s4_class(out, "BaseSets")
    expect_identical(slotNames(out), c("relations", "elementData", "setData"))

    # Check that names were dropped
    slot.relations <- slot(out, "relations")
    expect_null(rownames(slot.relations))
    expect_null(names(slot.relations$element))
    expect_null(names(slot.relations$set))
})

test_that("BaseSets validity method identifies issues", {

    # Valid object
    out <- BaseSets(relations)

    # Invalid colnames(object@relations)
    map0 <- relations
    colnames(map0) <- c("A", "B")
    expect_error(
        BaseSets(map0),
        "colnames(relations(object)) must be c(\"element\", \"set\")",
        fixed=TRUE
    )

    # Mismatch between elements and  elementData
    expect_error(
        BaseSets(relations, elementData=DataFrame(row.names="Z")),
        "Mismatch between relations$element and rownames(elementData)",
        fixed=TRUE
    )

    # Mismatch between elements and  elementData
    expect_error(
        BaseSets(relations, setData=DataFrame(row.names="set999")),
        "Mismatch between relations$set and rownames(setData)",
        fixed=TRUE
    )

})

# nRelations() ----

test_that("nRelations(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- nRelations(bs)
    expect_identical(out, 5L)

})

# nElements() ----

test_that("nElements(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- nElements(bs)
    expect_identical(out, 5L)

})

# nSets() ----

test_that("nSets(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- nSets(bs)
    expect_identical(out, 2L)

})

# relations<-() ----

test_that("relations(BaseSets) <- value works", {

    bs <- BaseSets(relations)

    relations(bs) <- relations(bs)[sample(nRelations(bs)), , drop=FALSE]

    expect_true(validObject(bs))

})

# elementData<-() ----

test_that("elementData(BaseSets) <- value works", {

    bs <- BaseSets(relations)

    elementData(bs) <- elementData(bs)[sample(nElements(bs)), , drop=FALSE]

    expect_true(validObject(bs))

})

# elementIds() ----

test_that("elementIds(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- elementIds(bs)

    expect_identical(out, c("A", "B", "C", "D", "E"))

})

# elementIds<-() ----

test_that("elementIds(BaseSets) <- value works", {

    bs <- BaseSets(relations)

    elementIds(bs) <- tail(LETTERS, nElements(bs))

    expect_true(validObject(bs))

})

# setData<-() ----

test_that("setData(BaseSets) <- value works", {

    bs <- BaseSets(relations)

    setData(bs) <- setData(bs)[sample(nSets(bs)), , drop=FALSE]

    expect_true(validObject(bs))

})

# setIds() ----

test_that("setIds(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- setIds(bs)

    expect_identical(out, c("set1", "set2"))

})

# setIds<-() ----

test_that("setIds(BaseSets) <- value works", {

    bs <- BaseSets(relations)

    setIds(bs) <- paste0("geneset", seq_len(nSets(bs)))

    expect_true(validObject(bs))

})

# subset() ----

test_that("subset(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- subset(bs, set == "set1")
    expect_true(all(out@relations$set == "set1"))
    expect_identical(rownames(out@setData), "set1")

})

# show() ----

test_that("show(BaseSets) works", {

    # Small objects fully displayed
    bs <- BaseSets(relations)

    out <- show(bs)
    expect_identical(colnames(out), c("element", "set", "elementData", "setData"))
    expect_identical(nrow(out), nrow(bs@relations)+1L)

    # Large objects partially displayed
    bs <- BaseSets(relations=DataFrame(element=letters, set=LETTERS))

    out <- show(bs)
    expect_identical(colnames(out), c("element", "set", "elementData", "setData"))
    expect_identical(nrow(out), unisets:::get_showHeadLines() + unisets:::get_showTailLines() + 2L)

})

# as.list() ----

test_that("as(BaseSets, \"list\") works", {

    bs <- BaseSets(relations)

    out <- as(bs, "list")
    expect_identical(out, list(set1=c("A", "B"), set2=c("C", "D", "E")))
    out <- as.list(bs)
    expect_identical(out, list(set1=c("A", "B"), set2=c("C", "D", "E")))

})

# as.matrix() ----

test_that("as(BaseSets, \"matrix\") works", {

    bs <- BaseSets(relations)

    expected.dim <- c(nrow(bs@elementData), nrow(bs@setData))

    out <- as(bs, "matrix")
    expect_type(out, "logical")
    expect_identical(dim(out), expected.dim)

    out <- as.matrix(bs)
    expect_type(out, "logical")
    expect_identical(dim(out), expected.dim)

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
    expect_identical(nrow(out@relations), 3L) # 3 TRUE values above

    out <- as.BaseSets.matrix(bm, "BaseSets")
    expect_s4_class(out, "BaseSets")
    expect_identical(nrow(out@relations), 3L) # 3 TRUE values above

})

# setLengths() ----

test_that("setLengths(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- setLengths(bs)
    expect_identical(out, c(set1=2L, set2=3L))

})

# elementLengths() ----

test_that("elementLengths(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- elementLengths(bs)
    expect_identical(out, c(A=1L, B=1L, C=1L, D=1L, E=1L))

})

