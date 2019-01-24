
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

    out <- BaseSets(relations)

    expect_s4_class(out, "BaseSets")
})

test_that("BaseSets validity method identifies issues", {

    # Invalid colnames(object@relations)
    map0 <- relations
    colnames(map0) <- c("A", "B")
    expect_error(
        BaseSets(relations=map0),
        "colnames(relations) must be c(\"element\", \"set\")",
        fixed=TRUE
    )

    # Mismatch between elements and elementData
    expect_error(
        BaseSets(relations, elementData=IdVector("Z")),
        "relations$element missing from id(elementData)",
        fixed=TRUE
    )

    expect_error(
        BaseSets(relations, setData=IdVector("set999")),
        "relations$set missing from id(setData)",
        fixed=TRUE
    )

    expect_message(
        BaseSets(relations, elementData=IdVector(c(relations$element, "Z"))),
        "Dropping elementData missing from relations$element",
        fixed=TRUE
    )

    expect_message(
        BaseSets(relations, setData=IdVector(c(relations$set, "set999"))),
        "Dropping setData missing from relations$set",
        fixed=TRUE
    )

})

# nRelations() ----

test_that("nRelations(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- nRelations(bs)
    expect_identical(out, 5L)

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
    expect_identical(out, 2L)

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

    expect_identical(elementIds(bs), tail(LETTERS, nElements(bs)))

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

    expect_identical(setIds(bs), paste0("geneset", seq_len(nSets(bs))))

})

# subset() ----

test_that("subset(BaseSets) works", {

    bs <- BaseSets(relations)

    out <- subset(bs, set == "set1")

    expect_true(all(id(setData(out)) == "set1"))
    expect_identical(length(setData(out)), 1L)

})

# show() ----

test_that("show(BaseSets) works", {

    # Small objects fully displayed
    bs <- BaseSets(relations)

    out <- show(bs)
    expect_identical(colnames(out), c("element", "set", "elementData", "setData"))
    expect_identical(nrow(out), nRelations(bs)+1L)

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

    expected.dim <- c(nElements(bs), nSets(bs))

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
    expect_identical(length(relations(out)), 3L) # 3 TRUE values above

    out <- as.BaseSets.matrix(bm, "BaseSets")
    expect_s4_class(out, "BaseSets")
    expect_identical(length(relations(out)), 3L) # 3 TRUE values above

})

# as(Go3AnnDbBimap, "BaseSets") ----

test_that("as(Go3AnnDbBimap, \"BaseSets\") works", {

    out <- as(org.Hs.egGO, "BaseSets")
    expect_s4_class(out, "BaseSets")
    expect_gt(nRelations(out), 0)
    expect_gt(length(elementData(out)), 0)
    expect_gt(length(setData(out)), 0)
    expect_gt(ncol(elementMetadata(setData(out))), 0)

    out <- as.BaseSets.Go3AnnDbBimap(org.Hs.egGO, "BaseSets")
    expect_s4_class(out, "BaseSets")
    expect_gt(nRelations(out), 0)
    expect_gt(length(elementData(out)), 0)
    expect_gt(length(setData(out)), 0)
    expect_gt(ncol(elementMetadata(setData(out))), 0)

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

