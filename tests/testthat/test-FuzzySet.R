
sets <- list(
  set1=c("A", "B"),
  set2=c("C", "D", "E")
)

# prepare named vectors to check that names are dropped by the constructor
elementsUnlist <- unlist(sets)
setsUnlist <- rep(names(sets), lengths(sets))
names(setsUnlist) <- paste0("x", seq_along(setsUnlist))

relations <- DataFrame(element=elementsUnlist, set=setsUnlist)

# We add a relation with membership function 0, that will be dropped during construction
membership <- seq(0, nrow(relations)-1) / nrow(relations)
# Names will also be dropped
names(membership) <- paste0("x", seq_along(membership))

# FuzzySets() ----

test_that("FuzzySets constructor produces valid objects", {

    expect_error(
        expect_message(
            FuzzySets(relations),
            "argument \"membership\" is missing, with no default",
            fixed=TRUE
        )
    )

    expect_message(FuzzySets(relations, membership=membership), "Setting names(membership) to NULL", fixed=TRUE)
    expect_message(FuzzySets(relations, membership=membership), "Setting rownames(relations) to NULL", fixed=TRUE)
    expect_message(FuzzySets(relations, membership=membership), "Setting names(relations$element) to NULL", fixed=TRUE)
    expect_message(FuzzySets(relations, membership=membership), "Setting names(relations$set) to NULL", fixed=TRUE)

    out <- FuzzySets(relations, membership=membership)

    expect_s4_class(out, "FuzzySets")
    expect_identical(slotNames(out), c("membership", "relations", "elementData", "setData"))

    # Check that names were dropped
    slot.relations <- slot(out, "relations")
    expect_null(rownames(slot.relations))
    expect_null(names(slot.relations$element))
    expect_null(names(slot.relations$set))
})

test_that("FuzzySets validity method identifies issues", {

    # Valid object
    out <- FuzzySets(relations, membership=membership)

    # Invalid membership length
    expect_error(
        FuzzySets(relations, membership=runif(nrow(relations)-1)),
        "length(membership) must be equal to nrow(relations)",
        fixed=TRUE
    )

    # membership function out of range [0,1]
    expect_error(
        FuzzySets(relations, membership=rep(-0.1, nrow(relations))),
        "membership function must be in the interval [0,1]",
        fixed=TRUE
    )

    expect_error(
        FuzzySets(relations, membership=rep(1.1, nrow(relations))),
        "membership function must be in the interval [0,1]",
        fixed=TRUE
    )

})

# relations<-() ----

test_that("relations(BaseSets) <- value works", {

    fs <- FuzzySets(relations, membership=membership)

    membership(fs) <- runif(nRelations(fs))

    expect_true(validObject(fs))

})

# subset() ----

test_that("subset(FuzzySets) works", {

    fs <- FuzzySets(relations, membership=membership)

    out <- subset(fs, membership > 0.5)
    expect_true(all(out@membership > 0.5))

})

# show() ----

test_that("show(FuzzySets) works", {

    fs <- FuzzySets(relations, membership=membership)

    out <- show(fs)
    expect_identical(colnames(out), c("element", "set", "membership", "elementData", "setData"))
    expect_identical(nrow(out), nrow(fs@relations)+1L)

})

# as.list() ----

test_that("as(FuzzySets, \"list\") works", {

    fs <- FuzzySets(relations, membership=membership)

    out <- as(fs, "list")
    expect_identical(out, list(set1=c("B"), set2=c("C", "D", "E")))
    out <- as.list(fs)
    expect_identical(out, list(set1=c("B"), set2=c("C", "D", "E")))

})

# as.matrix() ----

test_that("as(FuzzySets, \"matrix\") works", {

    fs <- FuzzySets(relations, membership=membership)

    expected.dim <- c(nrow(fs@elementData), nrow(fs@setData))

    out <- as(fs, "matrix")
    expect_type(out, "double")
    expect_identical(dim(out), expected.dim)

    out <- as.matrix(fs)
    expect_type(out, "double")
    expect_identical(dim(out), expected.dim)

})

test_that("as(FuzzySets, \"matrix\") throws message for multiple membership observations", {

    sets <- list(
        set1=c("A", "A", "B"),
        set2=c("C", "D", "E")
    )

    relations <- DataFrame(element=unlist(sets), set=rep(names(sets), lengths(sets)))
    membership <- runif(nrow(relations))

    fs <- FuzzySets(relations, membership=membership)

    expect_message(
        as(fs, "matrix"),
        "Aggregation function missing: defaulting to length"
    )

    expected.dim <- c(nrow(fs@elementData), nrow(fs@setData))

    out <- as(fs, "matrix")
    expect_type(out, "double")
    expect_identical(dim(out), expected.dim)

})

# as(matrix, "FuzzySets") ----

test_that("as(matrix, \"FuzzySets\") works", {

    nGenes <- 3
    nSets <- 2
    membership <- c(runif(nGenes*nSets-1), NA)
    fm <- matrix(
        membership,
        nrow=nGenes, ncol=nSets,
        dimnames=list(
            gene = paste0("gene", seq_len(nGenes)),
            set  = paste0("set", seq_len(nSets))
        )
    )

    out <- as(fm, "FuzzySets")
    expect_s4_class(out, "FuzzySets")
    expect_identical(nrow(out@relations), as.integer(nGenes*nSets-1))

    out <- as.FuzzySets.matrix(fm, "FuzzySets")
    expect_s4_class(out, "FuzzySets")
    expect_identical(nrow(out@relations), as.integer(nGenes*nSets-1))

})

# setLengths() ----

test_that("setLengths(FuzzySets) works", {

    fs <- FuzzySets(relations, membership=membership)

    out <- setLengths(fs)
    expect_identical(out, c(set1=1L, set2=3L))

})

# elementLengths() ----

test_that("elementLengths(FuzzySets) works", {

    fs <- FuzzySets(relations, membership=membership)

    out <- elementLengths(fs)
    expect_identical(out, c(B=1L, C=1L, D=1L, E=1L))

})

# as(BaseSets, "FuzzySets") ----

test_that("as(BaseSets, \"FuzzySets\") works", {

    bs <- BaseSets(relations)

    # Number of relations is preservedxw
    out <- as(bs, "FuzzySets")
    expect_s4_class(out, "FuzzySets")
    expect_identical(nrow(relations(out)), nrow(relations(bs)))
    expect_identical(out@membership, rep(1, nRelations(out)))

    out <- as.FuzzySets.BaseSets(bs)
    expect_s4_class(out, "FuzzySets")
    expect_identical(nrow(relations(out)), nrow(relations(bs)))
    expect_identical(out@membership, rep(1, nRelations(out)))

})
