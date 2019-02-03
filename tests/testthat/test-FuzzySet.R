
# Setup ----

# Visually intuitive definition of fuzzy sets
sets <- list(
    set1=c("A"=0, "B"=0.1),
    set2=c("B"=0.2, "C"=0.3, "D"=0.6),
    set3=c("E"=0.8)
)

# unlist the membership values
membershipUnlist <- unlist(sets) # named
# unlist the set names
setsUnlist <- rep(names(sets), lengths(sets))
names(setsUnlist) <- paste0("name", seq_along(setsUnlist))
# unlist the element names
elementUnlist <- unlist(sapply(sets, names)) # named

relations <- DataFrame(
    element=elementUnlist,
    set=setsUnlist,
    membership=membershipUnlist)

# FuzzySets() ----

test_that("FuzzySets constructor produces valid objects", {

    expect_error(
        FuzzySets(relations[, c("element", "set")]),
        "colnames(relations) must include c(\"membership\")",
        fixed=TRUE
    )

    expect_message(
        FuzzySets(relations),
        "Setting rownames(relations) to NULL",
        fixed=TRUE
    )

    out <- FuzzySets(relations)

    expect_s4_class(out, "FuzzySets")
})

test_that("FuzzySets validity method identifies issues", {

    # membership function out of range [0,1]
    relations0 <- relations
    relations0$membership[1] <- -1
    expect_error(
        FuzzySets(relations0),
        "membership function must be in the interval [0,1]",
        fixed=TRUE
    )
    relations0 <- relations
    relations0$membership[1] <- 2
    expect_error(
        FuzzySets(relations0),
        "membership function must be in the interval [0,1]",
        fixed=TRUE
    )

})

# membership<-() ----

test_that("membership(FuzzySets) <- value works", {

    fs <- FuzzySets(relations)

    newValues <- runif(length(fs))
    membership(fs) <- newValues

    expect_identical(membership(fs), newValues)

})

# subset() ----

test_that("subset(FuzzySets) works", {

    fs <- FuzzySets(relations)

    out <- subset(fs, membership > 0.5)
    expect_true(all(membership(out) > 0.5))

    out <- subset.FuzzySets(fs, membership > 0.5)
    expect_true(all(membership(out) > 0.5))

})

# show() ----

test_that("show(FuzzySets) works", {

    fs <- FuzzySets(relations)

    out <- show(fs)
    expect_identical(
        colnames(out),
        c("element", "set", "relationData", "elementData", "setData"))
    expect_identical(nrow(out), length(fs)+1L)

})

# as.list() ----

test_that("as(FuzzySets, \"list\") works", {

    fs <- FuzzySets(relations)

    out <- as(fs, "list")
    expect_identical(lengths(out), c(set1 = 2L, set2 = 3L, set3 = 1L))
    out <- as.list(fs)
    expect_identical(lengths(out), c(set1 = 2L, set2 = 3L, set3 = 1L))

})

# as.matrix() ----

test_that("as(FuzzySets, \"matrix\") works", {

    fs <- FuzzySets(relations)

    expected.dim <- c(nElements(fs), nSets(fs))

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

    relations <- DataFrame(
        element=unlist(sets),
        set=rep(names(sets), lengths(sets))
    )
    relations$membership <- runif(nrow(relations))

    fs <- FuzzySets(relations)

    expect_message(
        as(fs, "matrix"),
        "Aggregation function missing: defaulting to length"
    )

    expected.dim <- c(nElements(fs), nSets(fs))

    out <- as(fs, "matrix")
    expect_type(out, "double")
    expect_identical(dim(out), expected.dim)

})

# as(matrix, "FuzzySets") ----

test_that("as(matrix, \"FuzzySets\") works", {

    nGenes <- 3
    nSets <- 2
    membership <- runif(nGenes*nSets)
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
    expect_identical(length(out), as.integer(nGenes*nSets))

    out <- as.FuzzySets.matrix(fm, "FuzzySets")
    expect_s4_class(out, "FuzzySets")
    expect_identical(length(out), as.integer(nGenes*nSets))

})

# setLengths() ----

test_that("setLengths(FuzzySets) works", {

    fs <- FuzzySets(relations)

    out <- setLengths(fs)
    expect_identical(out, c(set1 = 2L, set2 = 3L, set3 = 1L))

})

# elementLengths() ----

test_that("elementLengths(FuzzySets) works", {

    fs <- FuzzySets(relations)

    out <- elementLengths(fs)
    expect_identical(out, c(A = 1L, B = 2L, C = 1L, D = 1L, E = 1L))

})

# as(BaseSets, "FuzzySets") ----

test_that("as(BaseSets, \"FuzzySets\") works", {

    bs <- BaseSets(relations[, c("element", "set")])

    # Number of relations is preserved
    out <- as(bs, "FuzzySets")
    expect_s4_class(out, "FuzzySets")
    expect_identical(nrow(relations(out)), nrow(relations(bs)))
    expect_identical(membership(out), rep(1, length(bs)))

    # if membership is present, it is used
    bs <- BaseSets(relations[, c("element", "set", "membership")])

    # Number of relations is preserved
    out <- as(bs, "FuzzySets")
    expect_s4_class(out, "FuzzySets")
    expect_identical(nrow(relations(out)), nrow(relations(bs)))
    expect_identical(membership(out), as.numeric(mcols(bs@relations)[["membership"]]))


})
