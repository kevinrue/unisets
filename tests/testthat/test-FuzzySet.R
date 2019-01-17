
sets <- list(
  set1=c("A", "B"),
  set2=c("C", "D", "E")
)

# prepare named vectors to check that names are dropped by the constructor
elementsUnlist <- unlist(sets)
setsUnlist <- rep(names(sets), lengths(sets))
names(setsUnlist) <- paste0("x", seq_along(setsUnlist))

map <- DataFrame(element=elementsUnlist, set=setsUnlist)

membership <- runif(nrow(map))
names(membership) <- paste0("x", seq_along(membership))

test_that("FuzzySets constructor produces valid objects", {

    expect_error(
        expect_message(
            FuzzySets(map),
            "argument \"membership\" is missing, with no default",
            fixed=TRUE
        )
    )

    expect_message(FuzzySets(map, membership=membership), "Setting rownames(map) to NULL", fixed=TRUE)
    expect_message(FuzzySets(map, membership=membership), "Setting names(map$element) to NULL", fixed=TRUE)
    expect_message(FuzzySets(map, membership=membership), "Setting names(map$set) to NULL", fixed=TRUE)
    expect_message(FuzzySets(map, membership=membership), "Setting names(membership) to NULL", fixed=TRUE)

    # Membership names are dropped during construction
    names(membership) <- paste0("name", seq_along(membership))
    expect_message(
        FuzzySets(map, membership=membership),
        "Setting names(membership) to NULL",
        fixed=TRUE
    )

    out <- FuzzySets(map, membership=membership)

    expect_s4_class(out, "FuzzySets")
    expect_identical(slotNames(out), c("membership", "map", "elementData", "setData"))

    # Check that names were dropped
    slot.map <- slot(out, "map")
    expect_null(rownames(slot.map))
    expect_null(names(slot.map$element))
    expect_null(names(slot.map$set))
})

test_that("FuzzySets validity method identifies issues", {

    # Valid object
    out <- FuzzySets(map, membership=membership)
    expect_true(uniset:::.valid.FuzzySets(out))

    # Invalid membership length
    expect_error(
        FuzzySets(map, membership=runif(nrow(map)-1)),
        "length(membership) must be equal to nrow(map)",
        fixed=TRUE
    )

    out0 <- out
    out0@membership <- c(out0@membership, 0.5)
    msg <- uniset:::.valid.FuzzySets(out0)
    expect_identical(
        msg,
        "length(membership) must be equal to nrow(map)"
    )

    # membership function out of range [0,1]
    expect_error(
        FuzzySets(map, membership=rep(-0.1, nrow(map))),
        "membership function must be in the interval [0,1]",
        fixed=TRUE
    )

    out0 <- out
    out0@membership[1] <- -0.1
    msg <- uniset:::.valid.FuzzySets(out0)
    expect_identical(
        msg,
        "membership function must be in the interval [0,1]"
    )

    expect_error(
        FuzzySets(map, membership=rep(1.1, nrow(map))),
        "membership function must be in the interval [0,1]",
        fixed=TRUE
    )

    out0 <- out
    out0@membership[1] <- 1.1
    msg <- uniset:::.valid.FuzzySets(out0)
    expect_identical(
        msg,
        "membership function must be in the interval [0,1]"
    )

})

test_that("subset(FuzzySets) works", {

    fs <- FuzzySets(map, membership=membership)

    out <- subset(fs, membership > 0.5)
    expect_true(all(out@membership > 0.5))

})

test_that("show(FuzzySets) works", {

    fs <- FuzzySets(map, membership=membership)

    out <- show(fs)
    expect_identical(colnames(out), c("element", "set", "membership", "elementData", "setData"))
    expect_identical(nrow(out), nrow(fs@map)+1L)

})

test_that("as(FuzzySets, \"list\") works", {

    fs <- FuzzySets(map, membership=membership)

    out <- as(fs, "list")
    expect_identical(out, list(set1=c("A", "B"), set2=c("C", "D", "E")))
    out <- as.list(fs)
    expect_identical(out, list(set1=c("A", "B"), set2=c("C", "D", "E")))

})

test_that("setLengths(FuzzySets) works", {

    bs <- FuzzySets(map, membership=membership)

    out <- setLengths(bs)
    expect_identical(out, c(set1=2L, set2=3L))

})

test_that("setLengths(FuzzySets) works", {

    bs <- FuzzySets(map, membership=membership)

    out <- elementLengths(bs)
    expect_identical(out, c(A=1L, B=1L, C=1L, D=1L, E=1L))

})
