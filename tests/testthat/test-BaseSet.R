
sets <- list(
  set1=c("A", "B"),
  set2=c("C", "D", "E")
)

# prepare named vectors to check that names are dropped by the constructor
elementsUnlist <- unlist(sets)
setsUnlist <- rep(names(sets), lengths(sets))
names(setsUnlist) <- paste0("x", seq_along(setsUnlist))

map <- DataFrame(element=elementsUnlist, set=setsUnlist)

test_that("BaseSets constructor produces valid objects", {

    expect_message(BaseSets(map), "Setting rownames(map) to NULL", fixed=TRUE)
    expect_message(BaseSets(map), "Setting names(map$element) to NULL", fixed=TRUE)
    expect_message(BaseSets(map), "Setting names(map$set) to NULL", fixed=TRUE)

    out <- BaseSets(map)

    expect_s4_class(out, "BaseSets")
    expect_identical(slotNames(out), c("map", "elementData", "setData"))

    # Check that names were dropped
    slot.map <- slot(out, "map")
    expect_null(rownames(slot.map))
    expect_null(names(slot.map$element))
    expect_null(names(slot.map$set))
})

test_that("BaseSets validity method identifies issues", {

    # Valid object
    out <- BaseSets(map)
    expect_true(uniset:::.valid.BaseSets(out))

    # Invalid colnames(object@map)
    map0 <- map
    colnames(map0) <- c("A", "B")
    expect_error(
        BaseSets(map0),
        "colnames(object@map) must be c(\"element\", \"set\")",
        fixed=TRUE
    )

    out0 <- out
    colnames(out0@map) <- c("A", "B")
    msg <- uniset:::.valid.BaseSets(out0)
    expect_identical(
        msg,
        "colnames(object@map) must be c(\"element\", \"set\")"
    )

    # Mismatch between elements and  elementData
    expect_error(
        BaseSets(map, elementData=DataFrame(row.names="Z")),
        "Mismatch between map$element and rownames(elementData)",
        fixed=TRUE
    )

    out0 <- out
    out0@elementData <- rbind(out0@elementData, DataFrame(row.names="Z"))
    msg <- uniset:::.valid.BaseSets(out0)
    expect_identical(
        msg,
        "Mismatch between map$element and rownames(elementData)"
    )

    # Mismatch between elements and  elementData
    expect_error(
        BaseSets(map, setData=DataFrame(row.names="set999")),
        "Mismatch between map$set and rownames(setData)",
        fixed=TRUE
    )

    out0 <- out
    out0@setData <- rbind(out0@setData, DataFrame(row.names="set999"))
    msg <- uniset:::.valid.BaseSets(out0)
    expect_identical(
        msg,
        "Mismatch between map$set and rownames(setData)"
    )

})

test_that("subset(BaseSets) works", {

    bs <- BaseSets(map)

    out <- subset(bs, set == "set1")
    expect_true(all(out@map$set == "set1"))
    expect_identical(rownames(out@setData), "set1")

})


test_that("show(BaseSets) works", {

    # Small objects fully displayed
    bs <- BaseSets(map)

    out <- show(bs)
    expect_identical(colnames(out), c("element", "set", "elementData", "setData"))
    expect_identical(nrow(out), nrow(bs@map)+1L)

    # Large objects partially displayed
    bs <- BaseSets(map=DataFrame(element=letters, set=LETTERS))

    out <- show(bs)
    expect_identical(colnames(out), c("element", "set", "elementData", "setData"))
    expect_identical(nrow(out), uniset:::get_showHeadLines() + uniset:::get_showTailLines() + 2L)

})

test_that("as(BaseSets, \"list\") works", {

    bs <- BaseSets(map)

    out <- as(bs, "list")
    expect_identical(out, list(set1=c("A", "B"), set2=c("C", "D", "E")))
    out <- as.list(bs)
    expect_identical(out, list(set1=c("A", "B"), set2=c("C", "D", "E")))

})

test_that("setLengths(BaseSets) works", {

    bs <- BaseSets(map)

    out <- setLengths(bs)
    expect_identical(out, c(set1=2L, set2=3L))

})

test_that("setLengths(BaseSets) works", {

    bs <- BaseSets(map)

    out <- elementLengths(bs)
    expect_identical(out, c(A=1L, B=1L, C=1L, D=1L, E=1L))

})
