
sets <- list(
  set1=c("A", "B"),
  set2=c("C", "D", "E")
)

# prepare named vectors to check that names are dropped by the constructor
elementsUnlist <- unlist(sets)
setsUnlist <- rep(names(sets), lengths(sets))
names(setsUnlist) <- paste0("x", seq_along(setsUnlist))

map <- DataFrame(element=elementsUnlist, set=setsUnlist)

test_that("BaseSet constructor produces valid objects", {

    expect_message(BaseSet(map), "Setting rownames(map) to NULL", fixed=TRUE)
    expect_message(BaseSet(map), "Setting names(map$element) to NULL", fixed=TRUE)
    expect_message(BaseSet(map), "Setting names(map$set) to NULL", fixed=TRUE)

    out <- BaseSet(map)

    expect_s4_class(out, "BaseSet")
    expect_identical(slotNames(out), c("map", "elementData", "setData"))

    # Check that names were dropped
    slot.map <- slot(out, "map")
    expect_null(rownames(slot.map))
    expect_null(names(slot.map$element))
    expect_null(names(slot.map$set))
})

test_that("BaseSet validity method identifies issues", {

    # Valid object
    out <- BaseSet(map)
    expect_true(uniset:::.valid.BaseSet(out))

    # Invalid colnames(object@map)
    map0 <- map
    colnames(map0) <- c("A", "B")
    expect_error(
        BaseSet(map0),
        "colnames(object@map) must be c(\"element\", \"set\")",
        fixed=TRUE
    )

    out0 <- out
    colnames(out0@map) <- c("A", "B")
    msg <- uniset:::.valid.BaseSet(out0)
    expect_identical(
        msg,
        "colnames(object@map) must be c(\"element\", \"set\")"
    )

    # Mismatch between elements and  elementData
    expect_error(
        BaseSet(map, elementData=DataFrame(row.names="Z")),
        "Mismatch between map$element and rownames(elementData)",
        fixed=TRUE
    )

    out0 <- out
    out0@elementData <- rbind(out0@elementData, DataFrame(row.names="Z"))
    msg <- uniset:::.valid.BaseSet(out0)
    expect_identical(
        msg,
        "Mismatch between map$element and rownames(elementData)"
    )

    # Mismatch between elements and  elementData
    expect_error(
        BaseSet(map, setData=DataFrame(row.names="set999")),
        "Mismatch between map$set and rownames(setData)",
        fixed=TRUE
    )

    out0 <- out
    out0@setData <- rbind(out0@setData, DataFrame(row.names="set999"))
    msg <- uniset:::.valid.BaseSet(out0)
    expect_identical(
        msg,
        "Mismatch between map$set and rownames(setData)"
    )

})

test_that("subset(BaseSet) works", {

    bs <- BaseSet(map)

    out <- subset(bs, set == "set1")
    expect_true(all(out@map$set == "set1"))
    expect_identical(rownames(out@setData), "set1")

})


test_that("show(BaseSet) works", {

    # Small objects fully displayed
    bs <- BaseSet(map)

    out <- show(bs)
    expect_identical(colnames(out), c("element", "set", "elementData", "setData"))
    expect_identical(nrow(out), nrow(bs@map)+1L)

    # Large objects partially displayed
    bs <- BaseSet(map=DataFrame(element=letters, set=LETTERS))

    out <- show(bs)
    expect_identical(colnames(out), c("element", "set", "elementData", "setData"))
    expect_identical(nrow(out), uniset:::get_showHeadLines() + uniset:::get_showTailLines() + 2L)

})

test_that("as(BaseSet, \"list\") works", {

    bs <- BaseSet(map)

    out <- as(bs, "list")
    expect_identical(out, list(set1=c("A", "B"), set2=c("C", "D", "E")))
    out <- as.list(bs)
    expect_identical(out, list(set1=c("A", "B"), set2=c("C", "D", "E")))

})

test_that("setLengths(BaseSet) works", {

    bs <- BaseSet(map)

    out <- setLengths(bs)
    expect_identical(out, c(set1=2L, set2=3L))

})

test_that("setLengths(BaseSet) works", {

    bs <- BaseSet(map)

    out <- elementLengths(bs)
    expect_identical(out, c(A=1L, B=1L, C=1L, D=1L, E=1L))

})
