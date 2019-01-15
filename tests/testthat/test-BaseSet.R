
sets <- list(
  set1=c("A", "B"),
  set2=c("C", "D", "E")
)

map <- DataFrame(
  element=unlist(sets),
  set=rep(names(sets), lengths(sets))
)

test_that("BaseSet constructor produces valid objects", {

    expect_message(
        BaseSet(map),
        "Setting rownames(map) to NULL",
        fixed=TRUE
    )

    out <- BaseSet(map)

    expect_s4_class(out, "BaseSet")
    expect_identical(slotNames(out), c("map", "elementData", "setData"))

    expected.map <- map
    rownames(expected.map) <- NULL
    expect_identical(slot(out, "map"), expected.map)
})

test_that("BaseSet validity method identifies issues", {

    # Valid object
    out <- BaseSet(map = DataFrame(element="a", set="b"))
    expect_true(uniset:::.valid.BaseSet(out))

    # Invalid colnames(object@map)
    expect_error(
        BaseSet(map = DataFrame(A="a", B="b")),
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
    out0 <- out
    out0@elementData <- rbind(out0@elementData, DataFrame(row.names="Z"))
    msg <- uniset:::.valid.BaseSet(out0)
    expect_identical(
        msg,
        "Some elements differ between the 'map' and 'elementData' slots"
    )

    # Mismatch between elements and  elementData
    out0 <- out
    out0@setData <- rbind(out0@setData, DataFrame(row.names="set999"))
    msg <- uniset:::.valid.BaseSet(out0)
    expect_identical(
        msg,
        "Some sets differ between the 'map' and 'elementData' slots"
    )

})
