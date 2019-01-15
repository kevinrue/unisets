
test_that("BaseSet constructor produces valid objects", {

    out <- BaseSet()

    expected.map <- DataFrame(element=character(0), set=character(0))
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

    colnames(out@map) <- c("A", "B")
    msg <- uniset:::.valid.BaseSet(out)
    expect_identical(
        msg,
        "colnames(object@map) must be c(\"element\", \"set\")"
    )

})
