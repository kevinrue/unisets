
# Setup ----

from <- c(5, 2, 3, 3, 3, 2)
to <- c(11, 15, 5, 4, 5, 11)
# We add a relation with membership function 0, that will be dropped during construction
membership <- c(0, 0.1, 0.2, 0.3, 0.6, 0.8)

# Names will also be dropped
names(membership) <- paste0("x", seq_along(membership))

# FuzzyHits() ----

test_that("FuzzyHits constructor produces valid objects", {

    expect_message(
        FuzzyHits(from, to, membership, 7, 15),
        "Setting names(membership) to NULL",
        fixed=TRUE)

    out <- FuzzyHits(from, to, membership, 7, 15)

    expect_s4_class(out, "FuzzyHits")
    expect_length(out, length(from))
})

test_that("FuzzyHits validity method identifies issues", {

    # Invalid membership length
    expect_error(
        FuzzyHits(from=c(1, 2), to=c(1, 2), membership=c(0.4), 7, 15),
        "mcols(x)' is not parallel to 'x'",
        fixed=TRUE
    )

    # membership function out of range [0,1]
    expect_error(
        FuzzyHits(from=c(1), to=c(1), membership=c(-1), 7, 15),
        "membership function must be in the interval [0,1]",
        fixed=TRUE
    )

    expect_error(
        FuzzyHits(from=c(1), to=c(1), membership=c(2), 7, 15),
        "membership function must be in the interval [0,1]",
        fixed=TRUE
    )

    expect_error(
        FuzzyHits(from=c(1), to=c(1), membership=c(NA), 7, 15),
        "membership function must be numeric",
        fixed=TRUE
    )

})

# show() ----

test_that("show(FuzzyHits) works", {

    fh <- FuzzyHits(from, to, membership, 7, 15)

    out <- show(fh)
    expect_null(out)

})

