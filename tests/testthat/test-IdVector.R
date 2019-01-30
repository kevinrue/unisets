
# Setup ----

nValues <- 10L

idValues <- head(LETTERS, nValues)
# add names to
names(idValues) <- head(letters, nValues)

# IdVector() ----

test_that("IdVector constructor produces valid objects", {

    expect_message(IdVector(idValues), "Setting names(id) to NULL", fixed=TRUE)

    out <- IdVector(idValues)

    expect_s4_class(out, "IdVector")
    expect_identical(slotNames(out), c("id", "elementMetadata", "metadata"))

    # Check that names were dropped
    slot.id <- slot(out, "id")
    expect_null(names(slot.id))
    expect_identical(id(out), as.character(idValues))
})

# id() ----

test_that("id(IdVector) works", {

    iv <- IdVector(idValues)

    id(iv) <- tail(LETTERS, length(iv))
    expect_identical(id(iv), tail(LETTERS, length(iv)))

})

# id<-() ----

test_that("id(IdVector) <- value works", {

    iv <- IdVector(idValues)

    out <- id(iv)
    expect_identical(out, as.character(idValues))

})

# [ ----

test_that("`[`IdVector works", {

    iv <- IdVector(idValues)

    out <- iv[1:5]
    expect_length(out, 5)
    expect_identical(id(out), head(as.character(idValues), 5))

})

# NSBS ----

test_that("`NSBS(IdVector) works", {

    idx <- as.integer(c(1, 3, 4, 5, 6))
    iv <- IdVector(idValues[idx])

    DF <- DataFrame(row.names = idValues)

    out <- NSBS(iv, DF)
    expect_length(out, 5)
    expect_identical(out, idx)

})

# show() ----

test_that("show(IdVector) works", {

    # Small objects fully displayed
    iv <- IdVector(head(idValues, 3))

    out <- show(iv)
    # The show method invisibly returns the character vector of identifiers
    expect_identical(out, head(as.character(idValues), 3))

    # Large objects partially displayed
    iv <- IdVector(idValues)

    out <- show(iv)
    # The show method invisibly returns the character vector of identifiers
    expect_identical(out, as.character(idValues))

    # Large elementMetadata
    elementMetadata(iv) <- DataFrame(a=1, b=2, c=3, d=4, e=5, f=6)
    out <- show(iv)

})

# pcompare() ----

test_that("pcompare(IdVector, IdVector) works", {

    iv <- IdVector(idValues)

    out <- pcompare(iv, iv)
    expect_identical(out, rep(TRUE, length(iv)))

    out <- pcompare(iv, as.character(iv))
    expect_identical(out, rep(TRUE, length(iv)))

    out <- pcompare(as.character(iv), iv)
    expect_identical(out, rep(TRUE, length(iv)))

})

# as(character, "IdVector") ----

test_that("as(character, \"IdVector\") works", {

    out <- as(idValues, "IdVector")
    expect_s4_class(out, "IdVector")
    expect_identical(id(out), as.character(idValues))

    out <- as.IdVector.character(idValues)
    expect_s4_class(out, "IdVector")
    expect_identical(id(out), as.character(idValues))

})

# as(, "character") ----

test_that("as(IdVector, \"character\") works", {

    iv <- IdVector(idValues)

    out <- as(iv, "character")
    expect_type(out, "character")
    expect_identical(out, id(iv))

    out <- as.character.IdVector(iv)
    expect_type(out, "character")
    expect_identical(out, id(iv))

})
