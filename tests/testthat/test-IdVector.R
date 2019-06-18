
# Setup ----

nValues <- 10L

idValues <- head(LETTERS, nValues)
# add names to
names(idValues) <- head(letters, nValues)

# IdVector() ----

test_that("IdVector constructor produces valid objects", {

    expect_message(IdVector(idValues), "Setting names(ids) to NULL", fixed=TRUE)

    out <- IdVector(idValues)

    expect_s4_class(out, "IdVector")
    expect_identical(slotNames(out), c("ids", "elementMetadata", "metadata"))

    # Check that names were dropped
    slot.ids <- slot(out, "ids")
    expect_null(names(slot.ids))
    expect_identical(ids(out), as.character(idValues))
})

# ids() ----

test_that("ids(IdVector) works", {

    iv <- IdVector(idValues)

    ids(iv) <- tail(LETTERS, length(iv))
    expect_identical(ids(iv), iv@ids)

})

# ids<-() ----

test_that("ids(IdVector) <- value works", {

    iv <- IdVector(idValues)

    out <- ids(iv)
    expect_identical(out, as.character(idValues))

})

test_that("IdVector validity method identifies issues", {

    # Invalid colnames(object@relations)
    iv0 <- IdVector(c("A", "A", "B", "B", "C", "C", "D", "D", "E", "E", "F"))
    mcols(iv0) <- DataFrame(
        dummy=c(rep(1:2, 5), 1)
    )

    expect_error(
        validObject(iv0),
        "some identifiers do not have unique metadata: ",
        fixed=TRUE
    )

})

# names() ----

test_that("names(IdVector) works", {

    iv <- IdVector(idValues)

    names(iv) <- tail(LETTERS, length(iv))
    expect_identical(names(iv), tail(LETTERS, length(iv)))

})

# names<-() ----

test_that("names(IdVector) <- value works", {

    iv <- IdVector(idValues)

    out <- names(iv)
    expect_identical(out, as.character(idValues))

})

# [ ----

test_that("`[`IdVector works", {

    iv <- IdVector(idValues)

    out <- iv[1:5]
    expect_length(out, 5)
    expect_identical(ids(out), head(as.character(idValues), 5))

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
    expect_identical(out, NULL)

    # Large objects partially displayed
    iv <- IdVector(idValues)

    out <- show(iv)
    # The show method invisibly returns the character vector of identifiers
    expect_identical(out, NULL)

    # Large elementMetadata
    mcols(iv) <- DataFrame(a=1, b=2, c=3, d=4, e=5, f=6)
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
    expect_identical(ids(out), as.character(idValues))

    out <- as.IdVector.default(idValues)
    expect_s4_class(out, "IdVector")
    expect_identical(ids(out), as.character(idValues))

})

# as(IdVector, "character") ----

test_that("as(IdVector, \"character\") works", {

    iv <- IdVector(idValues)

    out <- as(iv, "character")
    expect_type(out, "character")
    expect_identical(out, ids(iv))

    out <- as.character.IdVector(iv)
    expect_type(out, "character")
    expect_identical(out, ids(iv))

})

# as(IdVector, "vector") ----

test_that("as(IdVector, \"character\") works", {

    iv <- IdVector(idValues)

    out <- as(iv, "vector")
    expect_type(out, "character")
    expect_identical(out, ids(iv))

    out <- as.vector.IdVector(iv)
    expect_type(out, "character")
    expect_identical(out, ids(iv))

})

# split(IdVector, IdVector) ----

test_that("split(IdVector, IdVector) works", {

    iv1 <- IdVector(head(letters, 6))
    iv2 <- IdVector(c(rep("set1", 1), rep("set2", 2), rep("set3", 3)))

    out <- split(iv1, iv2)
    expect_named(out, c("set1", "set2", "set3"))
    expect_identical(lengths(out), c(set1=1L, set2=2L, set3=3L))

})

# duplicated(IdVector) ----

test_that("duplicated(IdVector) works", {

    iv <- IdVector(rep(head(letters, 3), 2L))
    mcols(iv) <- DataFrame(row.names = ids(iv), field1=rep(runif(3), 2))

    out <- duplicated(iv)
    expect_identical(out, c(rep(FALSE, 3), rep(TRUE, 3)))

})

# unique(IdVector) ----

test_that("unique(IdVector) works", {

    iv <- IdVector(rep(head(letters, 3), 2L))
    mcols(iv) <- DataFrame(row.names = ids(iv), field1=rep(runif(3), 2))

    out <- unique(iv)
    expect_length(out, 3L)
    expect_identical(ids(out), head(letters, 3))

})

# c(IdVector, ...) ----

test_that("c(IdVector, ...) works", {

    iv1 <- IdVector(head(letters, 3))
    mcols(iv1) <- DataFrame(row.names = ids(iv1), field1=runif(length(iv1)))
    iv2 <- IdVector(tail(letters, 3))
    mcols(iv2) <- DataFrame(row.names = ids(iv2), field1=runif(length(iv2)))
    iv3 <- iv2

    out <- c(iv1, iv2, iv3)
    expect_named(out, c("a", "b", "c", "x", "y", "z", "x", "y", "z"))
    expect_identical(mcols(out), rbind(mcols(iv1), mcols(iv2), mcols(iv3)))

})

# union(IdVector) ----

test_that("union(IdVector) works", {

    iv1 <- IdVector(head(letters, 3))
    mcols(iv1) <- DataFrame(row.names = ids(iv1), field1=runif(length(iv1)))
    iv2 <- IdVector(tail(letters, 3))
    mcols(iv2) <- DataFrame(row.names = ids(iv2), field1=runif(length(iv2)))

    # Union of non overlapping IdVector's
    out <- union(iv1, iv2)
    expect_identical(
        ids(out),
        unique(c(ids(iv1), ids(iv2)))
    )
    expect_identical(mcols(out), rbind(mcols(iv1), mcols(iv2)))


    # Union of fully overlapping IdVector's
    out <- union(iv1, iv1)
    expect_identical(out, iv1)

    # Union of partially overlapping IdVector's
    iv3 <- iv1
    ids(iv3)[1] <- "new"
    out <- union(iv1, iv3)

    expect_identical(
        ids(out),
        unique(c(ids(iv1), ids(iv3)))
    )
    expect_identical(mcols(out), rbind(mcols(iv1), mcols(iv3)["new", , drop=FALSE]))

})
