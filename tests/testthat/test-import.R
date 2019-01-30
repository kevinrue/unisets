
good_gmt <- system.file(package = "unisets", "extdata", "example.gmt")
dupe_ele <- system.file(package = "unisets", "extdata", "test_duplicated-elements.gmt")
dupe_set <- system.file(package = "unisets", "extdata", "test_duplicated-sets.gmt")

test_that("GMT importing produces valid BaseSets.", {
    good <- import(good_gmt)
    set_len <- as.numeric(setLengths(good))
    expect_equal(set_len, c(74, 200, 200, 200))
})

test_that("GMT importing produces error with duplicated set names.", {
    expect_error(import(dupe_set))
})

test_that("GMT importing passes with duplicated elements within set.", {
    pass <- import(dupe_ele)
    set_len <- as.numeric(setLengths(pass))
    expect_equal(set_len, c(3, 1))
})
