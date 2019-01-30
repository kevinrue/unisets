## Test GMT files
good_gmt <- system.file(package = "unisets", "extdata", "example.gmt")
dupe_ele <- system.file(package = "unisets", "extdata", "test_duplicated-elements.gmt")
dupe_set <- system.file(package = "unisets", "extdata", "test_duplicated-sets.gmt")

## Expected genesets from good_gmt
expected_gs <- c("http://www.broadinstitute.org/gsea/msigdb/cards/HALLMARK_TNFA_SIGNALING_VIA_NFKB",
              "http://www.broadinstitute.org/gsea/msigdb/cards/HALLMARK_HYPOXIA",
              "http://www.broadinstitute.org/gsea/msigdb/cards/HALLMARK_CHOLESTEROL_HOMEOSTASIS",
              "http://www.broadinstitute.org/gsea/msigdb/cards/HALLMARK_MITOTIC_SPINDLE")

## A basic BaseSet without source defined
sets <- list(set1=c("A", "B"), set2=c("B", "C", "D"))
elementsUnlist <- unlist(sets)
setsUnlist <- rep(names(sets), lengths(sets))
names(setsUnlist) <- paste0("x", seq_along(setsUnlist))
relations <- DataFrame(element=elementsUnlist, set=setsUnlist)
basic_baseset <- BaseSets(relations)


## Tests
test_that("GMT importing produces valid BaseSets with proper source.", {
    good <- import(good_gmt)
    set_len <- as.numeric(setLengths(good))
    expect_equal(set_len, c(74, 200, 200, 200))
    expect_identical(elementMetadata(setData(good))$source, expected_gs)
})

test_that("GMT importing produces error with duplicated set names.", {
    expect_error(import(dupe_set))
})

test_that("GMT importing passes with duplicated elements within set.", {
    pass <- import(dupe_ele)
    set_len <- as.numeric(setLengths(pass))
    expect_equal(set_len, c(3, 1))
})

test_that("GMTFile constructor works as expected.", {
    gmt <- GMTFile('example.gmt')
    expect_s4_class(gmt, "GMTFile")
})

test_that("GMT exporting produces a valid file from BaseSets.", {
    good <- import(good_gmt)
    tmp <- paste0(tempfile(), '.gmt')
    expect_silent(export(good, tmp))
    expect_message(export(basic_baseset, tmp))
})


test_that("GMT errors expected upon trying to import with bad suffixes.", {
    tmp <- paste0(tempfile(), '.poopsmith')
    expect_error(import(tmp))
    expect_error(export(basic_baseset, tmp))
})

test_that("GMT method import.gmt works as expected.", {
    expect_equal(import(good_gmt), import.gmt(good_gmt))
})

test_that("GMT method export.gmt works as expected.", {
    tmp <- paste0(tempfile(), '.gmt')
    gs <- import(good_gmt)
    expect_message(export.gmt(basic_baseset, tmp))
    expect_silent(export.gmt(gs, tmp))
})

