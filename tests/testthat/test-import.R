
# Setup ----

# Example GMT files
good_gmt <- system.file(package="unisets", "extdata", "example.gmt")
dupe_ele <- system.file(package="unisets", "extdata", "test_duplicated-elements.gmt")
dupe_set <- system.file(package="unisets", "extdata", "test_duplicated-sets.gmt")

# Expected genesets from good_gmt
expected_gs <- c(
    "http://www.broadinstitute.org/gsea/msigdb/cards/HALLMARK_TNFA_SIGNALING_VIA_NFKB",
    "http://www.broadinstitute.org/gsea/msigdb/cards/HALLMARK_HYPOXIA",
    "http://www.broadinstitute.org/gsea/msigdb/cards/HALLMARK_CHOLESTEROL_HOMEOSTASIS",
    "http://www.broadinstitute.org/gsea/msigdb/cards/HALLMARK_MITOTIC_SPINDLE"
)

# A basic BaseSet without source defined
sets <- list(
    set1=c("A", "B"),
    set2=c("B", "C", "D")
)
elementsUnlist <- unlist(sets)
setsUnlist <- rep(names(sets), lengths(sets))
names(setsUnlist) <- paste0("x", seq_along(setsUnlist))
relations <- DataFrame(element=elementsUnlist, set=setsUnlist)
basic_baseset <- BaseSets(relations)

# GMTFile ----

test_that("GMTFile constructor works as expected.", {

    out <- GMTFile('example.gmt')

    expect_s4_class(out, "GMTFile")
    expect_identical(out, new("GMTFile", resource = "example.gmt"))

})

# import ----

test_that("GMT importing produces valid BaseSets with proper source", {

    out <- import(good_gmt)

    expect_equal(
        setLengths(out),
        c(
            HALLMARK_CHOLESTEROL_HOMEOSTASIS = 74L,
            HALLMARK_HYPOXIA = 200L,
            HALLMARK_MITOTIC_SPINDLE = 200L,
            HALLMARK_TNFA_SIGNALING_VIA_NFKB = 200L
        )
    )
    expect_identical(elementMetadata(setData(out))[["source"]], expected_gs)

})

test_that("GMT importing produces error with duplicated set names.", {

    expect_error(
        import(dupe_set),
        "Duplicated geneset names exist for the sets below. Please check your GMT file.",
        fixed=TRUE
    )

})

test_that("GMT importing passes with duplicated elements within set.", {

    out <- import(dupe_ele)

    expect_equal(setLengths(out), c(ONEBIT = 3L, TWOBIT = 1L))

})

test_that("GMT exporting produces a valid file from BaseSets.", {

    x <- import(good_gmt)

    tmp <- paste0(tempfile(), '.gmt')
    expect_silent(export(x, tmp))
    expect_message(
        export(basic_baseset, tmp),
        "'source' column not found in elementMetadata(setData(object)), setting to \"unisets\"",
        fixed=TRUE
    )

    # Test that the written file is valid and can be imported again
    # The only different between the export and imported object is the added source="unisets"
    basic_baseset1 <- import(tmp)
    basic_baseset0 <- basic_baseset
    basic_baseset0@setData@elementMetadata$source <- "unisets"
    expect_identical(basic_baseset1, basic_baseset0)

})

# import.gmt ----

test_that("GMT method import.gmt works as expected.", {

    expect_identical(import(good_gmt), import.gmt(good_gmt))

})

# export.gmt ----

test_that("GMT method export.gmt works as expected.", {

    tmp <- paste0(tempfile(), '.gmt')

    out <- import(good_gmt)

    expect_message(
        export.gmt(basic_baseset, tmp),
        "'source' column not found in elementMetadata(setData(object)), setting to \"unisets\"",
        fixed=TRUE
    )
    expect_silent(export.gmt(out, tmp))

})

# .gmt extension ----

test_that("GMT errors expected upon trying to import with bad extensions", {

    tmp <- paste0(tempfile(), '.poopsmith')

    expect_error(
        import(tmp),
        "Format 'poopsmith' unsupported",
        fixed=TRUE
    )
    expect_error(
        export(basic_baseset, tmp),
        "Format 'poopsmith' unsupported",
        fixed=TRUE
    )

})
