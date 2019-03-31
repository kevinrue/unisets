
# EnsemblIdVector() ----

test_that("EnsemblIdVector constructor produces valid objects", {

    keys <- keys(org.Hs.eg.db, keytype="ENSEMBL")
    out <- EnsemblIdVector(keys)

    expect_s4_class(out, "EnsemblIdVector")
    expect_identical(slotNames(out), c("ids", "elementMetadata", "metadata"))

    expect_identical(length(out), length(keys))
})
