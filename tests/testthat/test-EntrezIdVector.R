
# EntrezIdVector() ----

test_that("EntrezIdVector constructor produces valid objects", {

    keys <- keys(org.Hs.eg.db, keytype="ENTREZID") # ENTREZID is default
    out <- EntrezIdVector(keys)

    expect_s4_class(out, "EntrezIdVector")
    expect_identical(slotNames(out), c("ids", "elementMetadata", "metadata"))

    expect_identical(length(out), length(keys))
})
