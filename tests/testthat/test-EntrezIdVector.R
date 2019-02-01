
# IdVector() ----

test_that("IdVector constructor produces valid objects", {

    out <- EntrezIdVector(keys(org.Hs.eg.db))

    expect_s4_class(out, "EntrezIdVector")
    expect_identical(slotNames(out), c("ids", "elementMetadata", "metadata"))

    expect_identical(length(out), length(keys(org.Hs.eg.db)))
})
