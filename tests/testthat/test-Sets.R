
# Setup ----

sets_list <- list(
    set1 = c("A", "B"),
    set2 = c("B", "C", "D"),
    set3 = c("E")
)

# prepare named vectors to check that names are dropped by the constructor
elements_unlist <- unlist(sets_list)
sets_unlist <- rep(names(sets_list), lengths(sets_list))
names(sets_unlist) <- paste0("x", seq_along(sets_unlist))

relations <- DataFrame(
    element = elements_unlist,
    set     = sets_unlist,
    extra1  = rep(c("ABC", "DEF"), c(3L, 3L)),
    extra2  = seq(0, 1, length.out = 6L)
)

# Sets() ----

test_that("Sets constructor produces valid objects", {

    out <- Sets(relations)

    expect_s4_class(out, "Sets")

})

test_that("Sets validity method identifies issues", {

    # Invalid colnames(object@relations)
    map0 <- relations
    colnames(map0) <- c("A", "B")
    expect_error(
        Sets(relations=map0),
        "colnames(relations) must include c(\"element\", \"set\")",
        fixed=TRUE
    )

    # Mismatch between elements and elementInfo
    expect_error(
        Sets(relations, elementInfo=IdVector("Z")),
        "relations$element missing from ids(elementInfo)",
        fixed=TRUE
    )

    expect_error(
        Sets(relations, setInfo=IdVector("set999")),
        "relations$set missing from ids(setInfo)",
        fixed=TRUE
    )

    expect_error(
        Sets(relations, elementInfo=IdVector(relations$element)),
        "duplicated values in ids(elementInfo(object))",
        fixed=TRUE
    )

    expect_error(
        Sets(relations, setInfo=IdVector(relations$set)),
        "duplicated values in ids(setInfo(object))",
        fixed=TRUE
    )

    # Provide metadata columns without rownames set
    relations0 <- DataFrame(element="element1", set="set1")
    elementInfo=IdVector("element1")
    mcols(elementInfo) <- DataFrame(field="elementValue")
    setInfo=IdVector("set1")
    mcols(setInfo) <- DataFrame(field="setValue")

    # Check that rownames(mcols(x)) is not NULL (using the default use.names=TRUE)
    # while the actual DataFrame does not store rownames
    out <- Sets(relations0, elementInfo, setInfo)
    expect_true(!is.null(rownames(mcols(elementInfo(out)))))
    expect_null(rownames(out@elementInfo@elementMetadata))
    expect_true(!is.null(rownames(mcols(setInfo(out)))))
    expect_null(rownames(out@setInfo@elementMetadata))

})

# relations() ----

test_that("relations(Sets) works", {

    bs <- Sets(relations)

    out <- relations(bs)
    expect_s4_class(out, "Hits")

})

# length() ----

test_that("length(Sets) works", {

    bs <- Sets(relations)

    out <- length(bs)
    expect_identical(out, 6L)

})

# elements() ----

test_that("elements(Sets) works", {

    bs <- Sets(relations)

    out <- elements(bs)
    expect_identical(out, bs@elementInfo[bs@relations@from])

})

# nElements() ----

test_that("nElements(Sets) works", {

    bs <- Sets(relations)

    out <- nElements(bs)
    expect_identical(out, 5L)

})

# sets() ----

test_that("elements(Sets) works", {

    bs <- Sets(relations)

    out <- sets(bs)
    expect_identical(out, bs@setInfo[bs@relations@to])

})

# nSets() ----

test_that("nSets(Sets) works", {

    bs <- Sets(relations)

    out <- nSets(bs)
    expect_identical(out, 3L)

})

# elementInfo() ----

test_that("ids(elementInfo(Sets)) works", {

    bs <- Sets(relations)

    out <- elementInfo(bs)
    expect_s4_class(out, "IdVector")

    out <- ids(elementInfo(bs))
    expect_identical(out, c("A", "B", "C", "D", "E"))

})

# elementInfo<-() ----

test_that("elementInfo(Sets) <- value works", {

    bs <- Sets(relations)

    ids(elementInfo(bs)) <- tail(LETTERS, nElements(bs))

    expect_identical(ids(elementInfo(bs)), tail(LETTERS, nElements(bs)))

})

# setInfo() ----

test_that("setInfo(Sets) works", {

    bs <- Sets(relations)

    out <- setInfo(bs)
    expect_s4_class(out, "IdVector")

    out <- ids(setInfo(bs))
    expect_identical(out, c("set1", "set2", "set3"))

})

# setInfo<-() ----

test_that("setInfo(Sets) <- value works", {

    bs <- Sets(relations)

    ids(setInfo(bs)) <- paste0("geneset", seq_len(nSets(bs)))

    expect_identical(ids(setInfo(bs)), paste0("geneset", seq_len(nSets(bs))))

})

# subset() ----

test_that("subset(Sets) works with default drop=TRUE", {

    bs <- Sets(relations)

    out <- subset(bs, set == "set1") # default is drop=TRUE

    expect_true(all(ids(setInfo(out)) == "set1"))
    expect_identical(length(setInfo(out)), 1L)

    out <- subset.Sets(bs, set == "set1") # default is drop=TRUE

    expect_true(all(ids(setInfo(out)) == "set1"))
    expect_identical(length(setInfo(out)), 1L)

    out <- bs[1:3] # default is drop=TRUE
    expect_identical(length(out), 3L)

})

test_that("subset(Sets) works with drop=FALSE", {

    bs <- Sets(relations)

    out <- subset(bs, set == "set1", drop=FALSE) # default is drop=TRUE

    expect_true(all(ids(sets(out)) == "set1"))
    expect_identical(ids(setInfo(out)), ids(setInfo(bs)))

    out <- subset.Sets(bs, set == "set1", drop=FALSE) # default is drop=TRUE

    expect_true(all(ids(sets(out)) == "set1"))
    expect_identical(ids(setInfo(out)), ids(setInfo(bs)))

    out <- bs[1:3, drop=FALSE] # default is drop=TRUE
    expect_identical(length(out), 3L)
    expect_identical(ids(setInfo(out)), ids(setInfo(bs)))

})

# c() ----

test_that("c(Sets) works", {

    bs1 <- bs2 <- bs3 <- Sets(relations)

    out <- c(bs1, bs2, bs3)

    # relations are concatenated
    expect_length(out, length(bs1) + length(bs2) + length(bs3))
    expect_identical(
        as.data.frame(out),
        rbind(as.data.frame(bs1), as.data.frame(bs2), as.data.frame(bs3))
    )
    # elements and sets are combined into their union
    expect_length(
        elementInfo(out),
        length(unique(c(ids(elementInfo(bs1)), ids(elementInfo(bs2)), ids(elementInfo(bs3)))))
    )
    expect_length(
        setInfo(out),
        length(unique(c(ids(setInfo(bs1)), ids(setInfo(bs2)), ids(setInfo(bs3)))))
    )

    out <- c.Sets(bs1, bs2, bs3)

    # relations are concatenated
    expect_length(out, length(bs1) + length(bs2) + length(bs3))
    expect_identical(
        as.data.frame(out),
        rbind(as.data.frame(bs1), as.data.frame(bs2), as.data.frame(bs3))
    )
    # elements and sets are combined into their union
    expect_length(
        elementInfo(out),
        length(unique(c(ids(elementInfo(bs1)), ids(elementInfo(bs2)), ids(elementInfo(bs3)))))
    )
    expect_length(
        setInfo(out),
        length(unique(c(ids(setInfo(bs1)), ids(setInfo(bs2)), ids(setInfo(bs3)))))
    )

})

# duplicated() ----

test_that("duplicated(Sets) works", {

    bs1 <- bs2 <- Sets(relations)

    # bs2 is an exact duplicate of bs1
    out <- duplicated(c(bs1, bs2))
    expect_identical(out, rep(c(FALSE, TRUE), c(length(bs1), length(bs2))))

    # change the metadata of the relations in bs2; creating _different_ relations
    mcols(relations(bs2)) <- mcols(relations(bs2))[rev(seq_along(bs2)), ]

    out <- duplicated(c(bs1, bs2))
    expect_identical(out, rep(c(FALSE, FALSE), c(length(bs1), length(bs2))))
})

# unique() ----

test_that("unique(Sets) works", {

    bs1 <- bs2 <- bs3 <- Sets(relations)

    bs <- c(bs1, bs2, bs3)

    out <- unique(bs)
    expect_length(out, length(bs1))
    expect_identical(out, bs1)
})

# union() ----

test_that("union(Sets) works", {

    bs1 <- bs2 <- Sets(relations)

    ids(elementInfo(bs2))[1] <- "new"

    out <- union(bs1, bs2)

    expect_identical(
        as.data.frame(out),
        unique(rbind(as.data.frame(bs1), as.data.frame(bs2)))
    )
    expect_identical(
        ids(elementInfo(out)),
        unique(c(ids(elementInfo(bs1)), ids(elementInfo(bs2))))
    )
    expect_identical(
        ids(setInfo(out)),
        unique(c(ids(setInfo(bs1)), ids(setInfo(bs2))))
    )

    out <- union.Sets(bs1, bs2)

    expect_identical(
        as.data.frame(out),
        unique(rbind(as.data.frame(bs1), as.data.frame(bs2)))
    )
    expect_identical(
        ids(elementInfo(out)),
        unique(c(ids(elementInfo(bs1)), ids(elementInfo(bs2))))
    )
    expect_identical(
        ids(setInfo(out)),
        unique(c(ids(setInfo(bs1)), ids(setInfo(bs2))))
    )
})

# show() ----

test_that("show(Sets) works", {

    # Small objects fully displayed
    bs <- Sets(relations)

    out <- show(bs)
    expect_identical(out, NULL)

    # Large objects partially displayed
    bs <- Sets(relations=DataFrame(element=letters, set=LETTERS))

    out <- show(bs)
    expect_identical(out, NULL)

})

# as.DataFrame() ----

test_that("as(Sets, \"DataFrame\") works", {

    bs <- Sets(relations)

    out <- as(bs, "DataFrame")
    expect_s4_class(out, "DataFrame")
    expect_identical(colnames(out), c("element", "set", "relationData", "elementInfo", "setInfo"))
    expect_identical(nrow(out), nrow(relations))

    out <- as.DataFrame.Sets(bs)
    expect_s4_class(out, "DataFrame")
    expect_identical(colnames(out), c("element", "set", "relationData", "elementInfo", "setInfo"))
    expect_identical(nrow(out), nrow(relations))

})

# as.data.frame() ----

test_that("as(Sets, \"data.frame\") works", {

    bs <- Sets(relations)

    out <- as(bs, "data.frame")
    expect_identical(colnames(out), c("element", "set", "extra1", "extra2"))
    expect_identical(dim(out), dim(relations))

    out <- as.data.frame.Sets(bs)
    expect_identical(colnames(out), c("element", "set", "extra1", "extra2"))
    expect_identical(dim(out), dim(relations))

})

# as.list() ----

test_that("as(Sets, \"list\") works", {

    bs <- Sets(relations)

    out <- as(bs, "list")
    expect_identical(lengths(out), c(set1 = 2L, set2 = 3L, set3 = 1L))
    out <- as.list(bs)
    expect_identical(lengths(out), c(set1 = 2L, set2 = 3L, set3 = 1L))

})

# as.matrix() ----

test_that("as(Sets, \"matrix\") works", {

    bs <- Sets(relations)

    expected.dim <- c(nElements(bs), nSets(bs))

    out <- as(bs, "matrix")
    expect_type(out, "logical")
    expect_identical(dim(out), expected.dim)

    out <- as.matrix(bs)
    expect_type(out, "logical")
    expect_identical(dim(out), expected.dim)

})

# as(list, "Sets") ----

test_that("as(list, \"Sets\") works", {

    bl <- list(set1=c("A", "B"), set2=c("B", "C"))

    out <- as(bl, "Sets")
    expect_s4_class(out, "Sets")
    expect_identical(length(out@relations), 4L)

    out <- as.Sets.list(bl, "Sets")
    expect_s4_class(out, "Sets")
    expect_identical(length(out@relations), 4L)

})

# as(matrix, "Sets") ----

test_that("as(matrix, \"Sets\") works", {

    nGenes <- 3
    nSets <- 2
    bm <- matrix(
        rep(c(TRUE, FALSE), nGenes),
        nrow=nGenes, ncol=nSets,
        dimnames=list(
            gene = paste0("gene", seq_len(nGenes)),
            set  = paste0("set", seq_len(nSets))
            )
        )

    out <- as(bm, "Sets")
    expect_s4_class(out, "Sets")
    expect_identical(length(out@relations), 3L) # 3 TRUE values above

    out <- as.Sets.matrix(bm, "Sets")
    expect_s4_class(out, "Sets")
    expect_identical(length(out@relations), 3L) # 3 TRUE values above

})

# setLengths() ----

test_that("setLengths(Sets) works", {

    bs <- Sets(relations)

    out <- setLengths(bs)
    expect_identical(out, c(set1 = 2L, set2 = 3L, set3 = 1L))

})

# elementLengths() ----

test_that("elementLengths(Sets) works", {

    bs <- Sets(relations)

    out <- elementLengths(bs)
    expect_identical(out, c(A = 1L, B = 2L, C = 1L, D = 1L, E = 1L))

})

