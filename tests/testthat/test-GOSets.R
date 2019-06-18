
# Setup ----

# Fetch a sample of GO annotations

base_sets <- import(org.Hs.egGO)
relations <- as.data.frame(head(base_sets))
# Set rownames to check that they are dropped
rownames(relations) <- paste0("row", seq_len(nrow(relations)))

# GOSets() ----

test_that("GOSets constructor produces valid objects", {

    expect_error(
        GOSets(relations[, c("element", "set", "ontology")]),
        "colnames(relations) must include \"evidence\"",
        fixed=TRUE
    )

    expect_error(
        GOSets(relations[, c("element", "set", "evidence")]),
        "colnames(relations) must include \"ontology\"",
        fixed=TRUE
    )

    expect_message(
        GOSets(relations),
        "Setting rownames(relations) to NULL",
        fixed=TRUE
    )

    out <- GOSets(relations=relations)

    expect_s4_class(out, "GOSets")
})

test_that("GOSets validity method identifies issues", {

    gs <- GOSets(relations=relations)

    # Cannot remove "evidence" or "ontology" metadata from a FuzzySets
    expect_error(
        mcols(relations(gs))[["evidence"]] <- NULL,
        "colnames(mcols(relations)) must include \"evidence\"",
        fixed=TRUE
    )
    expect_error(
        mcols(relations(gs))[["ontology"]] <- NULL,
        "colnames(mcols(relations)) must include \"ontology\"",
        fixed=TRUE
    )

    # unsupported evidence code (as.character)
    relations0 <- relations
    suppressWarnings(relations0$evidence[1] <- "NEW")
    expect_warning(
        GOSets(relations0),
        "invalid evidence code, NA generated (See ?GOEvidenceCodes)",
        fixed=TRUE
    )
    # unsupported ontology code (as.character)
    relations0 <- relations
    suppressWarnings(relations0$ontology[1] <- "NEW")
    expect_warning(
        GOSets(relations0),
        "invalid ontology code, NA generated (See ?GOOntologyCodes)",
        fixed=TRUE
    )

    # unsupported evidence code (as.factor)
    relations0 <- relations
    suppressWarnings(relations0$evidence[1] <- "NEW")
    relations0$evidence <- as.factor(relations0$evidence)
    expect_warning(
        GOSets(relations0),
        "invalid evidence code, NA generated (See ?GOEvidenceCodes)",
        fixed=TRUE
    )
    # unsupported ontology code (as.factor)
    relations0 <- relations
    suppressWarnings(relations0$ontology[1] <- "NEW")
    relations0$ontology <- as.factor(relations0$ontology)
    expect_warning(
        GOSets(relations0),
        "invalid ontology code, NA generated (See ?GOOntologyCodes)",
        fixed=TRUE
    )

})

# evidence<-() ----

test_that("evidence(GOSets) <- value works", {

    gs <- GOSets(relations)

    # Add a supported level
    gs0 <- gs
    newValues <- rep("EXP", length(gs))
    evidence(gs) <- newValues
    expect_identical(evidence(gs), factor(newValues, names(GOEvidenceCodes)))
    # Add an unsupported level
    gs0 <- gs
    expect_warning(
        evidence(gs0)[1] <- "NEW",
        "invalid factor level, NA generated"
    )

})

# ontology<-() ----

test_that("ontology(GOSets) <- value works", {

    gs <- GOSets(relations)

    # Add a supported level
    gs0 <- gs
    newValues <- rep("CC", length(gs))
    ontology(gs) <- newValues
    expect_identical(ontology(gs), factor(newValues, names(GOOntologyCodes)))
    # Add an unsupported level
    gs0 <- gs
    expect_warning(
        ontology(gs0)[1] <- "NEW",
        "invalid factor level, NA generated"
    )

})

# subset() ----

test_that("subset(GOSets) works", {

    gs <- GOSets(relations)

    out <- subset(gs, ontology == "BP" & evidence == "TAS")
    expect_s4_class(out, "GOSets")
    expect_true(all(ontology(out) == "BP" & evidence(out) == "TAS"))

})

# show() ----

test_that("show(GOSets) works", {

    gs <- GOSets(relations)

    out <- show(gs)
    expect_identical(out, NULL)

})

