
# Setup ----

from <- c(5, 2, 3, 3, 3, 2)
to <- c(11, 15, 5, 4, 5, 11)
# Note: ontology and evidence are coerced to factor by the constructor, if needed
ontology <- c("BP", "BP", "BP", "MF", "MF", "CC")
evidence <- c("IEA", "IDA", "IEA", "IDA", "IEA", "IDA")

# Names will also be dropped
names(ontology) <- paste0("O", seq_along(ontology))
names(evidence) <- paste0("E", seq_along(evidence))

# GOHits() ----

test_that("GOHits constructor produces valid objects", {

    expect_message(
        GOHits(from, to, evidence, ontology, 7, 15),
        "Setting names(ontology) to NULL",
        fixed=TRUE
    )
    expect_message(
        GOHits(from, to, evidence, ontology, 7, 15),
        "Setting names(evidence) to NULL",
        fixed=TRUE
    )
    expect_message(
        GOHits(from, to, evidence, ontology, 7, 15),
        "Coercing ontology to factor",
        fixed=TRUE
    )
    expect_message(
        GOHits(from, to, evidence, ontology, 7, 15),
        "Coercing evidence to factor",
        fixed=TRUE
    )

    out <- GOHits(from, to, evidence, ontology, 7, 15)

    expect_s4_class(out, "GOHits")
    expect_length(out, length(from))
})

test_that("GOHits validity method identifies issues", {

    # Invalid evidence and ontology codes
    expect_warning(
        GOHits(from=c(1, 2), to=c(1, 2), evidence=c("NEW", "NEW"), ontology=c("BP", "BP"), 7, 15),
        "invalid evidence code, NA generated (See ?GOEvidenceCodes)",
        fixed=TRUE
    )
    expect_warning(
        GOHits(from=c(1, 2), to=c(1, 2), evidence=c("EXP", "EXP"), ontology=c("NEW", "NEW"), 7, 15),
        "invalid ontology code, NA generated (See ?GOOntologyCodes)",
        fixed=TRUE
    )

    # Unsupported ontology codes should be provided as factors with adequate levels
    ontology0 <- factor(rep("NEW", length(from)), "NEW")
    expect_warning(
        GOHits(from, to, evidence, ontology0, 7, 15),
        "invalid ontology code, NA generated (See ?GOOntologyCodes)",
        fixed=TRUE
    )
    evidence0 <- factor(rep("NEW", length(from)), "NEW")
    expect_warning(
        GOHits(from, to, evidence0, ontology, 7, 15),
        "invalid evidence code, NA generated (See ?GOEvidenceCodes)",
        fixed=TRUE
    )

})

# show() ----

test_that("show(GOHits) works", {

    gh <- GOHits(from, to, evidence, ontology, 7, 15)

    out <- show(gh)
    expect_null(out)

})

