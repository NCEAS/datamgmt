context("eml_data_qa")

eml_test <- EML::read_eml(system.file("dummy_meta_full.xml", package="datamgmt"))

test_that("we can check if an abstract is >= 100 words.", {
    eml <- eml_test
    # Set to 100 characters.  Both the eml and text input should pass
    abstract_text <- paste0(rep("word", 100), collapse = " ")
    eml@dataset@abstract <- new("abstract", .Data = abstract_text)

    out <- qa_abstract(eml)
    expect_equal(out$status, "SUCCESS")
    out <- qa_abstract(abstract_text)
    expect_equal(out$status, "SUCCESS")
})

test_that("qa_abstract fails when < 100 words", {
    out <- qa_abstract(eml_test)
    expect_equal(out$status, "FAILURE")
})

test_that("we can check for a creative commons license", {
    out <- qa_creative_commons(eml_test)
    expect_equal(out$status, "SUCCESS")

    cc <- "This work is dedicated to the public domain under the Creative Commons Universal 1.0 Public Domain Dedication. To view a copy of this dedication, visit https://creativecommons.org/publicdomain/zero/1.0/"
    out <- qa_creative_commons(cc)
    expect_equal(out$status, "SUCCESS")

    cc_fail <- "This work has no license"
    out <- qa_creative_commons(cc_fail)
    expect_equal(out$status, "FAILURE")
})

test_that("we can check for the prescence of a title", {
    out <- qa_title(eml_test)
    expect_equal(out$status, "SUCCESS")

    out <- qa_title("Random input title")
    expect_equal(out$status, "SUCCESS")

    out <- qa_title(character(0))
    expect_equal(out$status, "FAILURE")
})
