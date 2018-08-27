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

    eml <- eml_test
    eml@dataset@abstract <- EML::read_eml("<abstract></abstract>")
    out <- qa_abstract(eml)
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

    eml <- eml_test
    eml@dataset@intellectualRights <- read_eml("<intellectualRights><para></para></intellectualRights>")
    out <- qa_creative_commons(eml)
    expect_equal(out$status, "FAILURE")

    # Test multiple intellectual rights
    eml@dataset@intellectualRights <- read_eml("<intellectualRights><para>rights1</para><para>rights2</para></intellectualRights>")
    out <- qa_creative_commons(eml)
    expect_equal(out$status, "FAILURE")
})

test_that("we can check for the prescence of a title", {
    out <- qa_title(eml_test)
    expect_equal(out$status, "SUCCESS")

    out <- qa_title("Random input title")
    expect_equal(out$status, "SUCCESS")

    out <- qa_title(character(0))
    expect_equal(out$status, "FAILURE")

    eml <- eml_test
    eml@dataset@title[[1]] <- EML::read_eml("<title></title>")
    out <- qa_title(eml)
    expect_equal(out$status, "FAILURE")
})

test_that("we can check for funding numbers", {
    out <- qa_award_number_present(eml_test)
    expect_equal(out$status, "SUCCESS")

    out <- qa_award_number_present("1234567")
    expect_equal(out$status, "SUCCESS")

    eml <- eml_test
    eml@dataset@project@funding <- EML::read_eml("<funding></funding>")
    out <- qa_award_number_present(eml)
    expect_equal(out$status, "FAILURE")

    out <- qa_award_number_present("")
    expect_equal(out$status, "FAILURE")
})
