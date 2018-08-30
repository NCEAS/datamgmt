context("eml_data_qa")

eml_test <- EML::read_eml(system.file("dummy_meta_full.xml", package = "datamgmt"))

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


test_that("creator is present", {
    input <- 7
    expect_error(qa_creator(input))

    out <- qa_creator(eml_test)
    expect_equal(out$status, "SUCCESS")

    out2 <- qa_creator(eml_test@dataset@creator)
    expect_equal(out2$status, "SUCCESS")

    eml_test@dataset@creator[[1]] <- NULL
    out3 <- qa_creator(eml_test)
    expect_equal(out3$status, "FAILURE")
})


test_that("creator info is present", {
    input <- 7
    expect_error(qa_creator_info(input))

    out <- qa_creator_info(eml_test)
    expect_equal(out$status, "SUCCESS")

    eml_test@dataset@creator[[1]]@address[[1]] <- NULL
    out2 <- qa_creator_info(eml_test)
    expect_equal(out2$status, "FAILURE")

    eml_test@dataset@creator[[1]]@electronicMailAddress[[1]]@.Data <- "notanemail"
    out3 <- qa_creator_info(eml_test)
    expect_equal(out3$status, "FAILURE")

    eml_test@dataset@creator[[1]]@electronicMailAddress[[1]] <- NULL
    out4 <- qa_creator_info(eml_test)
    expect_equal(out4$status, "FAILURE")

    eml_test@dataset@creator[[1]]@userId[[1]]@.Data <- "notanORCID"
    out5 <- qa_creator_info(eml_test)
    expect_equal(out5$status, "FAILURE")

    eml_test@dataset@creator[[1]]@userId[[1]] <- NULL
    out6 <- qa_creator_info(eml_test)
    expect_equal(out6$status, "FAILURE")

    eml_test@dataset@creator[[1]] <- NULL
    out7 <- qa_creator_info(eml_test)
    expect_equal(out7$status, "SKIP")
})


test_that("contact is present", {
    input <- 7
    expect_error(qa_contact(input))

    out <- qa_contact(eml_test)
    expect_equal(out$status, "SUCCESS")

    out2 <- qa_contact(eml_test@dataset@contact)
    expect_equal(out2$status, "SUCCESS")

    eml_test@dataset@contact[[4]] <- NULL
    eml_test@dataset@contact[[3]] <- NULL
    eml_test@dataset@contact[[2]] <- NULL
    eml_test@dataset@contact[[1]] <- NULL
    out3 <- qa_contact(eml_test)
    expect_equal(out3$status, "FAILURE")
})


test_that("contact info is present", {
    input <- 7
    expect_error(qa_contact_info(input))

    out <- qa_contact_info(eml_test)
    expect_equal(out$status, "SUCCESS")

    eml_test@dataset@contact[[1]]@address[[1]] <- NULL
    out2 <- qa_contact_info(eml_test)
    expect_equal(out2$status, "FAILURE")

    eml_test@dataset@contact[[1]]@electronicMailAddress[[1]]@.Data <- "notanemail"
    out3 <- qa_contact_info(eml_test)
    expect_equal(out3$status, "FAILURE")

    eml_test@dataset@contact[[1]]@electronicMailAddress[[1]] <- NULL
    out4 <- qa_contact_info(eml_test)
    expect_equal(out4$status, "FAILURE")

    eml_test@dataset@contact[[1]]@userId[[1]]@.Data <- "notanORCID"
    out5 <- qa_contact_info(eml_test)
    expect_equal(out5$status, "FAILURE")

    eml_test@dataset@contact[[1]]@userId[[1]] <- NULL
    out6 <- qa_contact_info(eml_test)
    expect_equal(out6$status, "FAILURE")

    eml_test@dataset@contact[[4]] <- NULL
    eml_test@dataset@contact[[3]] <- NULL
    eml_test@dataset@contact[[2]] <- NULL
    eml_test@dataset@contact[[1]] <- NULL
    out7 <- qa_contact_info(eml_test)
    expect_equal(out7$status, "SKIP")
})


test_that("geographic coverage description is present", {
    input <- 7
    expect_error(qa_geographic_desc(input))

    out <- qa_geographic_desc(eml_test)
    expect_equal(out$status, "SUCCESS")

    out2 <- qa_geographic_desc(eml_test@dataset@coverage@geographicCoverage)
    expect_equal(out2$status, "SUCCESS")

    eml_test@dataset@coverage@geographicCoverage[[1]] <- EML::read_eml("<geographicCoverage></geographicCoverage>")
    out3 <- qa_geographic_desc(eml_test)
    expect_equal(out3$status, "FAILURE")

    eml_test@dataset@coverage@geographicCoverage@.Data <- list(NULL)
    out4 <- qa_geographic_desc(eml_test)
    expect_equal(out4$status, "FAILURE")
})


test_that("geographic coverage bounding coordinates are present", {
    input <- 7
    expect_error(qa_geographic_coord(input))

    out <- qa_geographic_coord(eml_test)
    expect_equal(out$status, "SUCCESS")

    out2 <- qa_geographic_coord(eml_test@dataset@coverage@geographicCoverage)
    expect_equal(out2$status, "SUCCESS")

    eml_test@dataset@coverage@geographicCoverage[[1]] <- EML::read_eml("<geographicCoverage></geographicCoverage>")
    out3 <- qa_geographic_coord(eml_test)
    expect_equal(out3$status, "FAILURE")

    one <- "<geographicCoverage><boundingCoordinates><westBoundingCoordinate>70</westBoundingCoordinate></boundingCoordinates></geographicCoverage>"
    eml_test@dataset@coverage@geographicCoverage[[1]] <- EML::read_eml(one)
    out4 <- qa_geographic_coord(eml_test)
    expect_equal(out4$status, "FAILURE")

    eml_test@dataset@coverage@geographicCoverage[[2]] <- NULL
    eml_test@dataset@coverage@geographicCoverage[[1]] <- NULL
    out5 <- qa_geographic_coord(eml_test)
    expect_equal(out5$status, "SKIP")
})


test_that("geographic coverage intersects with Arctic", {
    out <- qa_geographic_arctic("test")
    expect_equal(out$status, "FAILURE")

    out2 <- qa_geographic_arctic(eml_test)
    expect_equal(out2$status, "SUCCESS")

    out3 <- qa_geographic_arctic(eml_test@dataset@coverage@geographicCoverage)
    expect_equal(out3$status, "SUCCESS")

    out4 <- qa_geographic_arctic(50)
    expect_equal(out4$status, "SUCCESS")

    out5 <- qa_geographic_arctic(30)
    expect_equal(out5$status, "FAILURE")

    out6 <- qa_geographic_arctic(100)
    expect_equal(out6$status, "FAILURE")

    eml_test@dataset@coverage@geographicCoverage[[1]]@boundingCoordinates@northBoundingCoordinate@.Data <- "test"
    out7 <- qa_geographic_arctic(eml_test)
    expect_equal(out7$status, "FAILURE")

    eml_test@dataset@coverage@geographicCoverage[[1]]@boundingCoordinates@northBoundingCoordinate@.Data <- "30"
    eml_test@dataset@coverage@geographicCoverage[[2]]@boundingCoordinates@northBoundingCoordinate@.Data <- "30"
    out8 <- qa_geographic_arctic(eml_test)
    expect_equal(out8$status, "FAILURE")

    eml_test@dataset@coverage@geographicCoverage[[2]] <- NULL
    eml_test@dataset@coverage@geographicCoverage[[1]] <- NULL
    out9 <- qa_geographic_arctic(eml_test)
    expect_equal(out9$status, "SKIP")
})


test_that("keywords are present", {
    input <- 7
    expect_error(qa_keywords(input))

    out <- qa_keywords(eml_test)
    expect_equal(out$status, "FAILURE")

    eml_test@dataset@keywordSet[[1]] <- EML::read_eml("<keywordSet><keyword>keyword</keyword></keywordSet>")
    out2 <- qa_keywords(eml_test)
    expect_equal(out2$status, "SUCCESS")
})


test_that("title length is sufficient", {
    input <- 7
    expect_error(qa_title_length(input))

    out <- qa_title_length(eml_test)
    expect_equal(out$status, "FAILURE")

    out2 <- qa_title_length(eml_test@dataset@title)
    expect_equal(out2$status, "FAILURE")

    out3 <- qa_title_length("test")
    expect_equal(out3$status, "FAILURE")

    eml_test@dataset@title[[1]]@.Data <- "A Longer Title for Dummy Package"
    out4 <- qa_title_length(eml_test)
    expect_equal(out4$status, "FAILURE")

    eml_test@dataset@title[[1]]@.Data <- "An Even Longer Title for Dummy Package"
    out5 <- qa_title_length(eml_test)
    expect_equal(out5$status, "SUCCESS")

    eml_test@dataset@title[[1]]@.Data <- paste0(rep("Title", 25), collapse = " ")
    out6 <- qa_title_length(eml_test)
    expect_equal(out6$status, "FAILURE")

    eml_test@dataset@title[[1]] <- NULL
    out7 <- qa_title_length(eml_test)
    expect_equal(out7$status, "SKIP")
})
