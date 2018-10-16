context("eml_data_qa")

eml_test <- EML::read_eml(system.file("dummy_meta_full.xml", package = "datamgmt"))
eml_test2 <- EML::read_eml(system.file("dummy_eml_w_attributes.xml", package = "datamgmt"))

# TODO: add tests for qa_attributes and qa_package


test_that("title is present with sufficient length", {
    expect_error(qa_title(7))

    out1 <- qa_title(character(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_title("Random input title")
    expect_equal(out2$status, "FAILURE")

    out3 <- qa_title("Random input title with more words")
    expect_equal(out3$status, "FAILURE")

    out4 <- qa_title("Random input title with even more words")
    expect_equal(out4$status, "SUCCESS")

    out5 <- qa_title(c("test", "test"))
    expect_equal(out5$status, "SUCCESS")

    out6 <- qa_title(eml_test)
    expect_equal(out6$status, "FAILURE")

    out7 <- qa_title(eml_test@dataset@title)
    expect_equal(out7$status, "FAILURE")

    eml_test@dataset@title[[1]]@.Data <- "A Longer Title for Dummy Package"
    out8 <- qa_title(eml_test)
    expect_equal(out8$status, "FAILURE")

    eml_test@dataset@title[[1]]@.Data <- "An Even Longer Title for Dummy Package"
    out9 <- qa_title(eml_test)
    expect_equal(out9$status, "SUCCESS")

    eml_test@dataset@title[[2]] <- eml_test@dataset@title[[1]]
    out10 <- qa_title(eml_test)
    expect_equal(out10$status, "SUCCESS")

    eml_test@dataset@title[[2]] <- NULL
    eml_test@dataset@title[[1]]@.Data <- paste0(rep("Title", 25), collapse = " ")
    out11 <- qa_title(eml_test)
    expect_equal(out11$status, "FAILURE")

    eml_test@dataset@title[[1]] <- NULL
    out12 <- qa_title(eml_test)
    expect_equal(out12$status, "FAILURE")
})


test_that("publication date is present with correct format", {
    expect_error(qa_pubDate(7))

    out1 <- qa_pubDate(character(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_pubDate("20")
    expect_equal(out2$status, "FAILURE")

    out3 <- qa_pubDate("2018")
    expect_equal(out3$status, "SUCCESS")

    out4 <- qa_pubDate("test")
    expect_equal(out4$status, "FAILURE")

    out5 <- qa_pubDate("2018-09")
    expect_equal(out5$status, "FAILURE")

    out6 <- qa_pubDate("2018-09-09")
    expect_equal(out6$status, "SUCCESS")

    out7 <- qa_pubDate("aabbccddee")
    expect_equal(out7$status, "FAILURE")

    out8 <- qa_pubDate("aa bb cc")
    expect_equal(out8$status, "FAILURE")

    out9 <- qa_pubDate("")
    expect_equal(out9$status, "FAILURE")

    out10 <- qa_pubDate(eml_test)
    expect_equal(out10$status, "SUCCESS")

    out11 <- qa_pubDate(eml_test)
    expect_equal(out11$status, "SUCCESS")

    out12 <- qa_pubDate(eml_test@dataset@pubDate)
    expect_equal(out12$status, "SUCCESS")

    eml_test@dataset@pubDate <- EML::read_eml("<pubDate>2017-09-15</pubDate>")
    out13 <- qa_pubDate(eml_test)
    expect_equal(out13$status, "SUCCESS")

    eml_test@dataset@pubDate <- EML::read_eml("<pubDate></pubDate>")
    out14 <- qa_pubDate(eml_test)
    expect_equal(out14$status, "FAILURE")

    eml_test@dataset <- EML::read_eml("<dataset></dataset>")
    out15 <- qa_pubDate(eml_test)
    expect_equal(out15$status, "FAILURE")
})


test_that("abstract is present with sufficient length", {
    expect_error(qa_abstract(7))

    out1 <- qa_abstract(character(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_abstract("Exists but not sufficient length.")
    expect_equal(out2$status, "FAILURE")

    out3 <- qa_abstract(c("test", "test"))
    expect_equal(out3$status, "FAILURE")

    out4 <- qa_abstract(eml_test)
    expect_equal(out4$status, "FAILURE")

    out5 <- qa_abstract(eml_test@dataset@abstract)
    expect_equal(out5$status, "FAILURE")

    eml_test@dataset@abstract@para[[2]] <- eml_test@dataset@abstract@para[[1]]
    out6 <- qa_abstract(eml_test)
    expect_equal(out6$status, "FAILURE")

    out7 <- qa_abstract(eml_test@dataset@abstract)
    expect_equal(out7$status, "FAILURE")

    abstract_text <- paste0(rep("word", 100), collapse = " ")
    out8 <- qa_abstract(abstract_text)
    expect_equal(out8$status, "SUCCESS")

    eml_test@dataset@abstract <- EML::read_eml(paste0("<abstract><para>", abstract_text, "</para></abstract>"))
    out9 <- qa_abstract(eml_test)
    expect_equal(out9$status, "SUCCESS")

    eml_test@dataset <- EML::read_eml("<dataset></dataset>")
    out10 <- qa_abstract(eml_test)
    expect_equal(out10$status, "FAILURE")
})

test_that("qa_abstract fails when < 100 words", {
    out <- qa_abstract(eml_test)
    expect_equal(out$status, "FAILURE")

    eml <- eml_test
    eml@dataset@abstract <- EML::read_eml("<abstract></abstract>")
    out <- qa_abstract(eml)
    expect_equal(out$status, "FAILURE")
})

test_that("keywords are present", {
    expect_error(qa_keywordSet(7))

    out1 <- qa_keywordSet(character(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_keywordSet("test")
    expect_equal(out2$status, "SUCCESS")

    out3 <- qa_keywordSet(eml_test)
    expect_equal(out3$status, "FAILURE")

    keywords <- EML::read_eml("<keywordSet><keyword>keyword</keyword></keywordSet>")
    eml_test@dataset@keywordSet[[1]] <- keywords
    out4 <- qa_keywordSet(eml_test)
    expect_equal(out4$status, "SUCCESS")

    keywords <- EML::read_eml("<keywordSet><keyword>keyword1</keyword><keyword>keyword2</keyword></keywordSet>")
    eml_test@dataset@keywordSet[[1]] <- keywords
    out5 <- qa_keywordSet(eml_test)
    expect_equal(out5$status, "SUCCESS")
})


test_that("intellectual rights are present", {
    expect_error(qa_intellectualRights(7))

    out1 <- qa_intellectualRights(character(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_intellectualRights("Exists but not CC-BY or CC-0.")
    expect_equal(out2$status, "FAILURE")

    out3 <- qa_intellectualRights(c("test", "test"))
    expect_equal(out3$status, "FAILURE")

    out4 <- qa_intellectualRights(eml_test)
    expect_equal(out4$status, "SUCCESS")

    out5 <- qa_intellectualRights(eml_test@dataset@intellectualRights)
    expect_equal(out5$status, "SUCCESS")

    cc_0 <- "<intellectualRights>CC-0: https://creativecommons.org/publicdomain/zero/1.0/</intellectualRights>"
    eml_test@dataset@intellectualRights <- EML::read_eml(cc_0)
    out6 <- qa_intellectualRights(eml_test)
    expect_equal(out6$status, "SUCCESS")

    cc_by <- "<intellectualRights>CC-BY: https://creativecommons.org/licenses/by/4.0/</intellectualRights>"
    eml_test@dataset@intellectualRights <- EML::read_eml(cc_by)
    out7 <- qa_intellectualRights(eml_test)
    expect_equal(out7$status, "SUCCESS")

    not_cc <- "<intellectualRights>Exists but not CC-BY or CC-0.</intellectualRights>"
    eml_test@dataset@intellectualRights <- EML::read_eml(not_cc)
    out8 <- qa_intellectualRights(eml_test)
    expect_equal(out8$status, "FAILURE")
})


test_that("creator is present", {
    expect_error(qa_creator(7))

    out1 <- qa_creator(character(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_creator("test")
    expect_equal(out2$status, "SUCCESS")

    out3 <- qa_creator(eml_test)
    expect_equal(out3$status, "SUCCESS")

    out4 <- qa_creator(eml_test@dataset@creator)
    expect_equal(out4$status, "SUCCESS")

    eml_test@dataset@creator[[2]] <- eml_test@dataset@creator[[1]]
    out5 <- qa_creator(eml_test)
    expect_equal(out5$status, "SUCCESS")

    eml_test@dataset@creator[[2]] <- NULL
    eml_test@dataset@creator[[1]] <- NULL
    out6 <- qa_creator(eml_test)
    expect_equal(out6$status, "FAILURE")
})

test_that("creator info is present", {
    expect_error(qa_creator_info(7))

    expect_error(qa_creator_info("test"))

    out1 <- qa_creator_info(eml_test)
    expect_equal(out1$status, "SUCCESS")

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
    expect_equal(out7$status, "FAILURE")
})

test_that("contact is present", {
    expect_error(qa_contact(7))

    out1 <- qa_contact(character(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_contact("test")
    expect_equal(out2$status, "SUCCESS")

    out3 <- qa_contact(eml_test)
    expect_equal(out3$status, "SUCCESS")

    out4 <- qa_contact(eml_test@dataset@contact)
    expect_equal(out4$status, "SUCCESS")

    eml_test@dataset@contact[[4]] <- NULL
    eml_test@dataset@contact[[3]] <- NULL
    eml_test@dataset@contact[[2]] <- NULL
    out5 <- qa_contact(eml_test)
    expect_equal(out5$status, "SUCCESS")

    eml_test@dataset@contact[[1]] <- NULL
    out6 <- qa_contact(eml_test)
    expect_equal(out6$status, "FAILURE")
})

test_that("contact info is present", {
    expect_error(qa_contact_info(7))

    expect_error(qa_contact_info("test"))

    out1 <- qa_contact_info(eml_test)
    expect_equal(out1$status, "SUCCESS")

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
    expect_equal(out7$status, "FAILURE")
})


test_that("geographic coverage is present with description", {
    expect_error(qa_geographic(7))

    out1 <- qa_geographic(character(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_geographic(eml_test)
    expect_equal(out2$status, "SUCCESS")

    out3 <- qa_geographic(eml_test@dataset@coverage@geographicCoverage)
    expect_equal(out3$status, "SUCCESS")

    eml_test@dataset@coverage@geographicCoverage[[1]] <- EML::read_eml("<geographicCoverage></geographicCoverage>")
    out4 <- qa_geographic(eml_test)
    expect_equal(out4$status, "FAILURE")

    eml_test@dataset@coverage@geographicCoverage@.Data <- list(NULL)
    out5 <- qa_geographic(eml_test)
    expect_equal(out5$status, "FAILURE")
})


test_that("geographic coverage bounding coordinates are present", {
    expect_error(qa_geographic_coord(7))

    out1 <- qa_geographic_coord(character(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_geographic_coord("test")
    expect_equal(out2$status, "FAILURE")

    out3 <- qa_geographic_coord(eml_test)
    expect_equal(out3$status, "SUCCESS")

    out4 <- qa_geographic_coord(eml_test@dataset@coverage@geographicCoverage)
    expect_equal(out4$status, "SUCCESS")

    eml_test@dataset@coverage@geographicCoverage[[1]] <- EML::read_eml("<geographicCoverage></geographicCoverage>")
    out5 <- qa_geographic_coord(eml_test)
    expect_equal(out5$status, "FAILURE")

    one <- "<geographicCoverage><boundingCoordinates><westBoundingCoordinate>70</westBoundingCoordinate></boundingCoordinates></geographicCoverage>"
    eml_test@dataset@coverage@geographicCoverage[[1]] <- EML::read_eml(one)
    out6 <- qa_geographic_coord(eml_test)
    expect_equal(out6$status, "FAILURE")

    eml_test@dataset@coverage@geographicCoverage[[2]] <- NULL
    eml_test@dataset@coverage@geographicCoverage[[1]] <- NULL
    out7 <- qa_geographic_coord(eml_test)
    expect_equal(out7$status, "FAILURE")
})


test_that("geographic coverage intersects with Arctic", {
    out1 <- qa_geographic_arctic(numeric(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_geographic_arctic(character(0))
    expect_equal(out2$status, "FAILURE")

    out3 <- qa_geographic_arctic("test")
    expect_equal(out3$status, "FAILURE")

    out4 <- qa_geographic_arctic(eml_test)
    expect_equal(out4$status, "SUCCESS")

    out5 <- qa_geographic_arctic(eml_test@dataset@coverage@geographicCoverage)
    expect_equal(out5$status, "SUCCESS")

    out6 <- qa_geographic_arctic(50)
    expect_equal(out6$status, "SUCCESS")

    out7 <- qa_geographic_arctic(30)
    expect_equal(out7$status, "FAILURE")

    out8 <- qa_geographic_arctic(100)
    expect_equal(out8$status, "FAILURE")

    eml_test@dataset@coverage@geographicCoverage[[1]]@boundingCoordinates@northBoundingCoordinate@.Data <- "test"
    out9 <- qa_geographic_arctic(eml_test)
    expect_equal(out9$status, "FAILURE")

    eml_test@dataset@coverage@geographicCoverage[[1]]@boundingCoordinates@northBoundingCoordinate@.Data <- "30"
    eml_test@dataset@coverage@geographicCoverage[[2]]@boundingCoordinates@northBoundingCoordinate@.Data <- "30"
    out10 <- qa_geographic_arctic(eml_test)
    expect_equal(out10$status, "FAILURE")

    eml_test@dataset@coverage@geographicCoverage[[2]] <- NULL
    eml_test@dataset@coverage@geographicCoverage[[1]] <- NULL
    out11 <- qa_geographic_arctic(eml_test)
    expect_equal(out11$status, "FAILURE")
})


test_that("temporal coverage is present", {
    expect_error(qa_temporal(7))

    out1 <- qa_temporal(character(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_temporal("test")
    expect_equal(out2$status, "FAILURE")

    out3 <- qa_temporal(eml_test)
    expect_equal(out3$status, "SUCCESS")

    out4 <- qa_temporal(eml_test@dataset@coverage@temporalCoverage)
    expect_equal(out4$status, "SUCCESS")

    eml_test@dataset@coverage@temporalCoverage[[2]] <- eml_test@dataset@coverage@temporalCoverage[[1]]
    out5 <- qa_temporal(eml_test)
    expect_equal(out5$status, "SUCCESS")

    out6 <- qa_temporal(eml_test@dataset@coverage@temporalCoverage)
    expect_equal(out6$status, "SUCCESS")

    eml_test@dataset@coverage <- EML::read_eml("<coverage></coverage>")
    out7 <- qa_temporal(eml_test)
    expect_equal(out7$status, "FAILURE")
})


test_that("taxonomic coverage is present", {
    expect_error(qa_taxonomic(7))

    out1 <- qa_taxonomic(character(0))
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_taxonomic("test")
    expect_equal(out2$status, "FAILURE")

    out3 <- qa_taxonomic(eml_test)
    expect_equal(out3$status, "SUCCESS")

    out4 <- qa_taxonomic(eml_test@dataset@coverage@taxonomicCoverage)
    expect_equal(out4$status, "SUCCESS")

    eml_test@dataset@coverage@taxonomicCoverage[[2]] <- eml_test@dataset@coverage@taxonomicCoverage[[1]]
    out5 <- qa_taxonomic(eml_test)
    expect_equal(out5$status, "SUCCESS")

    out6 <- qa_taxonomic(eml_test@dataset@coverage@taxonomicCoverage)
    expect_equal(out6$status, "SUCCESS")

    eml_test@dataset@coverage <- EML::read_eml("<coverage></coverage>")
    out7 <- qa_taxonomic(eml_test)
    expect_equal(out7$status, "FAILURE")
})


test_that("methods are present and complete", {
    expect_error(qa_methods(7))

    expect_error(qa_methods("test"))

    out1 <- qa_methods(eml_test)
    expect_equal(out1$status, "SUCCESS")

    out2 <- qa_methods(eml_test@dataset@methods)
    expect_equal(out2$status, "SUCCESS")

    mstep <- eml_test@dataset@methods@methodStep[[1]]
    eml_test@dataset@methods@methodStep[[2]] <- NULL
    eml_test@dataset@methods@methodStep[[1]] <- NULL
    out3 <- qa_methods(eml_test)
    expect_equal(out3$status, "FAILURE")

    eml_test@dataset@methods@methodStep[[1]] <- mstep
    study_extent <- eml_test@dataset@methods@sampling[[1]]@studyExtent
    eml_test@dataset@methods@sampling[[1]]@studyExtent@description[[1]] <- NULL
    out4 <- qa_methods(eml_test)
    expect_equal(out4$status, "FAILURE")

    eml_test@dataset@methods@sampling[[1]]@studyExtent <- study_extent
    eml_test@dataset@methods@sampling[[1]]@samplingDescription@para[[1]] <- NULL
    out5 <- qa_methods(eml_test)
    expect_equal(out5$status, "FAILURE")

    eml_test@dataset@methods@sampling[[1]] <- NULL
    out6 <- qa_methods(eml_test)
    expect_equal(out6$status, "FAILURE")

    eml_test@dataset@methods <- EML::read_eml("<methods></methods>")
    out7 <- qa_methods(eml_test)
    expect_equal(out7$status, "FAILURE")

    eml_test@dataset <- EML::read_eml("<dataset></dataset>")
    out8 <- qa_methods(eml_test)
    expect_equal(out8$status, "FAILURE")
})


test_that("project information is present and complete", {
    expect_error(qa_project(7))

    expect_error(qa_project("test"))

    out1 <- qa_project(eml_test)
    expect_equal(out1$status, "FAILURE")

    out2 <- qa_project(eml_test@dataset@project)
    expect_equal(out2$status, "FAILURE")

    project <- list(eml_test@dataset@project)
    project[[1]]@abstract <- EML::read_eml("<abstract><para>Random abstract.</para></abstract>")
    eml_test@dataset@project <- project[[1]]
    out3 <- qa_project(eml_test)
    expect_equal(out3$status, "SUCCESS")

    eml_test@dataset <- EML::read_eml("<dataset></dataset>")
    out4 <- qa_project(eml_test)
    expect_equal(out4$status, "FAILURE")
})


test_that("entity name and description are present and complete", {
    expect_error(qa_entity(7))

    expect_error(qa_entity("test"))

    out1 <- qa_entity(eml_test)
    expect_equal(out1$status, "SUCCESS")

    out2 <- qa_entity(eml_test2)
    expect_equal(out2$status, "FAILURE")

    eml_test2@dataset@otherEntity[[1]]@entityDescription@.Data <- "test"
    out3 <- qa_entity(eml_test2)
    expect_equal(out3$status, "SUCCESS")

    entity_name <- paste0(rep("a", 101), collapse = "")
    eml_test2@dataset@dataTable[[1]]@entityName <- entity_name
    out4 <- qa_entity(eml_test2)
    expect_equal(out4$status, "FAILURE")
})


test_that("entities have not been duplicated", {
    expect_error(qa_entity_dup(7))

    expect_error(qa_entity_dup("test"))

    out1 <- qa_entity_dup(eml_test)
    expect_equal(out1$status, "SUCCESS")

    out2 <- qa_entity_dup(eml_test2)
    expect_equal(out2$status, "SUCCESS")

    eml_test2@dataset@dataTable[[1]]@id@.Data <- eml_test2@dataset@otherEntity[[1]]@id@.Data
    out2 <- qa_entity_dup(eml_test2)
    expect_equal(out2$status, "FAILURE")
})


test_that("physical is present and complete", {
    expect_error(qa_physical(7))

    expect_error(qa_physical("test"))

    out1 <- qa_physical(eml_test)
    expect_equal(out1$status, "SUCCESS")

    eml_test2@dataset@dataTable[[1]]@id@.Data <- "test"
    eml_test2@dataset@otherEntity[[1]]@id@.Data <- "test"
    out2 <- qa_physical(eml_test2)
    expect_equal(out2$status, "FAILURE")

    eml_test2@dataset@dataTable[[1]]@entityName <- eml_test2@dataset@dataTable[[1]]@physical[[1]]@objectName@.Data
    url_pid <- stringr::str_split(eml_test2@dataset@dataTable[[1]]@physical[[1]]@distribution[[1]]@online@url@.Data, "(?=urn.)", simplify = TRUE)[[2]]
    eml_test2@dataset@dataTable[[1]]@id@.Data <- url_pid
    url_pid <- stringr::str_split(eml_test2@dataset@otherEntity[[1]]@physical[[1]]@distribution[[1]]@online@url@.Data, "(?=urn.)", simplify = TRUE)[[2]]
    eml_test2@dataset@otherEntity[[1]]@id@.Data <- url_pid
    out3 <- qa_physical(eml_test2)
    expect_equal(out3$status, "SUCCESS")

    eml_test2@dataset@dataTable[[1]]@physical[[1]] <- NULL
    out4 <- qa_physical(eml_test2)
    expect_equal(out4$status, "FAILURE")
})


test_that("qa_eml only accepts eml input", {
    expect_error(qa_eml(7))

    expect_error(qa_eml("test"))

    expect_error(qa_eml(list("test")))

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
    out1 <- qa_eml(eml_test)
    expect_equal(out1$qa_title$status, "FAILURE")
    expect_equal(out1$qa_abstract$status, "FAILURE")
    expect_equal(out1$qa_keywordSet$status, "FAILURE")
    expect_equal(out1$qa_project$status, "FAILURE")
})
