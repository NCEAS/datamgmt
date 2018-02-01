library(testthat)
library(arcticdatautils)
library(dataone)

context("Add creator id")

#for add_creator_id
    #misspelled surname will give an error
    #surname is not case sensitive
    #orcid is added to correct location in EML
    #ref id is added to correct location in EML

cnTest <- dataone::CNode('STAGING')
mnTest <- dataone::getMNode(cnTest,'urn:node:mnTestARCTIC')

eml_pid <- "urn:uuid:dd4e18fe-3c5b-488a-8a43-5bcd1ba96500"
# alternative if there are issues with given metadata pid: eml_pid <- arcticdatautils::create_dummy_metadata(mnTest)

eml1 <- EML::read_eml(rawToChar(getObject(mnTest, eml_pid)))

test_that("misspelled surname will give an error", {
  expect_error(add_creator_id(eml1, surname = "mecm"))
})

test_that("surname is not case sensitive", {
  expect_equal(add_creator_id(eml1, 
                              id = "mecum_id",
                              surname = "mecum"),
               add_creator_id(eml1, 
                              id = "mecum_id",
                              surname = "MECUM"))
})

test_that("orcid is added to correct location in EML", {
  orcid_test <- "INSERT_ORCID_HERE"
  eml_new <- add_creator_id(eml1, 
                            orcid = orcid_test)
  
  expect_equal(orcid_test,
               eml_new@dataset@creator[[1]]@userId)###NEEDS EDITING
})

test_that("ref id is added to correct location in EML", {
  id_test <- "SOME_ID"
  eml_new <- add_creator_id(eml1,
                            id = id_test)
  expect_equal(id_test, 
               eml_new@dataset@creator[[1]]@id@.Data)
})
