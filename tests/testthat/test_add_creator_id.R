context("Add creator id")

#for add_creator_id
    #misspelled surname will give an error
    #surname is not case sensitive
    #orcid is added to correct location in EML
    #ref id is added to correct location in EML

eml_path_original <- file.path(system.file(package = "datamgmt"), "dummy_meta_full.xml")
eml1 <- EML::read_eml(eml_path_original)

test_that("misspelled surname will give an error", {
  expect_error(add_creator_id(eml1, surname = "HighStakes"))
})

test_that("surname is not case sensitive", {
  expect_equal(add_creator_id(eml1,
                              id = "hs_id",
                              surname = "High-Stakes"),
               add_creator_id(eml1,
                              id = "hs_id",
                              surname = "HIGH-STAKES"))
})

test_that("orcid is added to correct location in EML", {
  orcid_test <- "INSERT_ORCID_HERE"
  eml_new <- add_creator_id(eml1,
                            orcid = orcid_test)

  expect_equal(orcid_test,
               eml_new@dataset@creator[[1]]@userId[[1]]@.Data)
})

test_that("ref id is added to correct location in EML", {
  id_test <- "SOME_ID"
  eml_new <- add_creator_id(eml1,
                            id = id_test)
  expect_equal(id_test,
               eml_new@dataset@creator[[1]]@id@.Data)
})
