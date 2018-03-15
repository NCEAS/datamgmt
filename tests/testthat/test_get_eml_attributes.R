context("unit tests for get_eml_attributes function")

cn <- dataone::CNode('PROD')
mn <- dataone::getMNode(cn,'urn:node:ARCTIC')
eml <- EML::read_eml(rawToChar(dataone::getObject(mn, "doi:10.18739/A23W02")))

# loads the correct output .rdata object for get_eml_attributes unit tests
path <- file.path(system.file(package = "datamgmt"), "get_eml_attributes_output.Rdata")
load(path)

test_that("error checks function correctly", {
    expect_error(get_eml_attributes(metadata = 5))
    expect_error(get_eml_attributes("dummy node",
                                    "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02"))
    expect_error(get_eml_attributes("ADC",
                                    "https://arcticdata.io/catalog/#view/DUMMY_PID"))
    eml@dataset@dataTable <- new("ListOfdataTable")
    expect_error(get_eml_attributes(eml))
    expect_error(get_eml_attributes(mn,
                                    eml,
                                    write_to_csv = "dummy value"))
    expect_error(get_eml_attributes(mn,
                                    eml,
                                    write_to_csv = TRUE,
                                    download_directory = "dummy directory"))
})

test_that("get_eml_attributes 'eml' argument outputs the correct .RData object", {
    results <- get_eml_attributes(mn, eml)
    expect_equal(results, get_eml_attributes_output)
})

test_that("get_eml_attributes 'url' argument outputs the correct .RData object", {
    results <- get_eml_attributes(mn, "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02")
    expect_equal(results, get_eml_attributes_output)
})

test_that("get_eml_attributes returns the same table for 'eml' and 'url' arguments", {
    eml_attributes <- get_eml_attributes(mn, eml)
    url_attributes <- get_eml_attributes(mn, "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02")

    expect_equal(eml_attributes, url_attributes)
})

# test_that("get_eml_attributes can switch to a different node", {
#     cn <- dataone::CNode('PROD')
#     knb <- dataone::getMNode(cn,"urn:node:KNB")
#     attributes <- get_eml_attributes(knb, "https://knb.ecoinformatics.org/#view/doi:10.5063/F1639MWV")
#
#     expect_length(attributes, 2)
#
#     expect_equal(dim(attributes$DataTeam_BOF_Proposal_Coding0131.csv[[1]]), 19)
#     expect_equal(dim(attributes$DataTeam_BOF_Proposal_Coding0131.csv[[1]])[2], 17)
#
# })

test_that("get_eml_attributes can write output csv's", {
    results <- get_eml_attributes(mn,
                                  eml,
                                  write_to_csv = TRUE,
                                  download_directory = tempdir())
    file_path <- file.path(tempdir(), "2013_2014_winter_N2_dissoxy_csv_attributes.csv")

    expect_true(file.exists(file_path))
})
