context("unit tests for get_all_attributes function")

cn <- dataone::CNode('PROD')
mn <- dataone::getMNode(cn,'urn:node:ARCTIC')
eml <- EML::read_eml(rawToChar(dataone::getObject(mn, "doi:10.18739/A23W02")))

# loads the correct output .rdata object for get_all_attributes unit tests
path <- file.path(system.file(package = "datamgmt"), "get_all_attributes_output.rdata")
load(path)

test_that("error checks function correctly", {
    expect_error(get_all_attributes())
    expect_error(get_all_attributes(eml = 5))
    expect_error(get_all_attributes(url = "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02",
                                    node = "dummy node"))
    expect_error(get_all_attributes(url = "https://arcticdata.io/catalog/#view/DUMMY_PID"))
    expect_error(get_all_attributes(url = 7))
    eml@dataset@dataTable <- new("ListOfdataTable")
    expect_error(get_all_attributes(eml))
})

test_that("get_all_attributes 'eml' argument outputs the correct .RData object", {
    results <- get_all_attributes(eml)
    expect_equal(results, get_all_attributes_output)
})

test_that("get_all_attributes 'link' argument outputs the correct .RData object", {
    results <- get_all_attributes(url = "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02")
    expect_equal(results, get_all_attributes_output)
})

test_that("get_all_attributes returns the same table for 'eml' and 'url' arguments", {
    eml_attributes <- get_all_attributes(eml)
    url_attributes <- get_all_attributes(url = "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02")

    expect_equal(eml_attributes, url_attributes)
})

test_that("get_all_attributes can switch to a different node", {
    cn <- dataone::CNode('PROD')
    knb <- dataone::getMNode(cn,"urn:node:KNB")
    attributes <- get_all_attributes(url = "https://knb.ecoinformatics.org/#view/doi:10.5063/F1639MWV",
                                     node = knb)

    expect_length(attributes, 1)
    expect_length(attributes$DataTeam_BOF_Proposal_Coding0131.csv, 2)

    expect_equal(dim(attributes$DataTeam_BOF_Proposal_Coding0131.csv[[1]])[1], 19)
    expect_equal(dim(attributes$DataTeam_BOF_Proposal_Coding0131.csv[[1]])[2], 17)

})