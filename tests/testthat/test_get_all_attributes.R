context("unit tests for get_all_attributes function")

cn <- dataone::CNode('PROD')
mn <- dataone::getMNode(cn,'urn:node:ARCTIC')
eml <- EML::read_eml(rawToChar(dataone::getObject(mn, "doi:10.18739/A23W02")))

test_that("error checks function correctly", {
    expect_error(get_all_attributes())
    expect_error(get_all_attributes(eml = 5))
    expect_error(get_all_attributes(url = "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02",
                                    node = "dummy node"))
    expect_error(get_all_attributes(url = 7))
    eml@dataset@dataTable <- new("ListOfdataTable")
    expect_error(get_all_attributes(eml))

})

test_that("get_all_attributes returns the same table for 'eml' and 'url' arguments", {
    eml_attributes <- get_all_attributes(eml)
    url_attributes <- get_all_attributes(url = "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02")
    expect_equal(eml_attributes, url_attributes)
})
test_that("get_all_attributes can switch to a different node", {})
test_that("get_all_attributes")
test_that("get_all_attributes")