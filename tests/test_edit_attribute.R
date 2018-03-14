testthat::context("edit_attribute")

testthat::test_that("attributes are updated in correct slots", {
    eml <- EML::read_eml('/home/sfreund/datamgmt/inst/dummy_eml_w_attributes.xml')
    eml_key <- EML::read_eml('/home/sfreund/datamgmt/inst/key_eml_w_attributes.xml')

    eml_test <- edit_attribute(eml, 1, 2, attributeDefinition = "test definition", domain = "numericDomain",
                               measurementScale = "ratio", unit = "dimensionless", numberType = "whole", definition = NA)

    testthat::expect_match(eml_test@dataset@dataTable[[1]]@attributeList@attribute[[2]]@attributeDefinition, "test definition")
    testthat::expect_equivalent(eml_test@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale,
                           eml_key@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale)
    testthat::expect_equivalent(eml_test@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale@ratio,
                           eml_key@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale@ratio)
    testthat::expect_equivalent(eml_test@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale@ratio@unit,
                           eml_key@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale@ratio@unit)
    testthat::expect_match(eml_test@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale@ratio@numericDomain@numberType, "whole")
})

testthat::test_that("existing attributes that aren't changed in update are retained", {

    eml_test <- edit_attribute(eml, 1, 2, attributeDefinition = "test definition", domain = "numericDomain",
                               measurementScale = "ratio", unit = "dimensionless", numberType = "whole", definition = NA)

    testthat::expect_match(eml_test@dataset@dataTable[[1]]@attributeList@attribute[[2]]@attributeName, "col2")
})

testthat::test_that("slots that no longer apply to updated domain type are gone", {
    eml_test <- edit_attribute(eml, 1, 2, attributeDefinition = "test definition", domain = "numericDomain",
                               measurementScale = "ratio", unit = "dimensionless", numberType = "whole", definition = NA)

    testthat::expect_error(eml_test@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale@nominal@nonNumericDomain@textDomain[[1]]@definition)
})

testthat::test_that("error occurs if incorrect combinations occur between measurementScale and domain", {

    testthat::expect_error(edit_attribute(eml, 1, 2, attributeDefinition = "test definition", domain = "textDomain",
                                            measurementScale = "ratio", unit = "dimensionless", numberType = "whole", definition = 'NA'))
    testthat::expect_error(edit_attribute(eml, 1, 2, attributeDefinition = "test definition", domain = "numericDomain",
                                            measurementScale = "dateTime", unit = "dimensionless", numberType = "whole", definition = 'NA'))
})
