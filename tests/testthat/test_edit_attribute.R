context("edit_attribute")

eml_path <- system.file("dummy_eml_w_attributes.xml", package = "datamgmt")
eml <- EML::read_eml(eml_path)
eml_key_path <- system.file("key_eml_w_attributes.xml", package = "datamgmt")
eml_key <- EML::read_eml(eml_key_path)

test_that("attributes are updated in correct slots", {

    eml_test <- edit_attribute(eml, 1, 2,
                               attributeDefinition = "test definition",
                               domain = "numericDomain",
                               measurementScale = "ratio",
                               unit = "dimensionless",
                               numberType = "whole",
                               definition = NA)

    eml_attr_test <- eml_test@dataset@dataTable[[1]]@attributeList@attribute[[2]]
    eml_attr_key <- eml_key@dataset@dataTable[[1]]@attributeList@attribute[[2]]

    expect_match(eml_attr_test@attributeDefinition,
                 "test definition")

    expect_equivalent(eml_attr_test@measurementScale,
                      eml_attr_key@measurementScale)

    expect_equivalent(eml_attr_test@measurementScale@ratio,
                      eml_attr_key@measurementScale@ratio)

    expect_equivalent(eml_attr_test@measurementScale@ratio@unit,
                      eml_attr_key@measurementScale@ratio@unit)

    expect_match(eml_attr_test@measurementScale@ratio@numericDomain@numberType,
                 "whole")
})

test_that("existing attributes that aren't changed in update are retained", {

    eml_test <- edit_attribute(eml, 1, 2,
                               attributeDefinition = "test definition",
                               domain = "numericDomain",
                               measurementScale = "ratio",
                               unit = "dimensionless",
                               numberType = "whole",
                               definition = NA)

    expect_match(eml_test@dataset@dataTable[[1]]@attributeList@attribute[[2]]@attributeName,
                 "col2")
})

test_that("slots that no longer apply to updated domain type are gone", {
    eml_test <- edit_attribute(eml, 1, 2,
                               attributeDefinition = "test definition",
                               domain = "numericDomain",
                               measurementScale = "ratio",
                               unit = "dimensionless",
                               numberType = "whole",
                               definition = NA)

    expect_error(eml_test@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale@nominal@nonNumericDomain@textDomain[[1]]@definition)
})

test_that("error occurs if incorrect combinations occur between measurementScale and domain", {

    expect_error(edit_attribute(eml, 1, 2,
                                attributeDefinition = "test definition",
                                domain = "textDomain",
                                measurementScale = "ratio",
                                unit = "dimensionless",
                                numberType = "whole",
                                definition = 'NA'))

    expect_error(edit_attribute(eml, 1, 2,
                                attributeDefinition = "test definition",
                                domain = "numericDomain",
                                measurementScale = "dateTime",
                                unit = "dimensionless",
                                numberType = "whole",
                                definition = 'NA'))
})

test_that("error occurs if you specify either missingValueCode or missing ValueCodeExplanation but not both", {

    expect_error(edit_attribute(eml, 1, 2,
                                missingValueCode = "testValueCode"))
    expect_error(edit_attribute(eml, 1, 2,
                                missingValueCode = "testExplanation"))
})
