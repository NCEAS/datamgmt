context("edit_attribute")

eml_path <- system.file("dummy_eml_w_attributes.xml", package = "datamgmt")
eml <- EML::read_eml(eml_path)

eml_key_path <- system.file("key_eml_w_attributes.xml", package = "datamgmt")
eml_key <- EML::read_eml(eml_key_path)


test_that("inputs are correct", {
    expect_error(edit_attribute(7))
    expect_error(edit_attribute("test"))
    expect_error(edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute))
    expect_error(edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[1]],
                                attributeName = 7))
    expect_error(edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[1]],
                                domain = ""))
    expect_error(edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[1]],
                                domain = "text"))
    expect_error(edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[1]],
                                measurementScale = "other"))
    expect_error(edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[1]],
                                numberType = "other"))
    expect_error(edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[1]],
                                missingValueCode = "NA"))
    expect_error(edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[1]],
                                missingValueCodeExplanation = "data unavailable"))
})


test_that("attributes are updated in correct slots", {
    new_attribute <- edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[2]],
                                    attributeDefinition = "test definition",
                                    domain = "numericDomain",
                                    measurementScale = "ratio",
                                    unit = "dimensionless",
                                    numberType = "whole",
                                    definition = NA)

    eml_attr_key <- eml_key@dataset@dataTable[[1]]@attributeList@attribute[[2]]

    expect_match(new_attribute@attributeDefinition,
                 "test definition")

    expect_equivalent(new_attribute@measurementScale,
                      eml_attr_key@measurementScale)

    expect_equivalent(new_attribute@measurementScale@ratio,
                      eml_attr_key@measurementScale@ratio)

    expect_equivalent(new_attribute@measurementScale@ratio@unit,
                      eml_attr_key@measurementScale@ratio@unit)

    expect_match(new_attribute@measurementScale@ratio@numericDomain@numberType,
                 "whole")
})


test_that("existing attributes that are not changed in update are retained", {
    new_attribute <- edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[2]],
                                    attributeDefinition = "test definition",
                                    domain = "numericDomain",
                                    measurementScale = "ratio",
                                    unit = "dimensionless",
                                    numberType = "whole",
                                    definition = NA)

    expect_match(new_attribute@attributeName,
                 "col2")
})


test_that("slots that no longer apply to updated domain type are gone", {
    new_attribute <- edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[2]],
                                    attributeDefinition = "test definition",
                                    domain = "numericDomain",
                                    measurementScale = "ratio",
                                    unit = "dimensionless",
                                    numberType = "whole",
                                    definition = NA)

    expect_error(new_attribute@measurementScale@nominal@nonNumericDomain@textDomain[[1]]@definition)
})


test_that("error occurs if incorrect combinations occur between measurementScale and domain", {
    expect_error(edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[2]],
                                attributeDefinition = "test definition",
                                domain = "textDomain",
                                measurementScale = "ratio",
                                unit = "dimensionless",
                                numberType = "whole",
                                definition = NA))

    expect_error(edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[2]],
                                attributeDefinition = "test definition",
                                domain = "numericDomain",
                                measurementScale = "dateTime",
                                unit = "dimensionless",
                                numberType = "whole",
                                definition = NA))
})
