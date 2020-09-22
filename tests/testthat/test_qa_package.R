context("QA package")

testthat::skip_on_travis()

cn_prod <- CNode("PROD")
adc_prod <- getMNode(cn_prod, "urn:node:ARCTIC")

eml_test <- EML::read_eml(system.file("dummy_eml_w_attributes.xml", package = "datamgmt"))


test_that("qa_package() accepts correct inputs", {
    expect_error(qa_package(7))
    expect_error(qa_package(adc_prod, ""))
    expect_error(qa_package(adc_prod, "test"))
    expect_error(qa_package(adc_prod, "test", read_all_data = 7))
    expect_error(qa_package(adc_prod, "test", check_attributes = 7))
    expect_error(qa_package(adc_prod, "test", check_creators = 7))
    expect_error(qa_package(adc_prod, "test", check_access = 7))
})


test_that("qa_attributes() accepts correct inputs", {
    expect_error(qa_attributes(7))
    expect_error(qa_attributes(eml_test@dataset@dataTable[[1]], 7))
    data <- data.frame(1, 2, 3, 4)
    expect_error(qa_attributes(eml_test@dataset@dataTable[[1]], data, 7))
})
