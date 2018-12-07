context("QA package")

cn_prod <- CNode("PROD")
adc_prod <- getMNode(cn_prod, "urn:node:ARCTIC")


test_that("qa_package() accepts correct inputs", {
    expect_error(qa_package(7))
    expect_error(qa_package(adc_prod, ""))
    expect_error(qa_package(adc_prod, "test"))
    expect_error(qa_package(adc_prod, "test", read_all_data = 7))
    expect_error(qa_package(adc_prod, "test", check_attributes = 7))
    expect_error(qa_package(adc_prod, "test", check_creators = 7))
    expect_error(qa_package(adc_prod, "test", check_access = 7))
})
