context("QA system metadata")

cn_prod <- CNode("PROD")
adc_prod <- getMNode(cn_prod, "urn:node:ARCTIC")


test_that("qa_sysmeta() accepts correct inputs", {
    expect_error(qa_sysmeta(7))
    expect_error(qa_sysmeta(adc_prod, ""))
    expect_error(qa_sysmeta(adc_prod, "test"))
    expect_error(qa_sysmeta(adc_prod, "test", all_results = 7))
})
