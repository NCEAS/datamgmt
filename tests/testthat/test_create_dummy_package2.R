context("Test create_dummy_package2 errors")
#best check is to view package online

cnTest <- dataone::CNode('STAGING')
mnTest <- dataone::getMNode(cnTest,'urn:node:mnTestARCTIC')

test_that("Errors work", {
    if (!is_token_set(mnTest)) {
        skip("No token set. Skipping test.")
    }

    expect_error(create_dummy_package2(mnTest, title = 11))
    expect_error(create_dummy_package2("mnTest"))
})
