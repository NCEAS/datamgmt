context("Obsolete a Package")

cn <- dataone::CNode('STAGING')
mn <- dataone::getMNode(cn,'urn:node:mnTestARCTIC')

test_that("obsolete_package errors gracefully", {
    expect_error(obsolete_package(5))
    expect_error(obsolete_package(mn, 5, "metadata_pid"))
    expect_error(obsolete_package(mn,
                                  "urn:uuid:464c6b80-4e9a-4733-a167-833991637b23",
                                  "metadata_pid"))
    expect_error(obsolete_package(mn,
                                  "urn:uuid:bc417911-e43f-40bd-ba62-d91e14a0937f",
                                  "urn:uuid:31cb2518-bea4-4d16-a80c-a27b14ab1c39"))

})

test_that("obsolete_package obsoletes a package", {
    if (!arcticdatautils::is_token_set(mn)) {
        skip("No token set. Skipping test.")
    }

    pkg_old <- arcticdatautils::create_dummy_package(mn)
    pkg_new <- arcticdatautils::create_dummy_package(mn)

    expect_true(obsolete_package(mn, pkg_old$metadata, pkg_new$metadata))

})