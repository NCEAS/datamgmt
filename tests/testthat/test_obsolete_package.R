context("Obsolete a Package")

cn <- dataone::CNode('STAGING')
mn <- dataone::getMNode(cn,'urn:node:mnTestARCTIC')

test_that("obsolete_package errors gracefully", {
    expect_error(obsolete_package(5))
    expect_error(obsolete_package(mn, 5, "metadata_pid"))
    expect_error(obsolete_package(mn,
                                  "urn:uuid:464c6b80-4e9a-4733-a167-833991637b23",
                                  "metadata_pid"))
})

test_that("obsolete_package doesn't modify existing version chains", {
    if (!arcticdatautils::is_token_set(mn)) {
        skip("No token set. Skipping test.")
    }

    pkg_old <- arcticdatautils::create_dummy_package(mn)
    pkg_new <- arcticdatautils::publish_update(mn,
                                               pkg_old$metadata,
                                               pkg_old$resource_map,
                                               pkg_old$data)

    expect_error(obsolete_package(mn, pkg_old$metadata, pkg_new$metadata))
})

test_that("obsolete_package sets 'metadata_obsolete' to the end of a version chain", {
    if (!arcticdatautils::is_token_set(mn)) {
        skip("No token set. Skipping test.")
    }

    pkg <- arcticdatautils::create_dummy_package(mn)
    pkg2 <- arcticdatautils::publish_update(mn,
                                            pkg$metadata,
                                            pkg$resource_map,
                                            pkg$data)
    pkg3 <- arcticdatautils::create_dummy_package(mn)

    expect_warning(obsolete_package(mn, pkg$metadata, pkg3$metadata))
})

test_that("obsolete_package sets 'metadata_new' to the beginning of a version chain", {
    if (!arcticdatautils::is_token_set(mn)) {
        skip("No token set. Skipping test.")
    }

    pkg <- arcticdatautils::create_dummy_package(mn)
    pkg2 <- arcticdatautils::create_dummy_package(mn)
    pkg2 <- arcticdatautils::publish_update(mn,
                                            pkg2$metadata,
                                            pkg2$resource_map,
                                            pkg2$data)

    expect_warning(obsolete_package(mn, pkg$metadata, pkg2$metadata))
})

test_that("obsolete_package obsoletes a package", {
    if (!arcticdatautils::is_token_set(mn)) {
        skip("No token set. Skipping test.")
    }

    pkg_old <- arcticdatautils::create_dummy_package(mn)
    pkg_new <- arcticdatautils::create_dummy_package(mn)

    expect_true(obsolete_package(mn, pkg_old$metadata, pkg_new$metadata))
})
