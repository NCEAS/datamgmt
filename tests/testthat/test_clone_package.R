context("test_clone_package")

cn <- dataone::CNode('STAGING')
mn_test <- dataone::getMNode(cn,'urn:node:mnTestARCTIC')

test_that("clone_package errors gracefully", {
    expect_error(clone_package(5, mn_test, mn_test))
    expect_error(clone_package("resource_map_doi:10.18739/A2RZ6X",
                 "Dummy Node",
                 mn_test))
    expect_error(clone_package("resource_map_doi:10.18739/A2RZ6X",
                               mn_test))
    suppressWarnings(expect_error(clone_package("Dummy pid", mn_test, mn_test)))
})

test_that("clone_package copies a package with no data pids", {
    if (!is_token_set(mn_test)) {
        skip("No token set. Skipping test.")
    }

    # Make a test package
    package <- create_dummy_package(mn_test, size = 1)
    expect_named(package, c("metadata", "resource_map", "data"))

    # Clone package
    new_pids <- clone_package(package$resource_map, mn_test, mn_test)
    expect_named(new_pids, c("data", "metadata", "resource_map"))
})

test_that("clone_package copies a package", {
    if (!is_token_set(mnTest)) {
        skip("No token set. Skipping test.")
    }

    # Make a test package
    package <- create_dummy_package(mnTest)
    expect_named(package, c("metadata", "resource_map", "data"))

    # Clone package
    new_pids <- clone_package(package$resource_map, mnTest, mnTest)

    expect_named(new_pids, c("data", "metadata", "resource_map"))
    expect_true(all(arcticdatautils::object_exists(mnTest, unlist(new_pids))))
})