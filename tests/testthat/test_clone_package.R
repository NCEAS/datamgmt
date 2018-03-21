context("test_clone_package")

# Set test node
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

    expect_error(clone_package("resource_map_doi:10.18739/A2RZ6X",
                               mn_test,
                               mn_test,
                               "Dummy Value"))
})

test_that("clone_package copies a package with no data pids", {
    if (!arcticdatautils::is_token_set(mn_test)) {
        skip("No token set. Skipping test.")
    }

    # Make a test package
    package <- arcticdatautils::create_dummy_package(mn_test, size = 1)
    expect_named(package, c("metadata", "resource_map", "data"))

    # Clone package
    new_pids <- clone_package(package$resource_map, mn_test, mn_test)

    # Package tests
    expect_named(new_pids, c("data", "metadata", "resource_map"))
    expect_true(all(arcticdatautils::object_exists(mn_test, unlist(new_pids))))
    expect_length(new_pids, 3)

    # Check object lengths
    lengths <- sapply(new_pids, length)
    expect_equivalent(lengths, c(0,1,1))
})

test_that("clone_package copies a package", {
    if (!arcticdatautils::is_token_set(mn_test)) {
        skip("No token set. Skipping test.")
    }

    # Make a test package
    package <- arcticdatautils::create_dummy_package(mn_test)
    expect_named(package, c("metadata", "resource_map", "data"))

    # Clone package
    new_pids <- clone_package(package$resource_map, mn_test, mn_test)

    # Package tests
    expect_named(new_pids, c("data", "metadata", "resource_map"))
    expect_true(all(arcticdatautils::object_exists(mn_test, unlist(new_pids))))
    expect_length(new_pids, 3)

    # Check object lengths
    lengths <- sapply(new_pids, length)
    expect_equivalent(lengths, c(1,1,1))
})

test_that("clone_package copies a package with a child package", {
    if (!arcticdatautils::is_token_set(mn_test)) {
        skip("No token set. Skipping test.")
    }

    # Make a test parent package
    parent <- arcticdatautils::create_dummy_package(mn_test)
    expect_named(parent, c("metadata", "resource_map", "data"))

    # Make a test child package
    child <- arcticdatautils::create_dummy_package(mn_test)
    expect_named(child, c("metadata", "resource_map", "data"))

    # Nest packages
    rm <- arcticdatautils::update_resource_map(mn_test,
                                               parent$resource_map,
                                               parent$metadata,
                                               parent$data,
                                               child$resource_map,
                                               check_first = FALSE)

    # Clone package
    new_pids <- clone_package(rm, mn_test, mn_test, clone_child_packages = TRUE)

    expect_named(new_pids, c("data", "metadata", "resource_map", "child_packages"))
    expect_true(all(arcticdatautils::object_exists(mn_test, unlist(new_pids))))
    expect_length(new_pids, 4)

    child <- arcticdatautils::get_package(mn_test, new_pids$child_packages)

    # Check object lengths
    lengths <- sapply(child, length)
    expect_equivalent(lengths, c(1,1,1, 0))
})