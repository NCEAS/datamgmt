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

    # Clone package
    pkg <- clone_package("resource_map_urn:uuid:2b4e4174-4e4b-4a46-8ab0-cc032eda8269",
                         mn_prod,
                         mn_test,
                         clone_child_packages = FALSE)

    # Package tests
    expect_named(pkg, c("metadata", "data", "resource_map"))
    expect_true(all(arcticdatautils::object_exists(mn_test, unlist(pkg))))
    expect_length(pkg, 3)

    # Check object lengths
    lengths <- sapply(pkg, length)
    expect_equivalent(lengths, c(1,0,1))
})

test_that("clone_package copies a package with data", {
    if (!arcticdatautils::is_token_set(mn_test)) {
        skip("No token set. Skipping test.")
    }

    # Clone package
    pkg <- clone_package("resource_map_doi:10.18739/A2RZ6X", mn_prod, mn_test)

    # Package tests
    expect_named(pkg, c("metadata", "data", "resource_map"))
    expect_true(all(arcticdatautils::object_exists(mn_test, unlist(pkg))))
    expect_length(pkg, 3)

    # Check object lengths
    lengths <- sapply(pkg, length)
    expect_equivalent(lengths, c(1,4,1))
})

test_that("clone_package copies a package with a child package", {
    if (!arcticdatautils::is_token_set(mn_test)) {
        skip("No token set. Skipping test.")
    }

    # Clone package
    pkg <- clone_package("resource_map_urn:uuid:2b4e4174-4e4b-4a46-8ab0-cc032eda8269",
                              mn_prod, mn_test, clone_child_packages = TRUE)

    expect_named(pkg, c("metadata", "data", "resource_map", "child_packages"))
    expect_true(all(arcticdatautils::object_exists(mn_test, unlist(pkg))))
    expect_length(pkg, 4)

    child <- arcticdatautils::get_package(mn_test, pkg$child_packages[1])

    # Check object lengths
    lengths <- sapply(child, length)
    expect_equivalent(lengths, c(1,1,2,0))
})
