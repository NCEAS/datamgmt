context("New Category")

Set test sheet
ss_test <- "https://docs.google.com/spreadsheets/d/1GEj9THJdh22KCe1RywewbruUiiVkfZkFH8FO1ib9ysM/edit#gid=1479370118"

test_that("spelling is correct", {
    expect_error(clone_package(5, d1c_test, d1c_test))

    expect_error(clone_package("Dummy pid",
                               "Dummy Node",
                               d1c_test))

    expect_error(clone_package("Dummy pid",
                               d1c_test))

    suppressWarnings(expect_error(clone_package("Dummy pid", d1c_test, d1c_test)))
})

test_that("we can clone a package", {
    if (!arcticdatautils::is_token_set(d1c_test@mn)) {
        skip("No token set. Skipping test.")
    }

    pkg <- arcticdatautils::create_dummy_package(d1c_test@mn)
    pkg2 <- clone_package(pkg$resource_map, d1c_test, d1c_test,
                          add_access_to = arcticdatautils:::get_token_subject(),
                          change_auth_node = TRUE,
                          public = TRUE,
                          new_pid = TRUE)

    expect_length(pkg2, 3)
})

test_that("we can copy a package", {
    if (!arcticdatautils::is_token_set(d1c_test@mn)) {
        skip("No token set. Skipping test.")
    }

    pkg <- arcticdatautils::create_dummy_package(d1c_test@mn)
    pkg2 <- copy_package(pkg$resource_map, d1c_test, d1c_test)

    expect_length(pkg2, 3)
})

test_that("clone_package copies a package w - w/o data and children", {
    if (!arcticdatautils::is_token_set(d1c_test@mn)) {
        skip("No token set. Skipping test.")
    }

    # Create a child
    child <- arcticdatautils::create_dummy_package(d1c_test@mn, size = 2)

    # Create a parent
    parent1 <- arcticdatautils::create_dummy_parent_package(d1c_test@mn, child$resource_map)

    # Create a parent
    parent2 <- arcticdatautils::create_dummy_parent_package(d1c_test@mn, parent1$parent)

    # Clone package
    clone <- clone_package(resource_map_pid = parent2$parent,
                           from = d1c_test,
                           to = d1c_test,
                           add_access_to = arcticdatautils:::get_token_subject(),
                           public = TRUE,
                           clone_children = TRUE,
                           new_pid = TRUE,
                           change_auth_node = FALSE)

    # Parent2 Test
    expect_named(clone, c("metadata", "data", "child_packages", "resource_map"))
    expect_true(all(arcticdatautils::object_exists(d1c_test@mn, unlist(clone))))
    expect_length(clone$data, 0) # 0 Data
    expect_length(clone$metadata, 1) # 1 Metadata file
    expect_length(clone$child_packages, 1) # 1 Child
    expect_length(clone$resource_map, 1) # 1 Resource Map

    # Parent1 Test
    clone_parent1 <- arcticdatautils::get_package(d1c_test@mn, clone$child_packages)
    expect_true(all(arcticdatautils::object_exists(d1c_test@mn, unlist(clone_parent1))))
    expect_length(clone_parent1$data, 0) # No Data
    expect_length(clone_parent1$metadata, 1) # 1 Metadata file
    expect_length(clone_parent1$child_packages, 1) # 1 Child
    expect_length(clone_parent1$resource_map, 1) # 1 Resource Map

    # Child Test
    clone_child <- arcticdatautils::get_package(d1c_test@mn, clone_parent1$child_packages)
    expect_true(all(arcticdatautils::object_exists(d1c_test@mn, unlist(clone_child))))
    expect_length(clone_child$data, 1) # 1 Data
    expect_length(clone_child$metadata, 1) # 1 Metadata file
    expect_length(clone_child$child_packages, 0) # 0 Child
    expect_length(clone_child$resource_map, 1) # 1 Resource Map

    # Child Data Test
    dataobj1 <- dataone::getObject(d1c_test@mn, child$data)
    cloneobj1 <- dataone::getObject(d1c_test@mn, clone_child$data)
    expect_equal(dataobj1, cloneobj1) #cloned data is the same
})
