context("Clone package")

# Set test node
d1c_test <- dataone::D1Client("STAGING", "urn:node:mnTestARCTIC")

test_that("clone_package errors gracefully", {
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

# Version chain tests: clone between two different nodes so PIDs can be kept
d1c_to <- dataone::D1Client("STAGING", "urn:node:mnTestKNB")

is_unset <- function(x) length(x) == 0 || is.na(x)

# Creates a two-version chain (pid1 -> pid2) on the source node
create_chain <- function() {
    pid1 <- arcticdatautils::create_dummy_object(d1c_test@mn)
    path <- tempfile(fileext = ".csv")
    write.csv(data.frame(a = 1:3), path, row.names = FALSE)
    pid2 <- arcticdatautils::update_object(d1c_test@mn, pid1, path, format_id = "text/csv")
    list(pid1 = pid1, pid2 = pid2)
}

skip_if_no_tokens <- function() {
    if (!arcticdatautils::is_token_set(d1c_test@mn) ||
        !arcticdatautils::is_token_set(d1c_to@mn)) {
        skip("No token set. Skipping test.")
    }
}

clone_args <- function() {
    list(from = d1c_test,
         to = d1c_to,
         add_access_to = arcticdatautils:::get_token_subject(),
         change_auth_node = TRUE,
         public = TRUE)
}

test_that("new_pid = FALSE keeps obsoletes/obsoletedBy and updates the previous version", {
    skip_if_no_tokens()

    chain <- create_chain()
    args <- clone_args()

    cloned1 <- do.call(clone_object, c(list(pid = chain$pid1, new_pid = FALSE), args))
    cloned2 <- do.call(clone_object, c(list(pid = chain$pid2, new_pid = FALSE), args))

    expect_equal(cloned1, chain$pid1)
    expect_equal(cloned2, chain$pid2)

    sysmeta2 <- dataone::getSystemMetadata(d1c_to@mn, cloned2)
    expect_equal(sysmeta2@obsoletes, chain$pid1)

    # The previous version on the destination node now points at the newer one
    sysmeta1 <- dataone::getSystemMetadata(d1c_to@mn, cloned1)
    expect_equal(sysmeta1@obsoletedBy, chain$pid2)
})

test_that("new_pid = TRUE resets obsoletes/obsoletedBy", {
    skip_if_no_tokens()

    chain <- create_chain()
    args <- clone_args()

    cloned1 <- do.call(clone_object, c(list(pid = chain$pid1, new_pid = TRUE), args))
    cloned2 <- do.call(clone_object, c(list(pid = chain$pid2, new_pid = TRUE), args))

    expect_false(cloned1 == chain$pid1)
    expect_false(cloned2 == chain$pid2)

    expect_true(is_unset(dataone::getSystemMetadata(d1c_to@mn, cloned1)@obsoletedBy))
    expect_true(is_unset(dataone::getSystemMetadata(d1c_to@mn, cloned2)@obsoletes))
})

test_that("a missing previous version only warns and the clone still succeeds", {
    skip_if_no_tokens()

    chain <- create_chain()
    args <- clone_args()

    # Previous version was never cloned to the destination node
    expect_warning(
        cloned2 <- do.call(clone_object, c(list(pid = chain$pid2, new_pid = FALSE), args))
    )
    expect_equal(cloned2, chain$pid2)
})
