context("Clone package")

# Set test node
d1c_test <- dataone::D1Client("STAGING", "urn:node:mnTestARCTIC")

# This is a temporary function to wait for a newly created package to become visible on its member node. 
# It will repeatedly check for the existence of the package until it is found or a timeout is reached.
# TODO: Fix arcticdatautils::create_dummy_package() to wait for the package to become available before returning.
# Wait for a newly created package to become visible on its member node.
wait_for_package <- function(mn, resource_map_pid, timeout = 60, interval = 1) {
    deadline <- Sys.time() + timeout

    repeat {
        package <- tryCatch(
            arcticdatautils::get_package(mn, resource_map_pid),
            warning = function(w) NULL,
            error = function(e) NULL
        )

        if (!is.null(package)) {
            return(package)
        }

        if (Sys.time() >= deadline) {
            stop("Package resource map did not become available within ", timeout, " seconds: ",
                 resource_map_pid)
        }

        Sys.sleep(interval)
    }
}

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
    wait_for_package(d1c_test@mn, pkg$resource_map)
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
    wait_for_package(d1c_test@mn, pkg$resource_map)
    pkg2 <- copy_package(pkg$resource_map, d1c_test, d1c_test)

    expect_length(pkg2, 3)
})

test_that("clone_package copies a package w - w/o data and children", {
    if (!arcticdatautils::is_token_set(d1c_test@mn)) {
        skip("No token set. Skipping test.")
    }

    # Create a child
    child <- arcticdatautils::create_dummy_package(d1c_test@mn, size = 2)
    wait_for_package(d1c_test@mn, child$resource_map)

    # Create a parent
    parent1 <- arcticdatautils::create_dummy_parent_package(d1c_test@mn, child$resource_map)
    wait_for_package(d1c_test@mn, parent1$parent)

    # Create a parent
    parent2 <- arcticdatautils::create_dummy_parent_package(d1c_test@mn, parent1$parent)
    wait_for_package(d1c_test@mn, parent2$parent)

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

test_that("cloning an updated package reuses its existing data objects", {
    skip_if_no_tokens()

    # size includes metadata; size = 2 creates one data object.
    pkg <- arcticdatautils::create_dummy_package(d1c_test@mn, size = 2)
    wait_for_package(d1c_test@mn, pkg$resource_map)

    package_clone_args <- list(
        from = d1c_test,
        to = d1c_to,
        add_access_to = arcticdatautils:::get_token_subject(),
        change_auth_node = TRUE,
        public = TRUE,
        new_pid = FALSE
    )

    first_clone <- do.call(
        clone_package,
        c(list(resource_map_pid = pkg$resource_map), package_clone_args)
    )

    # Create a new metadata PID and resource map while keeping the data PIDs.
    updated_metadata_pid <- clone_object(
        pid = pkg$metadata,
        from = d1c_test,
        to = d1c_test,
        add_access_to = arcticdatautils:::get_token_subject(),
        change_auth_node = TRUE,
        public = TRUE,
        new_pid = TRUE
    )
    updated_resource_map_pid <- arcticdatautils::create_resource_map(
        mn = d1c_test@mn,
        metadata_pid = updated_metadata_pid,
        data_pids = pkg$data
    )
    wait_for_package(d1c_test@mn, updated_resource_map_pid)
    updated_package <- arcticdatautils::get_package(d1c_test@mn, updated_resource_map_pid)
    expect_equal(updated_package$metadata, updated_metadata_pid)
    expect_equal(updated_package$data, pkg$data)

    second_clone <- do.call(
        clone_package,
        c(list(resource_map_pid = updated_resource_map_pid), package_clone_args)
    )

    expect_length(pkg$data, 1)
    expect_equal(first_clone$data, pkg$data)
    expect_equal(second_clone$data, first_clone$data)
    expect_equal(second_clone$metadata, updated_metadata_pid)
    expect_equal(second_clone$resource_map, updated_resource_map_pid)
    expect_false(identical(second_clone$metadata, first_clone$metadata))
    expect_false(identical(second_clone$resource_map, first_clone$resource_map))
    expect_true(all(arcticdatautils::object_exists(d1c_to@mn, second_clone$data)))
})

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

test_that("is_blank_pid detects empty identifiers", {
    expect_true(is_blank_pid(NA_character_))
    expect_true(is_blank_pid(character(0)))
    expect_true(is_blank_pid(""))
    expect_true(is_blank_pid(c("a", "b")))
    expect_false(is_blank_pid("urn:uuid:123"))
})

test_that("cloning the newer version first still links the chain once the older one is cloned", {
    skip_if_no_tokens()

    chain <- create_chain()
    args <- clone_args()

    suppressWarnings(
        cloned2 <- do.call(clone_object, c(list(pid = chain$pid2, new_pid = FALSE), args))
    )
    expect_true(is_unset(dataone::getSystemMetadata(d1c_to@mn, cloned2)@obsoletes))

    cloned1 <- do.call(clone_object, c(list(pid = chain$pid1, new_pid = FALSE), args))

    expect_equal(dataone::getSystemMetadata(d1c_to@mn, cloned1)@obsoletedBy, chain$pid2)
    expect_equal(dataone::getSystemMetadata(d1c_to@mn, cloned2)@obsoletes, chain$pid1)
})

test_that("cloning in chain order does not warn", {
    skip_if_no_tokens()

    chain <- create_chain()
    args <- clone_args()

    expect_no_warning({
        do.call(clone_object, c(list(pid = chain$pid1, new_pid = FALSE), args))
        do.call(clone_object, c(list(pid = chain$pid2, new_pid = FALSE), args))
    })
})
