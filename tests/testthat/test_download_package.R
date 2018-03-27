context("Download package")

cn <- dataone::CNode('PROD')
mn <- dataone::getMNode(cn,'urn:node:ARCTIC')
package <- arcticdatautils::get_package(mn,
                                        "resource_map_doi:10.18739/A23W02",
                                        file_names = TRUE)

test_that("download_data_objects works", {
    if (!arcticdatautils::is_token_set(mn)) {
        skip("No token set. Skipping test.")
    }

    out_path <- file.path(tempdir(), names(package$data)[1])
    download_data_objects(mn, package$data[1], out_path)

    expect_true(file.exists(out_path))
    data <- read.csv(out_path)
    expect_equal(round(data$DOY[1], digits = 4), 172.4953)
})

test_that("All package contents download to a directory", {
    if (!arcticdatautils::is_token_set(mn)) {
        skip("No token set. Skipping test.")
    }

    # Create dummy package
    package <- arcticdatautils::create_dummy_package(mn)

    # Download dummy package
    directory <- tempdir()
    download_packages(mn,
                      package$resource_map,
                      directory,
                      check_download_size = FALSE)

    expect_equal(file.exists(file.path(directory, "dummy_object")), TRUE)
})

test_that("Contents of child packages download correctly", {
    if (!arcticdatautils::is_token_set(mn)) {
        skip("No token set. Skipping test.")
    }

    # Create dummy packages
    parent <- arcticdatautils::create_dummy_metadata(mn)
    package1 <- arcticdatautils::create_dummy_package(mn)
    package2 <- arcticdatautils::create_dummy_package(mn)
    resource_map_pid <- arcticdatautils::create_resource_map(mn,
                                                             metadata_pid = parent,
                                                             child_pids = c(package1$resource_map,
                                                                            package2$resource_map))
    # Update fileName
    sys_meta <- dataone::getSystemMetadata(mn, package2$data)
    sys_meta@fileName <- "dummy_object2"
    dataone::updateSystemMetadata(mn, package2$data, sys_meta)

    # Download dummy package
    directory <- tempdir()
    package <- arcticdatautils::get_package(mn, resource_map_pid)
    download_packages(mn,
                      package$resource_map,
                      directory,
                      check_download_size = FALSE)

    expect_equal(file.exists(file.path(directory, "dummy_object")), TRUE)
    expect_equal(file.exists(file.path(directory, "dummy_object2")), TRUE)

})

test_that("remove_special_characters works correctly", {
    input <- "doi:10.18739/A23W02"
    output <- remove_special_characters(input)
    expect_equal(output, "doi1018739A23W02")
})
