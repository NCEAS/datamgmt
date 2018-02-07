context("Download package")

cn <- dataone::CNode('STAGING')
mn <- dataone::getMNode(cn,'urn:node:mnTestARCTIC')

test_that("Special characters are removed from string", {
    input_string <- "doi:10.18739/A2BW6H"
    output_string <- remove_special_characters(input_string)

    expect_equal((grepl("\\/|:|\\.", output_string)), FALSE)
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