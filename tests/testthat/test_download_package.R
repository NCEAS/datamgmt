context("Download package")

cn <- dataone::CNode('PROD')
mn <- dataone::getMNode(cn,'urn:node:ARCTIC')

# package <- arcticdatautils::get_package(mn,
#                                         "resource_map_doi:10.18739/A23W02",
#                                         file_names = TRUE)

#faster alternative:
package <- list()
package$metadata <- c(N2_2014metadata.xml = "doi:10.18739/A23W02")
package$resource_map <- "resource_map_doi:10.18739/A23W02"
package$data <- c(`2013_2014_winter_N2_dissoxy.csv` = "urn:uuid:0e648447-b685-4a8d-b2d1-9143c799509f",
                  `2014_summer_N2_spconductance.csv` = "urn:uuid:76770fec-8b7e-43b9-8b50-821bd042af95",
                  `2014_summer_N2_temperature.csv` = "urn:uuid:bdd04c25-73f9-4d17-bc91-3c38bf05d561",
                  `2013_2014_winter_N2_spconductance.csv` = "urn:uuid:6c2d697e-bd0f-4449-9bf4-c76f191bc417",
                  `2013_2014_winter_N2_temperature.csv` = "urn:uuid:62451306-55f6-4392-a548-5e57edff2ccc")

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
