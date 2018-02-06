library(testthat)
library(arcticdatautils)
library(dataone)

context("Update object & resource map")

test_that("specified data object is changed; rest of package is intact", {
    cnTest <- dataone::CNode('STAGING')
    mnTest <- dataone::getMNode(cnTest,'urn:node:mnTestARCTIC')

    #make dummy pkg/data
    pkg <- arcticdatautils::create_dummy_package(mnTest,
                                                 size = 4)

    dummy_data <- data.frame(col1 = 1:26, col2 = letters)
    new_data_path <- tempfile(fileext = ".csv")
    write.csv(dummy_data, new_data_path, row.names = FALSE)

    data_pid <- pkg$data[2]

    pkg_new <- update_package_object(mnTest,
                                     data_pid,
                                     new_data_path,
                                     pkg$resource_map)

    #test: other objects are retained
    expect_equal(all(pkg$data[-2] %in% pkg_new$data), TRUE)


    #test: metadata stays the same
    expect_equal(pkg$metadata, pkg_new$metadata)

    #test: new data pid is a version of old data pid
    versions <- get_all_versions(mnTest, data_pid)
    latest_version <- versions[length(versions)]

    new_data_pid <- pkg_new$data[!pkg_new$data %in% pkg$data]

    expect_equal(latest_version, new_data_pid)
})
