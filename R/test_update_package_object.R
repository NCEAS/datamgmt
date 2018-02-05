library(testthat)
library(arcticdatautils)
library(dataone)

context("Update object & resource map")

#test that:
    #other objects are retained
    #metadata stays the same
    #specified data_pid changes

cnTest <- dataone::CNode('STAGING')
mnTest <- dataone::getMNode(cnTest,'urn:node:mnTestARCTIC')

#make dummy pkg/data
pkg <- arcticdatautils::create_dummy_package(mnTest,
                                             size = 4)

dummy_data <- data.frame(col1 = 1:26, col2 = letters)
new_data_path <- tempfile(pattern = "", fileext = ".csv")
write.csv(dummy_data, new_data_path, row.names = FALSE)

data_pid <- pkg$data[2]

pkg_new <- update_package_object(mnTest,
                                 pkg$resource_map,
                                 data_pid,
                                 new_data_path)

test_that("other objects are retained", {
    expect_equal(pkg$data[-2] %in% pkg_new$data,
                 TRUE)
})

test_that("metadata stays the same", {
    expect_equal(pkg$metadata,
                 pkg_new$metadata)
})

test_that("new data pid is a version of old data pid", {
    versions <- get_all_versions(mnTest, data_pid)
    latest_version <- versions[length(versions)]

    new_data_pid <- pkg_new$data[!pkg_new$data %in% pkg$data]

    expect_equal(latest_version,
                 new_data_pid)
})