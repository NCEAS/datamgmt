library(testthat)
library(dataone)
library(arcticdatautils)
library(EML)

context("Test create_dummy_package2 errors")
#best check is to view package online

cnTest <- dataone::CNode('STAGING')
mnTest <- dataone::getMNode(cnTest,'urn:node:mnTestARCTIC')

test_that("Errors work", {
    expect_error(create_dummy_package2(mnTest, title = 11))
    expect_error(create_dummy_package2("mnTest"))
})