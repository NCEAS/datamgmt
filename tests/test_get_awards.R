testthat::context("Checks for get_awards function")

testthat::test_that("get_awards runs with only from_date specified", {
    library(testthat)
    library(XML)
    library(stringr)
    library(RCurl)

    award_info <- get_awards(from_date = "11/01/2017")
    testthat::expect_is(award_info, "data.frame")
})

testthat::test_that("get_awards runs with query specified", {
    library(testthat)
    library(XML)
    library(stringr)
    library(RCurl)

    award_info <- get_awards(from_date = "11/01/2017",
                             query = "id=1748653")
    testthat::expect_is(award_info, "data.frame")
})

testthat::test_that("get_awards runs with only print_fields specified", {
    library(testthat)
    library(XML)
    library(stringr)
    library(RCurl)

    award_info <- get_awards(from_date = "11/01/2017",
                             print_fields = "id,title")
    testthat::expect_equal(length(award_info), 2)
})

testthat::test_that("Error checks work", {
    library(testthat)
    library(XML)
    library(stringr)
    library(RCurl)

    testthat::expect_error(get_awards(from_date = "1-1-17"))
    testthat::expect_error(get_awards(from_date = 2017))
    testthat::expect_error(get_awards(query = "hi"))
    testthat::expect_error(get_awards(print_fields = TRUE))
})
