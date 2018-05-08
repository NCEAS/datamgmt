context("Checks for get_awards function")

test_that("get_awards runs with only from_date specified", {
    library(testthat)
    library(XML)
    library(stringr)
    library(RCurl)

    award_info <- get_awards(from_date = "11/01/2017")
    expect_is(award_info, "data.frame")
})

test_that("get_awards runs with from_date and to_date specified", {
    library(testthat)
    library(XML)
    library(stringr)
    library(RCurl)

    award_info <- get_awards(from_date = "11/01/2017",
                             to_date = "01/01/2018")
    expect_is(award_info, "data.frame")
})

test_that("get_awards runs with query specified", {
    library(testthat)
    library(XML)
    library(stringr)
    library(RCurl)

    award_info <- get_awards(from_date = "11/01/2017",
                             query = "id=1748653")
    expect_is(award_info, "data.frame")
})

test_that("get_awards runs with only print_fields specified", {
    library(testthat)
    library(XML)
    library(stringr)
    library(RCurl)

    award_info <- get_awards(from_date = "11/01/2017",
                             print_fields = "id,title")
    expect_equal(length(award_info), 2)
})

test_that("Error checks work", {
    library(testthat)
    library(XML)
    library(stringr)
    library(RCurl)

    expect_error(get_awards(from_date = "1-1-17"))
    expect_error(get_awards(from_date = 2017))
    expect_error(get_awards(query = "hi"))
    expect_error(get_awards(print_fields = TRUE))
})
