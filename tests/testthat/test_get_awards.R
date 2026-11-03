context("Get NSF awards")

mock_awards_api <- function(total_count = 1L) {
    test_env <- parent.frame()
    test_env$requested_offsets <- integer()

    testthat::local_mocked_bindings(
        getURL = function(url) {
            offset <- as.integer(sub(".*&offset=", "", url))
            test_env$requested_offsets <- c(test_env$requested_offsets, offset)
            paste0(
                '<?xml version="1.0"?><response>',
                '<award><id>', offset, '</id><title>Test award</title></award>',
                '<metadata><offset>', offset, '</offset><rpp>25</rpp>',
                '<totalCount>', total_count, '</totalCount></metadata>',
                '</response>'
            )
        },
        .package = "datamgmt",
        .env = test_env
    )
}

test_that("get_awards runs with only from_date specified", {
    mock_awards_api()
    award_info <- get_awards(from_date = "11/01/2017")
    expect_is(award_info, "data.frame")
})

test_that("get_awards runs with from_date and to_date specified", {
    mock_awards_api()
    award_info <- get_awards(from_date = "11/01/2017",
                             to_date = "01/01/2018")
    expect_is(award_info, "data.frame")
})

test_that("get_awards runs with query specified", {
    mock_awards_api()
    award_info <- get_awards(from_date = "11/01/2017",
                             query = "id=1748653")
    expect_is(award_info, "data.frame")
})

test_that("get_awards runs with only print_fields specified", {
    mock_awards_api()
    award_info <- get_awards(from_date = "11/01/2017",
                             print_fields = "id,title")
    expect_equal(length(award_info), 2)
})

test_that("get_awards stops pagination at the API total count", {
    mock_awards_api(total_count = 26L)
    award_info <- get_awards(print_fields = "id,title")

    expect_equal(requested_offsets, c(1L, 26L))
    expect_equal(award_info$id, c("1", "26"))
})

test_that("Error checks work", {
    expect_error(get_awards(from_date = "1-1-17"))
    expect_error(get_awards(from_date = 2017))
    expect_error(get_awards(query = "hi"))
    expect_error(get_awards(print_fields = TRUE))
})
