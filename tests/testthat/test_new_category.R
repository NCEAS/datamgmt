context("New Category")

ss_test <- "https://docs.google.com/spreadsheets/d/1GEj9THJdh22KCe1RywewbruUiiVkfZkFH8FO1ib9ysM/edit#gid=1479370118"

test_that("spelling is correct", {
    expect_error(new_category("doi:10.18739/A2QJ77Z09", c("biolog", "oceanogrophy"), "test spelling", T))

    expect_error(new_category("not a pid", c("biology", "oceanography"), "test pid", T))

    expect_error(new_category("not a pid", c("biology", "oceanography", "soil science", "ecology", "biology", "oceanography"), "test pid", T))
})

# test_that("writes to sheet properly", {
#
#     sheet <- googlesheets4::read_sheet(ss_test)
#     new <- new_category("doi:10.18739/A2QJ77Z09", c("biology", "oceanography"), "your name", T)
#
#     expect_length(new, (sheet + 1))
# })
