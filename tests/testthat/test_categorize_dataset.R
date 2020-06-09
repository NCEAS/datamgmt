context("New Category")

ss_test <- "https://docs.google.com/spreadsheets/d/1GEj9THJdh22KCe1RywewbruUiiVkfZkFH8FO1ib9ysM/edit#gid=1479370118"

test_that("spelling is correct", {
    expect_error(categorize_dataset("doi:10.18739/A2QJ77Z09", c("biolog", "oceanogrophy"), "test spelling", T))

    expect_error(categorize_dataset("not a pid", c("biology", "oceanography"), "test pid", T))

    expect_error(categorize_dataset("not a pid", c("biology", "oceanography", "soil science", "ecology", "biology", "oceanography"), "test pid", T))
})
