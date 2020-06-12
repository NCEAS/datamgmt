context("Categorize dataset")

test_sheet <- "https://docs.google.com/spreadsheets/d/1GEj9THJdh22KCe1RywewbruUiiVkfZkFH8FO1ib9ysM/edit#gid=1479370118"

test_that("spelling is correct", {
    expect_error(categorize_dataset("doi:10.18739/A2QJ77Z09", c("biolog", "oceanogrophy"), "test spelling", T))

    expect_error(categorize_dataset("not a pid", c("biology", "oceanography"), "test pid", T))

    expect_error(categorize_dataset("not a pid", c("biology", "oceanography", "soil science", "ecology", "biology", "oceanography"), "test pid", T))
})

test_that("doi or versions are already in sheet"){
    #same doi already in the sheet
    expect_warning(categorize_dataset("doi:10.18739/A2QJ77Z09", c("biology", "oceanography"), "test pid", T),
                   "identifier not added, already categorized")
    #same doi but different themes
    expect_warning(categorize_dataset("doi:10.18739/A2QJ77Z09", c("biology", "soil science"), "test pid", T),
                   "identifier not added, overwrite themes?")
    #previous version in sheet, categories the same
    expect_warning(categorize_dataset("doi:10.18739/A2QJ77Z09", c("biology", "soil science"), "test pid", T),
                   "identifier updated")

    expect_warning(categorize_dataset("doi:10.18739/A2QJ77Z09", c("biology", "soil science"), "test pid", T),
                   "doi not added, overwrite themes?")

}
