context("Excel to csv")

test_that("csv files are written correctly", {
    excel_path <- file.path(tempdir(), "dummy.xlsx")
    file.copy(file.path(system.file(package = "datamgmt"),
                        "example_excel.xlsx"),
              excel_path)

    datamgmt::excel_to_csv(excel_path, tempdir())
    file_paths <- file.path(tempdir(), c("dummy_Sheet1.csv", "dummy_Sheet2.csv"))

    expect_equal(all(file.exists(file_paths)), TRUE)
})

test_that("excel_to_csv doesn't stop if file is not excel format", {
    dummy_data <- data.frame(seq(1:5))

    path <- file.path(tempdir(), "dummy.Rdata")
    save(dummy_data, file = path)

    # Will output a message but will not error
    expect_message(datamgmt::excel_to_csv(path))
})