testthat::context("Checks for add_attributes function")

data <- data.frame("depth" = c(1.2, 2.3), "temperature" = c(10.7, 9.5))
attributes <- data.frame("attributeName" = c("depth", "temperature"),
                          "attributeDefinition" = c("water depth in meters", "temperature in celsius"),
                          "unit" = c("meter", "celsius"))

testthat::test_that("add_attributes doesn't convert data to a different class",{
    data <- add_attributes(data, attributes)
    testthat::expect_true(is.data.frame(data))
})

testthat::test_that("add_attributes fails when there is insufficient attribute metadata", {
    attributes <- attributes[,1]
    testthat::expect_error(add_attributes(data, attributes))
})

testthat::test_that("attributes can be called with $ operator", {
    data <- add_attributes(data, attributes)

    testthat::expect_length(attributes(data)$depth, 3)
    testthat::expect_type(attributes(data)$temperature, "list")
    testthat::expect_null(attributes(data)$invalid_name)
})

# test all 3 error checks
# test a message is output when a package has no data