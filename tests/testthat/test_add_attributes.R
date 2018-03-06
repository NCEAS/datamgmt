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

# test that get format type returns resource, metadata, data, and null
# test that get_resource map messages and returns null

# test that add_attributes
# - update old tests
# - new one for column level metadata

# test that transfer_attributes
# - initializes blank lists correctly
# - all argument checks work