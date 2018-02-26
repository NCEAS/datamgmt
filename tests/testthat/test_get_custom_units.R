testthat::context("Get Custom Units")

testthat::test_that("get_custom_units accepts multiple unit input formats", {

    form_1 <- datamgmt::get_custom_units('kilometersPerSquareSecond') #preferred input form
    form_2 <- get_custom_units('Kilometers per seconds squared')
    form_3 <- get_custom_units('km/s^2')
    form_4 <- get_custom_units('km s-2')
    form_5 <- get_custom_units('s-2 /     kilometers-1')

    forms_equal <- (form_1 == form_2 &&
                    form_1 == form_3 &&
                    form_1 == form_4 &&
                    form_1 == form_5)

    testthat::expect_true(forms_equal)
})

testthat::test_that("get_custom_units recognizes reciprocal meters", {

    out_1 <- datamgmt::get_custom_units('per meter') #preferred input form
    out_2 <- datamgmt::get_custom_units('radians per meter') #preferred input form

    testthat::expect_true(out_1$unitType == "lengthReciprocal")
    testthat::expect_true(out_2$unitType == "lengthReciprocal")
})
