testthat::context("Get Custom Units")

if("units" %in% rownames(installed.packages())) {

testthat::test_that("get_custom_units accepts multiple unit input formats", {


    form_1 <- datamgmt::get_custom_units('kilometersPerSquareSecond') #preferred input form
    form_2 <- datamgmt::get_custom_units('Kilometers per seconds squared')
    form_3 <- datamgmt::get_custom_units('km/s^2')
    form_4 <- datamgmt::get_custom_units('km s-2')
    form_5 <- datamgmt::get_custom_units('s-2 /     kilometers-1')
    form_6 <- datamgmt::get_custom_units('km/kg km/(s^2*km)*kg^4/(kg kilograms kg seconds) sec(s)(s)(s s)/(s s^3)')

    forms_equal <- (form_1 == form_2 &&
                    form_1 == form_3 &&
                    form_1 == form_4 &&
                    form_1 == form_5 &&
                    form_1 == form_6)

    testthat::expect_true(forms_equal)

})

testthat::test_that("get_custom_units recognizes reciprocals", {

    out_1 <- datamgmt::get_custom_units('per meter')
    out_2 <- datamgmt::get_custom_units('/ meter')
    out_3 <- datamgmt::get_custom_units('/m')
    out_4 <- datamgmt::get_custom_units('m^-1')
    out_5 <- datamgmt::get_custom_units('1/m')

    forms_equal <- (out_1 == out_2 &&
                    out_1 == out_3 &&
                    out_1 == out_4 &&
                    out_1 == out_5)

    testthat::expect_true(forms_equal)
    testthat::expect_true(out_1$unitType == "lengthReciprocal")
})

}