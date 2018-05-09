context("Return EML Units")

test_that("return_eml_units accepts multiple unit input formats", {


    form_1 <- return_eml_units('kilometersPerSquareSecond') #preferred input form
    form_2 <- return_eml_units('Kilometers per seconds squared')
    form_3 <- return_eml_units('km/s^2')
    form_4 <- return_eml_units('km s-2')
    form_5 <- return_eml_units('s-2 /     kilometers-1')
    form_6 <- return_eml_units('km/kg km/(s^2*km)*kg^4/(kg kilograms kg seconds) sec(s)(s)(s s)/(s s^3)')

    forms_equal <- (form_1 == form_2 &&
                    form_1 == form_3 &&
                    form_1 == form_4 &&
                    form_1 == form_5 &&
                    form_1 == form_6)

    expect_true(forms_equal)

})

test_that("return_eml_units recognizes reciprocals", {

    out_1 <- return_eml_units('per meter')
    out_2 <- return_eml_units('/ meter')
    out_3 <- return_eml_units('/m')
    out_4 <- return_eml_units('m^-1')
    out_5 <- return_eml_units('1/m')

    forms_equal <- (out_1 == out_2 &&
                    out_1 == out_3 &&
                    out_1 == out_4 &&
                    out_1 == out_5)

    expect_true(forms_equal)
    expect_true(out_1$unitType == "lengthReciprocal")
})
