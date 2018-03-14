context("test_guess_member_node")

test_that("guess_member_node returns correct output", {
    mn <- guess_member_node("PROD", "doi:10.18739/A2G287")

    cn <- dataone::CNode("PROD")
    mn_test <- dataone::getMNode(cn, "urn:node:ARCTIC")

    expect_equivalent(mn, mn_test)
})

test_that("guess_member_nodes outputs message when pid not found", {
    expect_error(guess_member_node("PROD", "DUMMY PID"))
    expect_error(guess_member_node("STAGING", "doi:10.18739/A2G287"))
})

test_that("guess_member_node error checks catch incorrect args", {
    expect_error(guess_member_node(5))
    expect_error(guess_member_node(5, "doi:10.18739/A2G287"))
    expect_error(guess_member_node("DUMMY NODE", "doi:10.18739/A2G287"))
    expect_error(guess_member_node("PROD", 5))
})
