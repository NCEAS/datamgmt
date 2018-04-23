context("test_guess_member_node")

test_that("guess_member_node returns correct output", {
    mn_guess <- guess_member_node("doi:10.18739/A2G287", "PROD")
    mn_guess_url <- guess_member_node("https://arcticdata.io/catalog/#view/doi:10.18739/A2G287")

    cn <- dataone::CNode("PROD")
    mn <- dataone::getMNode(cn, "urn:node:ARCTIC")

    expect_equivalent(mn, mn_guess)
    expect_equivalent(mn, mn_guess_url)
})

test_that("guess_member_nodes outputs message when pid not found", {
    expect_error(guess_member_node("DUMMY PID", "PROD"))
    expect_error(guess_member_node("doi:10.18739/A2G287", "STAGING"))
})

test_that("guess_member_node error checks catch incorrect args", {
    expect_error(guess_member_node(5))
    expect_error(guess_member_node("doi:10.18739/A2G287", 5))
    expect_error(guess_member_node("doi:10.18739/A2G287", "DUMMY NODE"))
    expect_error(guess_member_node(5, "PROD"))
})
