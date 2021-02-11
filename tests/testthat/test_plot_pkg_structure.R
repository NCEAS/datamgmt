context("Plot package structure")


cn <- dataone::CNode("PROD")
mn <- dataone::getMNode(cn,"urn:node:ARCTIC")


test_that("plot_pkg_structure() accepts correct inputs", {
    expect_error(plot_pkg_structure(7, "test"))
    expect_error(plot_pkg_structure("test", "test"))
    expect_error(plot_pkg_structure(list("test"), "test"))

    expect_error(plot_pkg_structure(mn, ""))
    expect_error(plot_pkg_structure(mn, 7))
    expect_error(plot_pkg_structure(mn, list("test")))
})
