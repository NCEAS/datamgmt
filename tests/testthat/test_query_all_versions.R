context("test_query_all_versions")
cn <- dataone::CNode("PROD")
mn <- dataone::getMNode(cn, "urn:node:ARCTIC")

test_that("query_all_versions stops with invalid input", {

    expect_error(query_all_versions(5))
    expect_error(query_all_versions("m", "doi:10.18739/A23R0PS65"))
    expect_error(query_all_versions(mn, "doi:10.18739/A23R0P"))
    expect_error(query_all_versions(mn, "doi:10.18739/A23R0PS65", c("origin", "banana")))
    expect_error(query_all_versions(mn, "doi:10.18739/A23R0PS65", 5))
    expect_error(query_all_versions(mn, 5, c("origin", "id")))
})

test_that("query_solr_metadata returns correct output", {

    df_fun <- query_solr_metadata(mn, "doi:10.18739/A23R0PS65", c("origin", "id"))
    df_query <- dataone::query(mn, list(q="documents:\"doi:10.18739/A23R0PS65\"",
                                          fl= "origin, id",
                                          rows="5"),
                               as = "data.frame")

    expect_equivalent(df_fun, df_query)

    df_fun_all <- query_solr_metadata(mn, "doi:10.18739/A23R0PS65")
    df_query_all <- dataone::query(mn, list(q="documents:\"doi:10.18739/A23R0PS65\"",
                                        fl= "*",
                                        rows="5"),
                               as = "data.frame")

    expect_equivalent(df_fun_all, df_query_all)
})
