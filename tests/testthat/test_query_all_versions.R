context("Query all versions of a PID")

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

    if (!arcticdatautils::is_token_set(mn_test)) {
        skip("No token set. Skipping test.")
    }

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

test_that("query_all_versions returns correct output", {
    # Note: This test could fail if pid: urn:uuid:ed600b2a-76a8-4974-91cc-0e86a68c47e2 is updated
    # The test doesn't check for the exact correct output, but relatively correct output

    cnTest <- dataone::CNode('STAGING')
    mn_test <- dataone::getMNode(cnTest,'urn:node:mnTestARCTIC')

    if (!arcticdatautils::is_token_set(mn_test)) {
        skip("No token set. Skipping test.")
    }

    results <- query_all_versions(mn_test,
                                  "urn:uuid:ed600b2a-76a8-4974-91cc-0e86a68c47e2",
                                  fields = "submitter")
    submitters <- c("http://orcid.org/0000-0002-5511-9717",
                    "http://orcid.org/0000-0002-2561-5840")

    expect_equivalent(unique(results$submitter), submitters)

})
