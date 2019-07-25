context("QA package")

cn_prod <- CNode("PROD")
adc_prod <- getMNode(cn_prod, "urn:node:ARCTIC")

eml_test <- EML::read_eml(system.file("dummy_eml_w_attributes.xml", package = "datamgmt"))


test_that("qa_package() accepts correct inputs", {
    expect_error(qa_package(7))
    expect_error(qa_package(adc_prod, ""))
    expect_error(qa_package(adc_prod, "test"))
    expect_error(qa_package(adc_prod, "test", read_all_data = 7))
    expect_error(qa_package(adc_prod, "test", check_attributes = 7))
    expect_error(qa_package(adc_prod, "test", check_creators = 7))
    expect_error(qa_package(adc_prod, "test", check_access = 7))
})


test_that("qa_attributes() accepts correct inputs", {
    expect_error(qa_attributes(7))
    expect_error(qa_attributes(eml_test@dataset@dataTable[[1]], 7))
    data <- data.frame(1, 2, 3, 4)
    expect_error(qa_attributes(eml_test@dataset@dataTable[[1]], data, 7))
})

test_that('qa_creator_orcids accepts one creator', {
    me <- list(individualName = list(givenName = "Dom", surName = "Mullen"))
    orcid <- "http://orcid.org/1234-1234-1234-1234"
    creator <- eml$creator(me,
        electronicMailAddress = "fakeaddress@email.com",
        userId = list(directory = "http://orcid.org",
                      userId = orcid))
    my_eml <- list(dataset = list(
        title = "A Minimal Valid EML Dataset",
        creator = creator,
        contact = me)
    )
    expect_equal(qa_creator_ORCIDs(my_eml), orcid)
})

test_that('qa_creator_orcids accepts multiple creators', {
    me <- list(individualName = list(givenName = "Dom", surName = "Mullen"))
    orcid <- "http://orcid.org/1234-1234-1234-1234"
    creator <- eml$creator(me,
                           electronicMailAddress = "fakeaddress@email.com",
                           userId = list(directory = 'http://orcid.org',
                                         userId = "http://orcid.org/1234-1234-1234-1234"))
    my_eml <- list(dataset = list(
        title = "A Minimal Valid EML Dataset",
        creator = list(creator, creator),
        contact = me)
    )
    expect_equal(qa_creator_ORCIDs(my_eml), rep(orcid, 2))
})

test_that('qa_creator_orcids catches invalid orcids', {
    me <- list(individualName = list(givenName = "Dom", surName = "Mullen"))
    creator <- eml$creator(me,
                           electronicMailAddress = "fakeaddress@email.com",
                           userId = list(directory = 'http://orcid.org',
                                         userId = "arcticdata.io"))
    my_eml <- list(dataset = list(
        title = "A Minimal Valid EML Dataset",
        creator = creator,
        contact = me)
    )
    fn <- function() qa_creator_ORCIDs(my_eml)
    res <- evaluate_promise(fn())
    expect_true(grepl('Each creator needs to have a proper ORCID.', res$output))
})

test_that('qa_creator_orcids catches creators without orcids', {
    me <- list(individualName = list(givenName = "Dom", surName = "Mullen"))
    my_eml <- list(dataset = list(
        title = "A Minimal Valid EML Dataset",
        creator = me,
        contact = me)
    )
    fn <- function() qa_creator_ORCIDs(my_eml)
    res <- evaluate_promise(fn())
    expect_true(grepl('Each creator needs to have a proper ORCID.', res$output))
})

