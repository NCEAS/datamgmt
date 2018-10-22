context("Search EML for index")

test_that("Returns correct locations", {

    attributes <-
        data.frame(
            attributeName = c(
                "length_1",
                "time_2",
                "length_3"),
            attributeDefinition = c(
                "def 1",
                "def 2",
                "def 3"),
            formatString = c(
                NA,
                NA,
                NA),
            measurementScale = c(
                "ratio",
                "ratio",
                "ratio"),
            domain = c(
                "numericDomain",
                "numericDomain",
                "numericDomain"),
            definition = c(
                NA,
                NA,
                NA),
            unit = c(
                "meter",
                "second",
                "meter"),
            numberType = c(
                "real",
                "real",
                "real"),
            stringsAsFactors = FALSE
        )

    attributeList <- EML::set_attributes(attributes)

    dataTable_1 <- new("dataTable",
                       entityName = "2016_data.csv",
                       entityDescription = "2016 data",
                       attributeList = attributeList)

    dataTable_2 <- dataTable_1

    dataTable_3 <- new("dataTable",
                       entityName = "2015_data.csv",
                       entityDescription = "2016 data",
                       attributeList = attributeList)

    creator_1 <- new("creator",
                     individualName = new("individualName",
                                          surName = "LAST",
                                          givenName = "FIRST"))
    creator_2 <- new("creator",
                     individualName = new("individualName",
                                          surName = "LAST",
                                          givenName = "FIRST_2"))
    creator_3 <- creator_2

    title <- "Title"

    dataset <- new("dataset",
                   title = title,
                   creator = c(creator_1, creator_2, creator_3),
                   dataTable = c(dataTable_1, dataTable_2, dataTable_3))

    eml <- new("eml",
               dataset = dataset)

    expect_equal(c(2,3), which_in_eml(eml@dataset@creator, "givenName", "FIRST_2"))
    expect_error(which_in_eml(eml@dataset@dataTable, "attributeName", "length_3"))
    expect_equal(c(1,3), which_in_eml(eml@dataset@dataTable[[1]]@attributeList@attribute, "attributeName", function(x) {grepl("^length", x)}))
})
