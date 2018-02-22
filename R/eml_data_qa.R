library(arcticdatautils)
library(dataone)
library(EML)
library(crayon)

#' Test congruence of attributes and data for a package
#'
#' This script assumes correctness in the resource map and data
#' Purpose: QA script to check that attributes match values in the data
#'    The first iteration of this script will scrape all remote packages on various servers (get pids via solr query) KNB, ArcticData, etc
#'    Output will be written to a file and should include resource map pid, pids of problem datasets, and the exact issues
#'    Second iteration will be built to take in local data and eml (before it is pushed to KNB)
#'
#'    Functions:
#'     Names: Check that all column names in attributes match the column names in the csv
#'     Possible conditions to account for:
#'     - dataTable does not exist for a csv
#'     - Physical has not been set and so URL id in dataTable is incorrect
#'     - Some of the attributes that exist in the data don't exist in the attribute table
#'     - Some of the attributes that exist in the attribute table don't exist in the data
#'     - There is a typo in one of the attributes or column names so they don't match (maybe covered by above)
#'     Domains: Check that all attribute types match attribute types in the csv
#'     Possible conditions to account for:
#'     - nominal, ordinal, integer, ratio, dateTime
#'     - If domain is enumerated domain, not all enumerated values in the data are accounted for in the enumarated definition
#'     - If domain is enumerated domain, not all enumerated values in the enumerated definition are actually represented in the data
#'     - Type of data does not match type
#'     Values: Check for accidental characters in the csv (one char in a column of ints)
#'
#'     Note: this function also calls qa_attributes and passes the data object and associated dataTable, but this function can also be called directly.
#'
#' @param node (MNode) Member Node where the PID is associated with an object.
#' @param pid (character) The PID of a resource map to do QA on a package.
#' @param readData (logical) Default TRUE. If True, pull all data from remote and check that column types match attributes, otherwise only pull first 10 rows. Only applicable to public packages (private packages will read complete dataset).
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # For a package, run QA checks
#' qa_package(mn, pid, readData = FALSE)
#' }
qa_package <- function(node, pid, readData = TRUE) {
    stopifnot(class(node) %in% c("MNode", "CNode"))
    stopifnot(is.character(pid), nchar(pid) > 0)

    # This function checks that all data in the package is consistent between the EML and systemMetadata, and
    # checks that all data, metadata, and resource maps have proper rights and access set.
    # datamgmt::check_package(node, pid)

    supported_file_formats <- c("text/csv",
                                "text/tsv",
                                "text/plain",
                                "application/vnd.ms-excel",
                                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")

    versions <- suppressWarnings(get_all_versions(node, pid))
    package <- suppressWarnings(get_package(node, versions[length(versions)]))
    eml <- read_eml(rawToChar(getObject(node, package$metadata)))

    cat(green(paste0("\n\n\n..................Processing package ", package$resource_map, "..................")))

    urls <- character(0)
    for (i in seq_along(eml@dataset@dataTable)) {
        dataTable <- eml@dataset@dataTable[[i]]

        if (length(dataTable@physical[[1]]@distribution) == 0) {
            cat(red(paste0("Missing URL/distribution info for dataTable with objectName: ", dataTable@physical[[1]]@objectName)))
        } else {
            urls <- c(urls, dataTable@physical[[1]]@distribution[[1]]@online@url)
        }
    }

    if (length(urls) == 0) {
        cat(red("\nNone of the objects in this package have associated physicals with URLs. Fix this error and try again."))
        return(0)
    }

    for (objectpid in package$data)
    {
        # Does the given pid have an associated datatable
        sysmeta <- getSystemMetadata(node, objectpid)

        # If object is not tabular data, continue
        format <- sysmeta@formatId
        if (!format %in% supported_file_formats) next

        # If object is tabular data, should also have a dataTable, which is matched by the url
        i <- which(grepl(objectpid, urls))

        if (length(i) == 0) {
            cat(red(paste0("\nThe are no matching dataTables for object ", objectpid, " with format ", format,
                           ". Check for a mismatch in the physical url and the pid in the resource map.")))
            next
        }

        dataTable <- eml@dataset@dataTable[[i]]
        cat(green(paste0("\nProcessing object ", objectpid, ", ", dataTable@physical[[1]]@objectName, "..................")))

        # If package is public, we can read directly from the csv, otherwise we use data one have to get all the data
        isPublic <- "public" %in% sysmeta@accessPolicy$subject

        if (readData == TRUE) {
            rowsToRead <- -1
        } else {
            rowsToRead <- 10
        }

        if (isPublic == TRUE) {
            data <- tryCatch({
                if (format == "text/csv") {
                    read.csv(urls[i], nrows=rowsToRead, check.names=FALSE)
                } else if (format == "text/tsv") {
                    read.delim(urls[i], nrows=rowsToRead)
                } else if (format == "text/plain") {
                    fread(urls[i], nrows=rowsToRead)
                } else if (format == "application/vnd.ms-excel") {
                    readxl:::read_xls(urls[i], nrows=rowsToRead)
                } else if (format == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
                    readxl:::read_xlsx(urls[i], nrows=rowsToRead)
                }
            },
            error = function(e) {
                cat(red(paste0("\nFailed to read file ", urls[i])))
            })
        } else {
            # instead, read data from DataOne
            data <- read.csv(textConnection(rawToChar(getObject(node, objectpid))), stringsAsFactors=FALSE)
        }

        qa_attributes(node, dataTable, data, readData)
        # print(paste0("Check for ", objectpid, " is complete."))
    }
}

#' @param node (MNode) Member Node where the PID is associated with an object.
#' @param dataTable (dataTable) EML dataTable associated with the data object.
#' @param data (data.frame) Data frame of data object.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # For a package, run QA checks on a data.frame and its associated EML dataTable.
#'
#' qa_attributes(mn, dataTable, dataObject)
#' }
qa_attributes <- function(node, dataTable, data, checkEnumeratedDomains = TRUE) {
    attributeTable <- EML::get_attributes(dataTable@attributeList)
    attributeNames <- attributeTable$attributes$attributeName

    if (is.null(attributeNames)) {
        cat(red(paste0("\nEmpty attribute table for ", dataTable@physical[[1]]@distribution[[1]]@online@url)))
        return(0)
    }

    dataCols <- colnames(data)

    # Check for attribute correctness according to the EML schema using arcticdatautils::eml_validate_attributes
    attOutput <- capture.output(eml_validate_attributes(dataTable@attributeList))
    attErrors <- which(grepl('FALSE', head(attOutput, -2)))

    if (length(attErrors) > 0) {
        print(attOutput[attErrors])
    }

    # Check that attribute names match column names - if not, we can't move forward with any other congruence tests
    allequal <- isTRUE(all.equal(dataCols, attributeNames))

    if (allequal == FALSE) {
        intersection <- intersect(attributeNames, dataCols)
        nonmatcheml <- attributeNames[!attributeNames %in% intersection]
        nonmatchdata <- dataCols[!dataCols %in% intersection]

        # Eml has values that data doesn't have
        if (length(nonmatcheml) > 0) {
            cat(red(paste0(c("\nThe EML dataTable includes that attributes ", nonmatcheml, " which are not present in the data."), collapse=" ")))
            cat(yellow("\nContinuing attribute and data matching WITHOUT mismatched attributes - fix issues and re-run the function after first round completion."))
        }

        # Data has values that EML doesn't have
        if (length(nonmatchdata) > 0) {
            cat(red(paste0(c("\nThe data includes that attributes ", nonmatchdata, " which are not present in the EML."), collapse=" ")))
            cat(yellow("\nContinuing attribute and data matching WITHOUT mismatched attributes - fix issues and re-run the function after first round completion."))
        }

        # Values match but aren't ordered correctly
        if (length(nonmatcheml) == 0 & length(nonmatchdata) == 0 & allequal == FALSE) {
            cat(yellow(paste0("\nAttributes in the attribute table match column names but are incorrectly ordered.")))
        }

        data <- data[, which(colnames(data) %in% intersection)]
        attributeTable$attributes <- attributeTable$attributes[which(attributeTable$attributes$attributeName %in% intersection),]
        attributeTable$attributes <- attributeTable$attributes[order(match(attributeTable$attributes$attributeName, colnames(data))),]
    }

    # Check if type of column matches the type of the data based on acceptable DataOne formats
    for (i in seq_along(data)) {
        matchingAtt <- attributeTable$attributes[i,]
        attClass <- class(data[,i])

        # TODO: If matching Att has a datetime domain, try coercing the column in R based on the date time format
        # if (matchingAtt$measurementScale == "dateTime") {
        #
        # }

        if (attClass == "numeric" | attClass == "integer" | attClass == "double") {
            if (matchingAtt$measurementScale != "ratio" & matchingAtt$measurementScale != "interval" & matchingAtt$measurementScale != "dateTime") {
                cat(yellow(paste0(c("\nWarning: Mismatch in attribute type for the following attribute: ", matchingAtt$attributeName, ". Type of data is ", attClass, " which must either have interval or ratio measurementScale in EML, not ", matchingAtt$measurementScale), collapse = " ")))
            }
        } else if (attClass == "character" | attClass == "logical") {
            if (matchingAtt$measurementScale != "nominal" & matchingAtt$measurementScale != "ordinal") {
                cat(yellow(paste0(c("\nWarning: Mismatch in attribute type for the following attribute: ", matchingAtt$attributeName, ".
                                  Type of data is ", attClass, " which must either have nominal or ordinal measurementScale in EML,
                                  not ", matchingAtt$measurementScale), collapse = " ")))
            }
        }
    }

    if (readData == TRUE) {
        # If enumerated domains exist, check that values in the data match the enumerated domains
        if (length(attributeTable$factors) > 0) {
            for (i in seq_along(unique(attributeTable$factors$attributeName))) {
                emlAttName <- unique(attributeTable$factors$attributeName)[i]
                emlUniqueValues <- subset(attributeTable$factors, attributeName == emlAttName)$code

                dataUniqueValues <- unique(data[,which(colnames(data) == emlAttName)])

                intersection <- intersect(dataUniqueValues, emlUniqueValues)
                nonmatcheml <- emlUniqueValues[!emlUniqueValues %in% intersection]
                nonmatchdata <- dataUniqueValues[!dataUniqueValues %in% intersection]

                if (length(nonmatcheml) > 0) {
                    cat(red(paste0(c("\nThe EML contains the following enumerated domain values for the attribute ", as.character(emlAttName), " that do not appear in the data:", as.character(nonmatcheml)), collapse=" ")))
                }

                if (length(nonmatchdata) > 0) {
                    cat(red(paste0(c("\nThe data contains the following enumerated domain values for the attribute ", as.character(emlAttName), " that do not appear in the EML:", as.character(nonmatchdata)), collapse=" ")))
                }
            }
        }
        # If there are any NA's or missing values in the data, check that there is an associated missing value code in the eml}
        for (i in which(colSums(is.na(data)) > 0)) {
            attribute <- attributeTable$attributes[i , ]
            if (is.na(attribute$missingValueCode)) {
                cat(red(paste0("\nThe column ", attribute$attributeName, " contains missing values (NA) but does not have a missing value code.")))
            }
        }
    }
}

