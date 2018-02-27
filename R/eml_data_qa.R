#' Test congruence of attributes and data for a package
#'
#' This script assumes correctness in the resource map and data
#' Purpose: QA script to check that attributes match values in the data
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
#'#' @importFrom datapack hasAccessRule
#' @param node (MNode) Member Node where the PID is associated with a package.
#' @param pid (character) The PID of a resource map to be QA'ed.
#' @param rowsToRead (character/numeric) Default 10. If set to "All", will pull all data from remote and check that column types match attributes, otherwise only pull first rows listed equal to rowsToRead (e.g. rowsToRead = 10 will pull first 10 rows). Only applicable to public packages (private packages will read complete dataset). To skip checking data, set rowsToread = 0.
#'
#' @return
#' @export
#'
#' @author Emily O'Dean \email{eodean10@@gmail.com}
#'
#' @examples
#' \dontrun{
#' # For a package, run QA checks
#' qa_package(mn, pid, rowsToRead = 10)
#' }
qa_package <- function(node, pid, rowsToRead = "All") {
    stopifnot(class(node) %in% c("MNode", "CNode"))
    stopifnot(is.character(pid), nchar(pid) > 0)
    stopifnot(rowsToRead == "All" | is.numeric(rowsToRead))

    supported_file_formats <- c("text/csv",
                                "text/tsv",
                                "text/plain",
                                "application/vnd.ms-excel",
                                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")

    package <- tryCatch({
        suppressWarnings(arcticdatautils::get_package(node, pid))
    },
    error = function(e) {
        stop("\nFailed to get package. Is your DataONE token set?")
    })

    eml <- EML::read_eml(rawToChar(dataone::getObject(node, package$metadata)))

    cat(crayon::green(paste0("\n\n\n..................Processing package ", package$resource_map, "..................\n")))

    # Check creators
    creators <- eml@dataset@creator
    creator_ORCIDs <- unlist(eml_get(creators, "userId"))
    isORCID <-  grepl("http[s]?:\\/\\/orcid.org\\/[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}", creator_ORCIDs)
    creator_ORCIDs <- sub("^https://", "http://", creator_ORCIDs)

    if (length(isORCID) != length(creators) || !all(isORCID)) {
        cat(crayon::red("\nEach creator needs to have a proper ORCID."))
    } else {
        cat(crayon::green("\nAll creators have a proper ORCID."))
    }

    urls <- unique(unlist(EML::eml_get(eml, "url"), recursive = T))

    if(length(urls) != length(package$data)) {
        stop("Not all data files have a physical section in the eml. Please fix and try again.")
    }

    urls_in_data <- grepl(paste0(package$data, collapse="$|"), urls)

    if(!all(urls_in_data)) {
       cat(crayon::red("\nThe following URLs set in the eml do not match the URLs set in the physicals. Fix this error and try again."))
       cat(crayon::red("\n", urls[!urls_in_data]))
       return(0)
    } else {
        cat(crayon::green("\nAll objects in this package have associated physicals with URLs."))
    }

    for (objectpid in package$data)
    {
        # Does the given pid have an associated datatable
        sysmeta <- dataone::getSystemMetadata(node, objectpid)

        i <- which(grepl(paste0(objectpid, collapse="$|"), urls))

        if (length(i) == 0) next

        if (i <= length(eml@dataset@dataTable)) {
            dataTable <- eml@dataset@dataTable[[i]]
            objectName <- dataTable@physical[[1]]@objectName
            is_otherEntity <- FALSE
        } else {
            dataTable <- NULL
            objectName <- eml@dataset@otherEntity[[i - length(eml@dataset@dataTable)]]@physical[[1]]@objectName
            is_otherEntity <- TRUE
        }

        cat(crayon::green(paste0("\n\n..................Processing object ", objectpid, ", ", objectName, "..................")))


        # Check rights holder
        if (!(sysmeta@rightsHolder %in% creator_ORCIDs)) {
            cat(crayon::yellow("\nThe rightsHolder is not set to one of the creators"))
        } else {
            cat(crayon::green("\nThe rightsHolder is set to one of the creators"))
        }

        # Check creator access
         for (creator in creator_ORCIDs) {
             creator_read <- datapack::hasAccessRule(sysmeta, creator, "read")
             creator_write <- datapack::hasAccessRule(sysmeta, creator, "write")
             creator_changePermissions <- datapack::hasAccessRule(sysmeta, creator, "changePermission")
             access <- c(creator_read, creator_write, creator_changePermissions)

             if (!all(access)) {
                 cat(crayon::yellow("\nFull access is not set for creator ORCID", creator))
             } else {
                 cat(crayon::green("\nFull access is set for creator ORCID", creator))
             }
         }

        # If object is not tabular data, continue
        format <- sysmeta@formatId
        if (!format %in% supported_file_formats) next

        # If object is tabular data, and set as an other Entity, warn.
        if (is_otherEntity) {
            cat(crayon::red(paste0("\nThis object has a format '", format, "', but is set as an otherEntity.\n ",
                           "If the data is tabular, please set as a dataTable")))
            next
        }

        # If package is public, we can read directly from the csv, otherwise we use data one have to get all the data
        isPublic <- datapack::hasAccessRule(sysmeta, "public", "read")

        if (rowsToRead == "All") {
            rowsToRead <- -1}

        if (rowsToRead == 0) next

        data <- tryCatch({
            if (isPublic == TRUE) {
                if (format == "text/csv") {
                    utils::read.csv(urls[i], nrows = rowsToRead, check.names=FALSE)
                } else if (format == "text/tsv") {
                    utils::read.delim(urls[i], nrows = rowsToRead)
                } else if (format == "text/plain") {
                    utils::read.table(urls[i], nrows = rowsToRead)
                } else if (format == "application/vnd.ms-excel") {
                    readxl::read_xls(urls[i], n_max = rowsToRead)
                } else if (format == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
                    readxl::read_xlsx(urls[i], n_max = rowsToRead)
                }
            } else {
                utils::read.csv(textConnection(rawToChar(dataone::getObject(node, objectpid))), nrows = rowsToRead, stringsAsFactors = FALSE)
            }
        },
        error = function(e) {
            cat(crayon::red(paste0("\nFailed to read file ", urls[i])))
        })

        qa_attributes(node, dataTable, data, readData)

        cat(crayon::green(paste0("\n..................Processing complete for object ", objectpid, ", ", objectName, "..................")))
    }
}


#' Test congruence of attributes and data for a given dataset and dataTable
#'
#' This function is called by get_package() but can be used on its own to test congruence
#' between a dataTable and a data object (data.frame). See qa_package() help documentation for more details.
#'
#' @param node (MNode) Member Node where the PID is associated with an object.
#' @param dataTable (dataTable) EML \code{dataTable} associated with the data object.
#' @param data (data.frame) Data frame of data object.
#' @param checkEnumeratedDomains (logical) Default TRUE. If True, will match unique values in data to defined EML enumerated domains.
#'
#' @return
#' @export
#'
#' @author Emily O'Dean \email{eodean10@@gmail.com}
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
        cat(crayon::red(paste0("\nEmpty attribute table for ", dataTable@physical[[1]]@distribution[[1]]@online@url)))
        return(0)
    }

    dataCols <- colnames(data)

    # Check for attribute correctness according to the EML schema using arcticdatautils::eml_validate_attributes
    attOutput <- utils::capture.output(arcticdatautils::eml_validate_attributes(dataTable@attributeList))
    attErrors <- which(grepl('FALSE', utils::head(attOutput, -2)))

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
            cat(crayon::red(paste0("\nThe EML dataTable includes attributes: ", toString(nonmatcheml, sep = ", "), " which are not present in the data.")))
            cat(crayon::yellow("\nContinuing attribute and data matching WITHOUT mismatched attributes - fix issues and re-run the function after first round completion."))
        }

        # Data has values that EML doesn't have
        if (length(nonmatchdata) > 0) {
            cat(crayon::red(paste0("\nThe data includes attributes: ", toString(nonmatchdata, sep = ", "), " which are not present in the EML.")))
            cat(crayon::yellow("\nContinuing attribute and data matching WITHOUT mismatched attributes - fix issues and re-run the function after first round completion."))
        }

        # Values match but aren't ordered correctly
        if (length(nonmatcheml) == 0 & length(nonmatchdata) == 0 & allequal == FALSE) {
            cat(crayon::yellow(paste0("\nAttributes in the attribute table match column names but are incorrectly ordered.")))
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
                cat(crayon::yellow(paste0(c("\nWarning: Mismatch in attribute type for the following attribute: ", matchingAtt$attributeName, ". Type of data is ", attClass, " which should probably have interval or ratio measurementScale in EML, not ", matchingAtt$measurementScale), collapse = "")))
            }
        } else if (attClass == "character" | attClass == "logical") {
            if (matchingAtt$measurementScale != "nominal" & matchingAtt$measurementScale != "ordinal") {
                cat(crayon::yellow(paste0(c("\nWarning: Mismatch in attribute type for the following attribute: ", matchingAtt$attributeName, ".
                                  Type of data is ", attClass, " which should probably have nominal or ordinal measurementScale in EML,
                                  not ", matchingAtt$measurementScale), collapse = "")))
            }
        }
    }

    if (checkEnumeratedDomains == TRUE) {
        # If enumerated domains exist, check that values in the data match the enumerated domains
        if (length(attributeTable$factors) > 0) {
            for (i in seq_along(unique(attributeTable$factors$attributeName))) {
                emlAttName <- unique(attributeTable$factors$attributeName)[i]
                emlUniqueValues <- subset(attributeTable$factors, attributeTable$factors$attributeName == emlAttName)$code

                dataUniqueValues <- unique(data[,which(colnames(data) == emlAttName)])

                intersection <- intersect(dataUniqueValues, emlUniqueValues)
                nonmatcheml <- emlUniqueValues[!emlUniqueValues %in% intersection]
                nonmatchdata <- dataUniqueValues[!dataUniqueValues %in% intersection]

                if (length(nonmatcheml) > 0) {
                    cat(crayon::red(paste0("\nThe EML contains the following enumerated domain values for the attribute ", as.character(emlAttName), " that do not appear in the data: ", toString(nonmatcheml, sep = ", "))))
                }

                if (length(nonmatchdata) > 0) {
                    cat(crayon::red(paste0("\nThe data contains the following enumerated domain values for the attribute ", as.character(emlAttName), " that do not appear in the EML: ", toString(nonmatchdata, sep = ", "))))
                }
            }
        }
        # If there are any NA's or missing values in the data, check that there is an associated missing value code in the eml}
        for (i in which(colSums(is.na(data)) > 0)) {
            attribute <- attributeTable$attributes[i , ]
            if (is.na(attribute$missingValueCode)) {
                cat(crayon::red(paste0("\nThe column ", attribute$attributeName, " contains missing values (NA) but does not have a missing value code.")))
            }
        }
    }
}

