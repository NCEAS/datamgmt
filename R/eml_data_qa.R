#' Test package including congruence of attributes and data
#'
#' This script assumes correctness in the resource map and data
#' Purpose: QA script to check a DataONE EML package. Will check that attributes match values in the data.
#'
#'     Note: this function also calls \code{\link{qa_attributes}} and passes the data object and associated dataTable, but that function can also be called directly.
#'
#' @importFrom datapack hasAccessRule
#' @import EML
#' @import crayon
#' @import arcticdatautils
#' @param node (MNode) Member Node where the PID is associated with a package.
#' @param pid (character) The PID of a resource map to be QA'ed.
#' @param readAllData (logical) Default TRUE. If True, pull all data from remote and check that column types match attributes, otherwise only pull first 10 rows. Only applicable to public packages (private packages will read complete dataset). If \code{check_attributes = FALSE}, no rows will be read.
#' @param check_attributes (logical). Default TRUE. Calls \code{\link{qa_attributes}}.
#' @param check_creators (logigal) Default FALSE. If True, will test if each creator has been assigned an ORCID. Will also run if \code{check_access = TRUE}.
#' @param check_access (logigal) Default FALSE. If True, will test if each creator has full access to the metadata, resource_map, and data objects. Will not run if the tests associated with \code{check_creators} fails.
#' @return
#' @export
#'
#' @author Emily O'Dean \email{eodean10@@gmail.com}
#'
#' @examples
#' \dontrun{
#' # For a package, run QA checks
#' qa_package(mn, pid, readAllData = FALSE, check_attributes = TRUE,
#'            check_creators = FALSE, check_access = FALSE)
#' }
qa_package <- function(node, pid, readAllData = TRUE,
                       check_attributes = TRUE,
                       check_creators = FALSE,
                       check_access = FALSE) {
    stopifnot(class(node) %in% c("MNode", "CNode"))
    stopifnot(is.character(pid), nchar(pid) > 0)

    supported_file_formats <- c("text/csv",
                                "text/tsv",
                                "text/plain",
                                "application/vnd.ms-excel",
                                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                "netCDF-4",
                                "netCDF-3",
                                "CF-1.4", "CF-1.3", "CF-1.2", "CF-1.1", "CF-1.0")

    package <- tryCatch({
        suppressWarnings(arcticdatautils::get_package(node, pid, file_names = TRUE))
    },
    error = function(e) {
        stop("\nFailed to get package. Is your DataONE token set?")
    })

    eml <- EML::read_eml(rawToChar(dataone::getObject(node, package$metadata)))

    cat(crayon::green(paste0("\n\n\n..................Processing package ", package$resource_map, "..................")))

    # Check creators
    if (check_creators | check_access) {
        creator_ORCIDs <- qa_creators(eml)
    }

    # Check access
    if (check_access && length(creator_ORCIDs) > 0) {
        # Check metadata
        sysmeta <- dataone::getSystemMetadata(node, package$metadata)
        qa_access(sysmeta, creator_ORCIDs)

        # Check resource_map
        sysmeta <- dataone::getSystemMetadata(node, package$resource_map)
        qa_access(sysmeta, creator_ORCIDs)
    }

    urls_dataTable <- unique(unlist(EML::eml_get(eml@dataset@dataTable, "url"), recursive = T))
    urls_otherEntity <- unique(unlist(EML::eml_get(eml@dataset@otherEntity, "url"), recursive = T))
    urls <- unique(append(urls_dataTable, urls_otherEntity))

    # Checks that each data object has a matching url in the eml.
    wrong_URL <- FALSE
    for (datapid in package$data) {
        n <- which(grepl(paste0(datapid, "$"), urls))

        if (length(n) != 1) {
            cat(crayon::red(paste0("\nThe URL/distribution for ", datapid, " is missing/incongruent in the physical section of the eml.")))
            wrong_URL <- TRUE
        }
    }

    if (length(urls) != length(package$data) | wrong_URL) {
        # Must stop here to ensure proper ordering in following loops.
        stop("\nAll distribution URLs in the eml must match the package's data URLs to continue.",
             "\nPlease fix and try again.")
    }

    for (objectpid in package$data)
    {
        n_dT <- which(grepl(paste0(objectpid, "$"), urls_dataTable))
        n_oE <- which(grepl(paste0(objectpid, "$"), urls_otherEntity))

        if (length(n_dT) == 1) {
            dataTable <- eml@dataset@dataTable[[n_dT]]
            urls <- urls_dataTable
            i <- n_dT
        } else if (length(n_oE) == 1) {
            dataTable <- eml@dataset@otherEntity[[n_oE]]
            urls <- urls_otherEntity
            i <- n_oE
        } else {
            next
        }

        sysmeta <- dataone::getSystemMetadata(node, objectpid)

        if (check_access && length(creator_ORCIDs) > 0) {
            qa_access(sysmeta, creator_ORCIDs)
        }

        if (!check_attributes) next

        # If object is not tabular data, continue
        format <- sysmeta@formatId
        if (!format %in% supported_file_formats) next

        cat(crayon::green(paste0("\n\n..................Processing object ", objectpid, " (", dataTable@physical[[1]]@objectName, ").................")))

        if (is.null(EML::get_attributes(dataTable@attributeList)$attributes)) {
            cat(crayon::red(paste0("\nEmpty attribute table for ", dataTable@physical[[1]]@distribution[[1]]@online@url)))
            next
        }

        # If package is public, we can read directly from the csv, otherwise we use data one to get all the data
        isPublic <- datapack::hasAccessRule(sysmeta, "public", "read")

        if (readAllData == TRUE) {
            rowsToRead <- -1
        } else {
            rowsToRead <- 10
        }

        data <- tryCatch({
            if (isPublic == TRUE) {
                if (format == "text/csv") {
                    utils::read.csv(urls[i], nrows = rowsToRead, check.names = FALSE, stringsAsFactors = FALSE)
                } else if (format == "text/tsv") {
                    utils::read.delim(urls[i], nrows = rowsToRead)
                } else if (format == "text/plain") {
                    utils::read.table(urls[i], nrows = rowsToRead)
                } else if (format == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" | format == "application/vnd.ms-excel") {
                    tmp = tempfile()
                    utils::download.file(url = urls[i], destfile = tmp, mode='wb')
                    readxl::read_excel(tmp, n_max = ifelse(rowsToRead == -1, Inf, rowsToRead))
                    unlink(tmp)
                } else if (format == "netCDF-4" | format == "netCDF-3" | format == "CF-1.4" | format ==  "CF-1.3" | format ==  "CF-1.2" | format ==  "CF-1.1"| format ==  "CF-1.0") {
                    tmp = tempfile()
                    utils::download.file(url = urls[i], destfile = tmp, mode='wb')
                    nc <- ncdf4::nc_open(tmp)
                    data <- netcdf_to_dataframe(nc)
                    rm(tmp)
                    rm(nc)  # clean up now because many net-cdfs are large
                    data
                    }
            } else {

                if (format == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" |
                    format == "application/vnd.ms-excel") {
                    cat(paste0("This function uses the DataOne API to get objects, and currently can't read .xls or .xlsx. Check attributes manually."))
                    next
                } else {
                    utils::read.csv(textConnection(rawToChar(dataone::getObject(node, objectpid))), nrows = rowsToRead, check.names = FALSE, stringsAsFactors = FALSE)
                }
            }
        },
        error = function(e) {
            stop(paste0("\nFailed to read file ", urls[i]))
        })

        qa_attributes(node, dataTable, data, readAllData)

        cat(crayon::green(paste0("\n..................Processing complete for object ", objectpid, " (", dataTable@physical[[1]]@objectName, ").................")))
    }
}

#' Test the ORCIDs of creators in a given EML
#'
#' This function is called by \code{\link{qa_package}}.
#' See \code{\link{qa_package}} help documentation for more details.
#'
#' @param eml (eml) Package metadata
#' @return creator_ORCIDs (character). Returns \code{character(0)} if any tests fail.
qa_creators <- function(eml) {
    # Check creators
    creators <- eml@dataset@creator
    creator_ORCIDs <- unlist(eml_get(creators, "userId"))
    isORCID <-  grepl("http[s]?:\\/\\/orcid.org\\/[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}", creator_ORCIDs)
    creator_ORCIDs <- sub("^https://", "http://", creator_ORCIDs)

    if (length(isORCID) != length(creators) || !all(isORCID)) {
        cat(crayon::red("\nEach creator needs to have a proper ORCID."))
        return(character(0))
    } else {
        return(creator_ORCIDs)
    }
}

#' Tests that the rights and access are set for creators for a give pid sysmeta
#'
#' This function is called by \code{\link{qa_package}}.
#' See \code{\link{qa_package}} help documentation for more details.
#'
#' @param sysmeta (sysmeta)  Sysmeta of a given object.
#' @param creator_ORCIDs (character) ORCIDs of creators. Result of \code{\link{qa_creators}}.
qa_access <- function(sysmeta, creator_ORCIDs) {

    # Check rightsHolder
    if (!(sysmeta@rightsHolder %in% creator_ORCIDs)) {
        cat(crayon::yellow("\nThe rightsHolder for", sysmeta@identifier, "is not set to one of the creators"))
    }

    # Check creator access
    for (creator in creator_ORCIDs) {
        creator_read <- datapack::hasAccessRule(sysmeta, creator, "read")
        creator_write <- datapack::hasAccessRule(sysmeta, creator, "write")
        creator_changePermissions <- datapack::hasAccessRule(sysmeta, creator, "changePermission")
        access <- c(creator_read, creator_write, creator_changePermissions)

        if (!all(access)) {
            cat(crayon::yellow("\nFull access for", sysmeta@identifier, "is not set for creator with ORCID", creator))
        }
    }
}

#' Test congruence of attributes and data for a given dataset and dataTable
#'
#' This function is called by \code{\link{qa_package}} but can be used on its own to test congruence
#' between a dataTable and a data object (data.frame). See \code{\link{qa_package}} help documentation for more details.
#'
#' Purpose: QA script to check that attributes match values in the data
#'
#' Functions:
#'     Names: Check that all column names in attributes match the column names in the csv
#'     Possible conditions to account for:
#'     - attributeList does not exist for a csv
#'     - Physical has not been set correctly
#'     - Some of the attributes that exist in the data don't exist in the attributeList
#'     - Some of the attributes that exist in the attributeList don't exist in the data
#'     - There is a typo in one of the attributes or column names so they don't match (maybe covered by above)
#'     Domains: Check that all attribute types match attribute types in the csv
#'     Possible conditions to account for:
#'     - nominal, ordinal, integer, ratio, dateTime
#'     - If domain is enumerated domain, not all enumerated values in the data are accounted for in the enumarated definition
#'     - If domain is enumerated domain, not all enumerated values in the enumerated definition are actually represented in the data
#'     - Type of data does not match type
#'     Values: Check for accidental characters in the csv (one char in a column of ints)
#'
#' @param node (MNode) Member Node where the PID is associated with an object.
#' @param dataTable (dataTable) EML \code{dataTable} or \code{otherEntity} associated with the data object.
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

    header <- as.numeric(dataTable@physical[[1]]@dataFormat@textFormat@numHeaderLines)

    if(length(header) > 0 && !is.na(header) && header > 1) {
        names(data) <- NULL
        names(data) <- data[(header-1),]
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


## Helper function for converting 2-D data from a netCDF to a data.frame object for QA
netcdf_to_dataframe <- function(nc) {
    att_names <- names(nc$var)
    dims <- nc$dim
    dim_names <- c()
    for (i in 1:length(dims)) {
        dim_names[i] <- dims[[i]]$name
    }

    var_names <- c(att_names, dim_names)
    var_names <- var_names[-which(duplicated(tolower(var_names)))]

    data <- lapply(var_names, function(x) ncdf4::ncvar_get(nc, x))
    max_length <- max(unlist(lapply(data, function(x) length(x))))

    results <- data.frame(matrix(ncol = length(data), nrow = max_length))
    names(results) <- var_names
    for (i in seq_along(results)) {
        results[,i] <- rep_len(data[[i]], length.out = max_length)
    }

    return(results)
}


qa_rightsHolder <- function(eml, system_metadata) {
    rightsHolder <- system_metadata@rightsHolder
    creators <- paste0(eml@dataset@creator, collapse = "")
    creator_orcids <- stringr::str_extract_all(creators, "http[s]?:\\/\\/orcid.org\\/[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}")

    # Check rightsHolder
    if (!(rightsHolder %in% creator_orcids)) {
        status <- "FAILURE"
        message <- sprintf("rightsHolder: %s is present in the metadata creators", rightsHolder)
    } else {
        status <- "SUCCESS"
        message <- sprintf("rightsHolder: %s is not present in the metadata creators", rightsHolder)
    }

    result <- list(status = status,
                   output = list(list(value = message)))

    return(result)
}


# Check if creator is present
qa_creator <- function(eml) {
    stopifnot(is(eml, "eml"))

    creators <- eml@dataset@creator

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Output messages will be stored in a list
    messages <- list()

    if (length(creators) <= 0) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "No creators are present."
    } else {
        if (length(creators) == 1) {
            messages[[length(messages) + 1]] <- "One creator is present."
        } else {
            messages[[length(messages) + 1]] <- sprintf("%d creators are present.", length(creators))
        }
    }

    return(list(status = status,
                output = messages))
}


# Check if creator ORCIDs, emails, and addresses are present
qa_creator_orcid <- function(eml) {
    stopifnot(is(eml, "eml"))

    creators <- eml@dataset@creator

    if (length(creators) <= 0) {
        return(list(status = "SKIP", output = "A creator entry is not present. Unable to check for an ORCID, email, or address."))
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Output messages will be stored in a list
    messages <- list()

    # There could be multiple creators, but just one creator with a "userId" will satisfy this check
    userId <- lapply(c(1:length(creators)), function(i) {length(eml@dataset@creator[[i]]@userId@.Data)})
    if (all(userId == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "A user identifier for any creator is not present, so checking for an ORCID is not possible."
    } else {
        creator_ORCIDs <- unlist(EML::eml_get(creators, "userId"))
        has_ORCID <-  grepl("http[s]?:\\/\\/orcid.org\\/[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}", creator_ORCIDs)
        if (suppressWarnings(any(has_ORCID))) {
            messages[[length(messages) + 1]] <- "The user identifier for a creator is an ORCID."
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "The user identifier for any creator is not an ORCID."
        }
    }

    # There could be multiple creators, but just one creator with an "email" will satisfy this check
    email <- lapply(c(1:length(creators)), function(i) {length(eml@dataset@creator[[i]]@electronicMailAddress@.Data)})
    if (all(email == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An email address for any creator is not present."
    } else {
        creator_emails <- unlist(EML::eml_get(creators, "electronicMailAddress"))
        has_email <-  grepl("@", creator_emails)
        if (suppressWarnings(any(has_email))) {
            messages[[length(messages) + 1]] <- "An email address for a creator is present."
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "An email address for any creator is not present."
        }
    }

    # There could be multiple creators, but just one creator with an "address" will satisfy this check
    address <- lapply(c(1:length(creators)), function(i) {length(eml@dataset@creator[[i]]@address@.Data)})
    if (all(address == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An address for any creator is not present."
    } else {
        messages[[length(messages) + 1]] <- "An address for a creator is present."
    }

    return(list(status = status,
                output = messages))
}


# Check if all creators have email and address
qa_creator_info <- function(eml) {
    stopifnot(is(eml, "eml"))

    creators <- eml@dataset@creator

    if (length(creators) <= 0) {
        return(list(status = "SKIP", output = "A creator entry is not present. Unable to check for email or address."))
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Output messages will be stored in a list
    messages <- list()

    # Check number of creators that have email addresses
    creator_emails <- unlist(EML::eml_get(creators, "electronicMailAddress"))
    if (length(creator_emails) == length(creators)) {
        messages[[length(messages) + 1]] <- "All creators have email addresses."
    } else {
        messages[[length(messages) + 1]] <- sprintf("%d of %d creators have email addresses.", length(creator_emails), length(creators))
    }

    # Check number of creators that have addresses
    creator_addresses <- unlist(EML::eml_get(creators, "deliveryPoint"))
    if (length(creator_addresses) == length(creators)) {
        messages[[length(messages) + 1]] <- "All creators have addresses."
    } else {
        messages[[length(messages) + 1]] <- sprintf("%d of %d creators have addresses.", length(creator_addresses), length(creators))
    }

    return(list(status = status,
                output = messages))
}


# Check if contact is present
qa_contact <- function(eml) {
    stopifnot(is(eml, "eml"))

    contacts <- eml@dataset@contact

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Output messages will be stored in a list
    messages <- list()

    if (length(contacts) <= 0) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "No contacts are present."
    } else {
        if (length(contacts) == 1) {
            messages[[length(messages) + 1]] <- "One contact is present."
        } else {
            messages[[length(messages) + 1]] <- sprintf("%d contacts are present.", length(contacts))
        }
    }

    return(list(status = status,
                output = messages))
}


# Check if contact ORCIDs, emails, and addresses are present
qa_contact_orcid <- function(eml) {
    stopifnot(is(eml, "eml"))

    contacts <- eml@dataset@contact

    if (length(contacts) <= 0) {
        return(list(status = "SKIP", output = "A contact entry is not present. Unable to check for an ORCID, email, or address."))
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Output messages will be stored in a list
    messages <- list()

    # There could be multiple contacts, but just one contact with a "userId" will satisfy this check
    userId <- lapply(c(1:length(contacts)), function(i) {length(eml@dataset@contact[[i]]@userId@.Data)})
    if (all(userId == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "A user identifier for any contact is not present, so checking for an ORCID is not possible."
    } else {
        contact_ORCIDs <- unlist(EML::eml_get(contacts, "userId"))
        has_ORCID <-  grepl("http[s]?:\\/\\/orcid.org\\/[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}", contact_ORCIDs)
        if (suppressWarnings(any(has_ORCID))) {
            messages[[length(messages) + 1]] <- "The user identifier for a contact is an ORCID."
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "The user identifier for any contact is not an ORCID."
        }
    }

    # There could be multiple contacts, but just one contact with an "email" will satisfy this check
    email <- lapply(c(1:length(contacts)), function(i) {length(eml@dataset@contact[[i]]@electronicMailAddress@.Data)})
    if (all(email == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An email address for any contact is not present."
    } else {
        contact_emails <- unlist(EML::eml_get(contacts, "electronicMailAddress"))
        has_email <-  grepl("@", contact_emails)
        if (suppressWarnings(any(has_email))) {
            messages[[length(messages) + 1]] <- "An email address for a contact is present."
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "An email address for any contact is not present."
        }
    }

    # There could be multiple contacts, but just one contact with an "address" will satisfy this check
    address <- lapply(c(1:length(contacts)), function(i) {length(eml@dataset@contact[[i]]@address@.Data)})
    if (all(address == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An address for any contact is not present."
    } else {
        messages[[length(messages) + 1]] <- "An address for a contact is present."
    }

    return(list(status = status,
                output = messages))
}


# Check if all contacts have email and address
qa_contact_info <- function(eml) {
    stopifnot(is(eml, "eml"))

    contacts <- eml@dataset@contact

    if (length(contacts) <= 0) {
        return(list(status = "SKIP", output = "A contact entry is not present. Unable to check for email or address."))
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Output messages will be stored in a list
    messages <- list()

    # Check number of contacts that have email addresses
    contact_emails <- unlist(EML::eml_get(contacts, "electronicMailAddress"))
    if (length(contact_emails) == length(contacts)) {
        messages[[length(messages) + 1]] <- "All contacts have email addresses."
    } else {
        messages[[length(messages) + 1]] <- sprintf("%d of %d contacts have email addresses.", length(contact_emails), length(contacts))
    }

    # Check number of contacts that have addresses
    contact_addresses <- unlist(EML::eml_get(contacts, "deliveryPoint"))
    if (length(contact_addresses) == length(contacts)) {
        messages[[length(messages) + 1]] <- "All contacts have addresses."
    } else {
        messages[[length(messages) + 1]] <- sprintf("%d of %d contacts have addresses.", length(contact_addresses), length(contacts))
    }

    return(list(status = status,
                output = messages))
}
