# QA for metadata based on EML 2.1.1
# See EML specs at https://knb.ecoinformatics.org/#external//emlparser/docs/eml-2.1.1/


#' Check package including congruence of attributes and data
#'
#' This function checks that the attributes listed in the metadata match the values in the data for each
#' tabular data object. It may also optionally check if all creators have ORCIDs and have full access
#' to all elements of the data package.
#'
#' @importFrom methods is
#' @import crayon
#' @import dataone
#' @importFrom datapack hasAccessRule
#' @import EML
#' @import arcticdatautils
#' @importFrom utils read.csv
#' @importFrom utils read.delim
#' @importFrom utils read.table
#' @importFrom utils download.file
#' @importFrom readxl read_excel
#' @importFrom sf read_sf
#' @importFrom sf st_set_geometry
#'
#' @param node (MNode) Member Node where the PID is located.
#' @param pid (character) The PID of a resource map.
#' @param readAllData (logical) Default TRUE. Read all data from remote and check that column types match attributes,
#' otherwise only pull first 10 rows. Only applicable to public packages (private packages will read complete dataset).
#' If \code{check_attributes = FALSE}, no rows will be read.
#' @param check_attributes (logical) Default TRUE. Checks congruence of attributes and data.
#' @param check_creators (logical) Default FALSE. Checks if each creator has an ORCID.
#' Will also run if \code{check_access = TRUE}.
#' @param check_access (logical) Default FALSE. Checks if each creator has full access to the
#' metadata, resource map, and data objects. Will not run if the checks associated with \code{check_creators} fail.
#'
#' @return invisible
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run QA checks
#'
#' qa_package(mn, pid, readAllData = FALSE, check_attributes = TRUE,
#'            check_creators = FALSE, check_access = FALSE)
#' }
qa_package <- function(node, pid, readAllData = TRUE, check_attributes = TRUE, check_creators = FALSE, check_access = FALSE) {
    stopifnot(class(node) %in% c("MNode", "CNode"))
    stopifnot(is.character(pid), nchar(pid) > 0)

    supported_file_formats <- c("text/csv",
                                "text/tsv",
                                "text/plain",
                                "application/vnd.ms-excel",
                                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                "application/zip",
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

    cat(crayon::green(paste0("\n.............Processing package ", package$resource_map, "..................")))

    # Check creators
    if (check_creators || check_access) {
        creator_ORCIDs <- qa_creator_ORCIDs(eml)
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

    urls_dataTable <- unique(unlist(EML::eml_get(eml@dataset@dataTable, "url"), recursive = TRUE))
    urls_otherEntity <- unique(unlist(EML::eml_get(eml@dataset@otherEntity, "url"), recursive = TRUE))
    urls_spatialVector <- unique(unlist(EML::eml_get(eml@dataset@spatialVector, "url"), recursive = TRUE))
    urls <- unique(c(urls_dataTable, urls_otherEntity, urls_spatialVector))

    # Check that each data object has a matching URL in the EML
    wrong_URL <- FALSE
    for (datapid in package$data) {
        n <- which(grepl(paste0(datapid, "$"), urls))
        if (length(n) != 1) {
            cat(crayon::red(paste0("\nThe URL/distribution for ", datapid, " is missing/incongruent in the physical section of the EML.")))
            wrong_URL <- TRUE
        }
    }

    if (length(urls) != length(package$data) || wrong_URL) {
        # Stop here to ensure proper ordering in the following loops
        stop("\nAll distribution URLs in the EML must match the package's data URLs to continue.",
             "\nPlease fix and try again.")
    }

    for (objectpid in package$data) {
        n_dT <- which(grepl(paste0(objectpid, "$"), urls_dataTable))
        n_oE <- which(grepl(paste0(objectpid, "$"), urls_otherEntity))
        n_sV <- which(grepl(paste0(objectpid, "$"), urls_spatialVector))

        if (length(n_dT) == 1) {
            dataTable <- eml@dataset@dataTable[[n_dT]]
            urls <- urls_dataTable
            i <- n_dT
        } else if (length(n_oE) == 1) {
            dataTable <- eml@dataset@otherEntity[[n_oE]]
            urls <- urls_otherEntity
            i <- n_oE
        } else if (length(n_sV) == 1) {
            dataTable <- eml@dataset@spatialVector[[n_sV]]
            urls <- urls_spatialVector
            i <- n_sV
        } else {
            next
        }

        sysmeta <- dataone::getSystemMetadata(node, objectpid)

        if (check_access && length(creator_ORCIDs) > 0) {
            qa_access(sysmeta, creator_ORCIDs)
        }

        if (!check_attributes) next

        # If object is not tabular data, skip to next object
        format <- sysmeta@formatId
        if (!format %in% supported_file_formats) next

        cat(crayon::green(paste0("\n\n..................Processing object ", objectpid,
                                 " (", dataTable@physical[[1]]@objectName, ").................")))

        if (is.null(EML::get_attributes(dataTable@attributeList)$attributes)) {
            cat(crayon::red(paste0("\nEmpty attribute table for object at", dataTable@physical[[1]]@distribution[[1]]@online@url)))
            cat(crayon::green(paste0("\n..................Processing complete for object ", objectpid,
                                     " (", dataTable@physical[[1]]@objectName, ").................")))
            next
        }

        # If package is public, read directly from the file; otherwise, use DataONE API
        isPublic <- datapack::hasAccessRule(sysmeta, "public", "read")

        if (readAllData) {
            rowsToRead <- -1
        } else {
            rowsToRead <- 10
        }

        data <- tryCatch({
            if (isPublic) {
                if (format == "text/csv") {
                    utils::read.csv(urls[i], nrows = rowsToRead, check.names = FALSE, stringsAsFactors = FALSE)
                } else if (format == "text/tsv") {
                    utils::read.delim(urls[i], nrows = rowsToRead)
                } else if (format == "text/plain") {
                    utils::read.table(urls[i], nrows = rowsToRead)
                } else if (format == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" || format == "application/vnd.ms-excel") {
                    tmp <- tempfile()
                    utils::download.file(url = urls[i], destfile = tmp, mode = "wb", quiet = TRUE)
                    data <- readxl::read_excel(tmp, n_max = ifelse(rowsToRead == -1, Inf, rowsToRead))
                    unlink(tmp)
                    data
                } else if (format == "application/zip") {
                    # Many formats can exist within a .zip file; skip if not spatial data
                    tmp <- tempfile()
                    utils::download.file(url = urls[i], destfile = tmp, quiet = TRUE)
                    tmp2 <- tempfile()
                    utils::unzip(tmp, exdir = tmp2)
                    t <- list.files(tmp2, full.names = TRUE, recursive = TRUE)
                    if (any(grep("*\\.shp", t))) {
                        data <- suppressWarnings(sf::read_sf(t[grep("*\\.shp", t)]) %>% sf::st_set_geometry(NULL))
                        cat(crayon::yellow("\nNote: Shapefiles have attribute name limits of 10 characters."))
                    } else if (any(grep("*\\.gdb", t))) {
                        data <- suppressWarnings(sf::read_sf(list.dirs(tmp2)[2]) %>% sf::st_set_geometry(NULL))
                    } else {
                        cat(crayon::yellow("\nSpatial data not present within .zip file."))
                        cat(crayon::green(paste0("\n..................Processing complete for object ", objectpid,
                                                 " (", dataTable@physical[[1]]@objectName, ").................")))
                        next
                    }
                    unlink(c(tmp, tmp2), recursive = TRUE)
                    data
                } else if (format == "netCDF-4" || format == "netCDF-3" || format == "CF-1.4" || format == "CF-1.3" ||
                           format == "CF-1.2" || format == "CF-1.1" || format == "CF-1.0") {
                    tmp <- tempfile()
                    utils::download.file(url = urls[i], destfile = tmp, mode = "wb", quiet = TRUE)
                    nc <- ncdf4::nc_open(tmp)
                    data <- netcdf_to_dataframe(nc)
                    unlink(tmp)
                    rm(nc) # clean up now because many netCDF files are large
                    data
                }
            } else {
                if (format == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" || format == "application/vnd.ms-excel") {
                    cat(crayon::yellow("This function uses the DataONE API to get objects and currently cannot read private .xls or .xlsx files. Check attributes manually."))
                    cat(crayon::green(paste0("\n..................Processing complete for object ", objectpid,
                                             " (", dataTable@physical[[1]]@objectName, ").................")))
                    next
                } else {
                    utils::read.csv(textConnection(rawToChar(dataone::getObject(node, objectpid))),
                                    nrows = rowsToRead, check.names = FALSE, stringsAsFactors = FALSE)
                }
            }
        },
        error = function(e) {
            stop(paste0("\nFailed to read file at ", urls[i]))
        })

        qa_attributes(dataTable, data, readAllData)

        cat(crayon::green(paste0("\n..................Processing complete for object ", objectpid,
                                 " (", dataTable@physical[[1]]@objectName, ").................")))
    }

    cat(crayon::green(paste0("\n\n.............Processing complete for package ",
                             package$resource_map, "..................")))

    return(invisible())
}


#' Check the ORCIDs of creators in a given EML
#'
#' This function is called by \code{\link{qa_package}}.
#' See \code{\link{qa_package}} documentation for more details.
#'
#' @param eml (eml) Package metadata.
#'
#' @return creator_ORCIDs (character) Returns \code{character(0)} if any tests fail.
qa_creator_ORCIDs <- function(eml) {
    # Check creators
    creators <- eml@dataset@creator
    creator_ORCIDs <- unlist(eml_get(creators, "userId"))
    isORCID <-  grepl("http[s]?:\\/\\/orcid.org\\/[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}", creator_ORCIDs)
    creator_ORCIDs <- sub("^https://", "http://", creator_ORCIDs)

    if (length(isORCID) != length(creators) || !all(isORCID)) {
        cat(crayon::red("\nEach creator needs to have a proper ORCID."))
        return(character(0))
    } else {
        return(creator_ORCIDs)
    }
}


#' Check rights and access for creators in sysmeta
#'
#' This function is called by \code{\link{qa_package}}.
#' See \code{\link{qa_package}} documentation for more details.
#'
#' @param sysmeta (sysmeta)  Sysmeta of a given object.
#' @param creator_ORCIDs (character) ORCIDs of creators. Result of \code{\link{qa_creator_ORCIDs}}.
qa_access <- function(sysmeta, creator_ORCIDs) {
    # Check rightsHolder
    if (!(sysmeta@rightsHolder %in% creator_ORCIDs)) {
        cat(crayon::yellow("\nThe rightsHolder for", sysmeta@identifier, "is not set to one of the creators."))
    }

    # Check creator access
    for (creator in creator_ORCIDs) {
        creator_read <- datapack::hasAccessRule(sysmeta, creator, "read")
        creator_write <- datapack::hasAccessRule(sysmeta, creator, "write")
        creator_changePermission <- datapack::hasAccessRule(sysmeta, creator, "changePermission")
        access <- c(creator_read, creator_write, creator_changePermission)

        if (!all(access)) {
            cat(crayon::yellow("\nFull access for", sysmeta@identifier, "is not set for creator with ORCID", creator))
        }
    }
}


#' Check congruence of attributes and data for a given dataset and dataTable
#'
#' This function is called by \code{\link{qa_package}} but can be used on its own to test congruence
#' between a dataTable and a data object (data.frame). See \code{\link{qa_package}} documentation for more details.
#'
#' Purpose: QA function to check that attributes match values in the data
#'
#' Functions:
#' \itemize{
#'     \item Names: Check that column names in attributes match column names in data frame. Possible conditions to check for:
#'     \itemize{
#'         \item attributeList does not exist for data frame
#'         \item Physical has not been set correctly
#'         \item Some of the attributes that exist in the data do not exist in the attributeList
#'         \item Some of the attributes that exist in the attributeList do not exist in the data
#'         \item Typos in attribute or column names resulting in nonmatches
#'     }
#'     \item Domains: Check that attribute types match attribute types in data frame. Possible conditions to check for:
#'     \itemize{
#'         \item nominal, ordinal, integer, ratio, dateTime
#'         \item If domain is enumerated domain, enumerated values in the data are accounted for in the enumerated definition
#'         \item If domain is enumerated domain, enumerated values in the enumerated definition are all represented in the data
#'         \item Type of data does not match attribute type
#'     }
#'     \item Values: Check for accidental characters in the data frame (one character in a column of integers)
#' }
#'
#' @param dataTable (dataTable) EML \code{dataTable}, \code{otherEntity}, or \code{spatialVector} associated with the data object.
#' @param data (data.frame) Data frame of data object.
#' @param checkEnumeratedDomains (logical) Default TRUE. Compare unique values in data to defined enumerated domains in EML.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run attribute checks on a data.frame and its associated EML dataTable
#'
#' qa_attributes(dataTable, dataObject)
#' }
qa_attributes <- function(dataTable, data, checkEnumeratedDomains = TRUE) {
    attributeTable <- EML::get_attributes(dataTable@attributeList)
    attributeNames <- attributeTable$attributes$attributeName

    if (is.null(attributeNames)) {
        cat(crayon::red(paste0("\nEmpty attribute table for object at ", dataTable@physical[[1]]@distribution[[1]]@online@url)))
    }

    header <- as.numeric(dataTable@physical[[1]]@dataFormat@textFormat@numHeaderLines)

    if (length(header) > 0 && !is.na(header) && header > 1) {
        names(data) <- NULL
        names(data) <- data[(header - 1), ]
    }

    dataCols <- colnames(data)

    # Check for attribute correctness according to the EML schema using arcticdatautils::eml_validate_attributes
    attOutput <- utils::capture.output(arcticdatautils::eml_validate_attributes(dataTable@attributeList))
    attErrors <- which(grepl("FALSE", utils::head(attOutput, -2)))

    if (length(attErrors) > 0) {
        print(attOutput[attErrors])
    }

    # Check that attribute names match column names
    allequal <- isTRUE(all.equal(dataCols, attributeNames))

    if (allequal == FALSE) {
        intersection <- intersect(attributeNames, dataCols)
        nonmatcheml <- attributeNames[!attributeNames %in% intersection]
        nonmatchdata <- dataCols[!dataCols %in% intersection]

        # EML has values that data does not have
        if (length(nonmatcheml) > 0) {
            cat(crayon::red(paste0("\nThe EML dataTable includes attributes '", toString(nonmatcheml, sep = ", "), "' that are not present in the data.")))
            cat(crayon::yellow("\nContinuing attribute and data matching WITHOUT mismatched attributes - fix issues and re-run the function after first round completion."))
        }

        # Data has values that EML does not have
        if (length(nonmatchdata) > 0) {
            cat(crayon::red(paste0("\nThe data includes attributes '", toString(nonmatchdata, sep = ", "), "' that are not present in the EML.")))
            cat(crayon::yellow("\nContinuing attribute and data matching WITHOUT mismatched attributes - fix issues and re-run the function after first round completion."))
        }

        # Values match but are not ordered correctly
        if (length(nonmatcheml) == 0 && length(nonmatchdata) == 0 && allequal == FALSE) {
            cat(crayon::yellow("\nAttributes in the attribute table match column names but are incorrectly ordered."))
        }

        data <- data[ , which(colnames(data) %in% intersection)]
        attributeTable$attributes <- attributeTable$attributes[which(attributeTable$attributes$attributeName %in% intersection), ]
        attributeTable$attributes <- attributeTable$attributes[order(match(attributeTable$attributes$attributeName, colnames(data))), ]
    }

    # Check that type of column matches type of data based on acceptable DataONE formats
    for (i in seq_along(data)) {
        matchingAtt <- attributeTable$attributes[i, ]
        attClass <- class(data[ , i])

        # TODO: If matching Att has a datetime domain, try coercing the column in R based on the date time format
        # if (matchingAtt$measurementScale == "dateTime") {
        #
        # }

        if (attClass == "numeric" || attClass == "integer" || attClass == "double") {
            if (matchingAtt$measurementScale != "ratio" && matchingAtt$measurementScale != "interval" && matchingAtt$measurementScale != "dateTime") {
                cat(crayon::yellow(paste0(c("\nWarning: Mismatch in attribute type for the attribute '", matchingAtt$attributeName,
                                            "'. Type of data is ", attClass, " which should probably have interval or ratio measurementScale in EML, not ",
                                            matchingAtt$measurementScale, "."), collapse = "")))
            }
        } else if (attClass == "character" || attClass == "logical") {
            if (matchingAtt$measurementScale != "nominal" && matchingAtt$measurementScale != "ordinal") {
                cat(crayon::yellow(paste0(c("\nWarning: Mismatch in attribute type for the attribute '", matchingAtt$attributeName,
                                            "'. Type of data is ", attClass, " which should probably have nominal or ordinal measurementScale in EML, not ",
                                            matchingAtt$measurementScale, "."), collapse = "")))
            }
        }
    }

    # Check that enumerated domains match values in data
    if (checkEnumeratedDomains == TRUE) {
        if (length(attributeTable$factors) > 0) {
            for (i in seq_along(unique(attributeTable$factors$attributeName))) {
                emlAttName <- unique(attributeTable$factors$attributeName)[i]
                emlUniqueValues <- attributeTable$factors[attributeTable$factors$attributeName == emlAttName, "code"]

                dataUniqueValues <- unique(data[[which(colnames(data) == emlAttName)]])

                intersection <- intersect(dataUniqueValues, emlUniqueValues)
                nonmatcheml <- emlUniqueValues[!emlUniqueValues %in% intersection]
                nonmatchdata <- dataUniqueValues[!dataUniqueValues %in% intersection]

                if (length(nonmatcheml) > 0) {
                    cat(crayon::red(paste("\nThe EML contains the following enumerated domain values for the attribute",
                                          paste0("'", as.character(emlAttName), "'"),
                                          "that do not appear in the data: ", toString(nonmatcheml, sep = ", "))))
                }

                if (length(nonmatchdata) > 0) {
                    cat(crayon::red(paste("\nThe data contains the following enumerated domain values for the attribute",
                                          paste0("'", as.character(emlAttName), "'"),
                                          "that do not appear in the EML: ", toString(nonmatchdata, sep = ", "))))
                }
            }
        }
        # If there are any missing values in the data, check that there is an associated missing value code in the EML
        for (i in which(colSums(is.na(data)) > 0)) {
            attribute <- attributeTable$attributes[i, ]
            if (is.na(attribute$missingValueCode)) {
                cat(crayon::red(paste0("\nThe attribute '", attribute$attributeName, "' contains missing values but does not have a missing value code.")))
            }
        }
    }
}


# Helper function for converting 2-D data from a netCDF to a data.frame object
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


# Check if rights holder is present in creators
qa_rightsHolder <- function(eml, system_metadata) {
    rightsHolder <- system_metadata@rightsHolder
    creators <- paste0(eml@dataset@creator, collapse = "")
    creator_orcids <- stringr::str_extract_all(creators, "http[s]?:\\/\\/orcid.org\\/[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}")

    if (!(rightsHolder %in% creator_orcids)) {
        return(list(status = "FAILURE",
                    output = sprintf("rightsHolder: %s is present in the metadata creators", rightsHolder)))
    } else {
        return(list(status = "SUCCESS",
                    output = sprintf("rightsHolder: %s is not present in the metadata creators", rightsHolder)))
    }
}


#' Check if title is present with sufficient length
#'
#' This function checks if a title is present with the sufficient length.
#' A title should be between 7 and 20 words.
#'
#' @importFrom stringr str_split
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
# TODO: check number of words in multiple titles (title[[i]])
qa_title <- function(input) {
    if (methods::is(input, "eml")) {
        title <- input@dataset@title
    } else if (any(c("ListOftitle", "character") %in% class(input))) {
        title <- input
    } else {
        stop("Input should be of class 'eml', 'ListOftitle', or 'character'.")
    }

    if (length(title) == 0) {
        return(list(status = "FAILURE",
                    output = "No title is present."))
    } else {
        # Required minimum word count
        req_min_count <- 5
        # Recommended minimum word count
        rec_min_count <- 7
        # Recommended max word count
        rec_max_count <- 20

        if (length(title) == 1) {
            if (class(input) == "character") { # For class 'eml', is.character(input) returns TRUE; use alternative method instead
                title2 <- title[[1]]
            } else {
                title2 <- title[[1]]@.Data
            }

            word_count <- length(stringr::str_split(title2, "\\s+", simplify = TRUE))

            if (word_count < req_min_count) {
                return(list(status = "FAILURE",
                            output = sprintf("One title is present with %d words. The minimum required word count is %d.",
                                             word_count, req_min_count)))
            } else if (word_count < rec_min_count) {
                return(list(status = "FAILURE",
                            output = sprintf("One title is present with %d words. The minimum recommended word count is %d.",
                                             word_count, rec_min_count)))
            } else if (word_count > rec_max_count) {
                return(list(status = "FAILURE",
                            output = sprintf("One title is present with %d words. The maximum recommended word count is %d.",
                                             word_count, rec_max_count)))
            } else {
                return(list(status = "SUCCESS",
                            output = sprintf("One title is present with %d words.", word_count)))
            }
        } else {
            return(list(status = "SUCCESS",
                        output = sprintf("%d titles are present.", length(title))))
        }
    }
}


#' Check if publication date is present with correct format
#'
#' This function checks if a publication date is present with the
#' correct format (YYYY or YYYY-MM-DD).
#'
#' @importFrom lubridate ymd
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_pubDate <- function(input) {
    if (methods::is(input, "eml")) {
        pubDate <- input@dataset@pubDate@.Data
    } else if (methods::is(input, "pubDate")) {
        pubDate <- input@.Data
    } else if (methods::is(input, "character")) {
        pubDate <- input
    } else {
        stop("Input should be of class 'eml', 'pubDate', or 'character'.")
    }

    # Publication dates should be in YYYY or YYYY-MM-DD format
    if (length(pubDate) == 0) {
        return(list(status = "FAILURE",
                    output = "No publication date is present."))
    } else {
        if (nchar(pubDate) == 4 && is.na(suppressWarnings(as.numeric(pubDate)))) {
            return(list(status = "FAILURE",
                        output = "A publication date is present but should be in YYYY or YYYY-MM-DD format."))
        } else if (nchar(pubDate) == 4 && !is.na(as.numeric(pubDate))) {
            return(list(status = "SUCCESS",
                        output = "A publication date is present with YYYY format."))
        } else if (nchar(pubDate) == 10 && is.na(suppressWarnings(lubridate::ymd(pubDate)))) {
            return(list(status = "FAILURE",
                        output = "A publication date is present but should be in YYYY or YYYY-MM-DD format."))
        } else if (nchar(pubDate) == 10 && !is.na(lubridate::ymd(pubDate))) {
            return(list(status = "SUCCESS",
                        output = "A publication date is present with YYYY-MM-DD format."))
        } else {
            return(list(status = "FAILURE",
                        output = "A publication date is present but should be in YYYY or YYYY-MM-DD format."))
        }
    }
}


#' Check if abstract is present with sufficient length
#'
#' This function checks if an abstract is present with sufficient length.
#' At least 100 words are required.
#'
#' importFrom stringr str_split
#' importFrom xml2 as_list
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_abstract <- function(input) {
    if (methods::is(input, "eml")) {
        # Usually 'abstract' has a 'para' slot but sometimes not
        if (length(input@dataset@abstract@para) == 0) {
            abstract <- input@dataset@abstract@.Data
            length_abstract <- length(abstract)
        } else {
            # There can be multiple 'para' but this code only extracts the first
            abstract <- unlist(xml2::as_list(input@dataset@abstract@para[[1]]@.Data[[1]]))
            length_abstract <- length(abstract)
        }
    } else if (methods::is(input, "abstract")) {
        if (length(input@para) == 0) {
            abstract <- input@.Data
            length_abstract <- length(abstract)
        } else {
            abstract <- unlist(xml2::as_list(input@para[[1]]@.Data[[1]]))
            length_abstract <- length(abstract)
        }
    } else if (methods::is(input, "character")) {
        abstract <- input
        length_abstract <- length(abstract)
    } else {
        stop("Input should be of class 'eml', 'abstract', or 'character'.")
    }

    if (length_abstract == 0) {
        return(list(status = "FAILURE",
                    output = "An abstract is not present."))
    } else if (length_abstract > 1) {
        return(list(status = "FAILURE",
                    output = "More than one abstract is present. Only one is allowed."))
    } else {
        # Split abstract into words and trim whitespace
        tokens <- trimws(stringr::str_split(abstract, "\\s+")[[1]], which = "both")
        # Remove any blank elements
        tokens <- tokens[tokens != ""]

        if (length(tokens) >= 100) {
            return(list(status = "SUCCESS",
                        output = sprintf("The abstract is present and %d words long.", length(tokens))))
        } else {
            return(list(status = "FAILURE",
                        output = sprintf("The abstract is present but only %d words long. 100 or more words are required.", length(tokens))))
        }
    }
}


#' Check if keywords are present
#'
#' This function checks if keywords are present. At least one
#' keyword is recommended.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_keywordSet <- function(input) {
    if (methods::is(input, "eml")) {
        key <- input@dataset@keywordSet
    } else if (any(c("ListOfkeywordSet", "character") %in% class(input))) {
        key <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfkeywordSet', or 'character'.")
    }

    if (length(key) == 0) {
        return(list(status = "FAILURE",
                    output = "No keywords are present. At least one keyword is recommended."))
    }

    # Count number of keywords if present
    if (class(input) == "character") { # For class 'eml', is.character(input) returns TRUE; use alternative method instead
        length_key <- length(key)
    } else {
        length_key <- length(key[[1]]@keyword)
    }

    if (length_key == 1) {
        return(list(status = "SUCCESS",
                    output = "One keyword is present."))
    } else {
        return(list(status = "SUCCESS",
                    output = sprintf("%d keywords are present.", length_key)))
    }
}


#' Check if data usage rights are present
#'
#' This function checks if data usage rights are present and are either CC-BY or CC-0.
#'
#' @importFrom stringr str_detect
#' @importFrom xml2 as_list
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_intellectualRights <- function(input) {
    if (methods::is(input, "eml")) {
        # Usually 'intellectualRights' has a 'para' slot but sometimes not
        if (length(input@dataset@intellectualRights@para) == 0) {
            rights <- input@dataset@intellectualRights@.Data
            length_rights <- length(rights)
        } else {
            # There can be multiple 'para' but this code only extracts the first
            rights <- unlist(xml2::as_list(input@dataset@intellectualRights@para[[1]]@.Data[[1]]))
            length_rights <- length(rights)
        }
    } else if (methods::is(input, "intellectualRights")) {
        if (length(input@para) == 0) {
            rights <- input@.Data
            length_rights <- length(rights)
        } else {
            rights <- unlist(xml2::as_list(input@para[[1]]@.Data[[1]]))
            length_rights <- length(rights)
        }
    } else if (methods::is(input, "character")) {
        rights <- input
        length_rights <- length(rights)
    } else {
        stop("Input should be of class 'eml', 'intellectualRights', or 'character'.")
    }

    # Most data packages should be CC-BY or CC-0
    phrases <- c("http[s]*://creativecommons.org/licenses/by/4.0", "http[s]*://creativecommons.org/publicdomain/zero/1.0")

    if (length_rights == 0) {
        return(list(status = "FAILURE",
                    output = "Intellectual rights are not present."))
    } else if (length_rights > 1) {
        return(list(status = "FAILURE",
                    output = "Multiple intellectual rights are present. Only one is allowed."))
    } else {
        if (stringr::str_detect(rights, phrases[[1]])) {
            return(list(status = "SUCCESS",
                        output = "Intellectual rights are present as a CC-BY license."))
        } else if (stringr::str_detect(rights, phrases[[2]])) {
            return(list(status = "SUCCESS",
                        output = "Intellectual rights are present as a CC-0 license."))
        } else {
            return(list(status = "FAILURE",
                        output = "Intellectual rights are present but do not indicate a CC-BY or CC-0 license."))
        }
    }
}


#' Check if creator is present
#'
#' This function checks if a creator is present.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_creator <- function(input) {
    if (methods::is(input, "eml")) {
        creators <- input@dataset@creator
    } else if (any(c("ListOfcreator", "character") %in% class(input))) {
        creators <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfcreator', or 'character'.")
    }

    if (length(creators) == 0) {
        return(list(status = "FAILURE",
                    output = "No creators are present."))
    } else {
        if (length(creators) == 1) {
            return(list(status = "SUCCESS",
                        output = "One creator is present."))
        } else {
            return(list(status = "SUCCESS",
                        output = sprintf("%d creators are present.", length(creators))))
        }
    }
}


#' Check if creator information is present
#'
#' This function checks if creator information is present, including an ID, email, and address.
#'
#' @import EML
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_creator_info <- function(input) {
    if (methods::is(input, "eml")) {
        creators <- input@dataset@creator
    } else if (methods::is(input, "ListOfcreator")) {
        creators <- input
    } else {
        stop("Input should be of class 'eml' or 'ListOfcreator'.")
    }

    if (length(creators) == 0) {
        return(list(status = "FAILURE",
                    output = "A creator entry is not present. Unable to check for an ORCID, email, or address."))
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Output messages will be stored in a vector
    messages <- c()

    # There could be multiple creators, but just one creator with an ID will satisfy this check
    userId <- lapply(c(1:length(creators)), function(i) {length(creators[[i]]@userId@.Data)})
    if (all(userId == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "A user identifier for any creator is not present. Unable to check for an ORCID."
    } else {
        creator_ORCIDs <- unlist(EML::eml_get(creators, "userId"))
        has_ORCID <-  grepl("http[s]?:\\/\\/orcid.org\\/[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}", creator_ORCIDs)
        if (suppressWarnings(any(has_ORCID))) {
            messages[[length(messages) + 1]] <- sprintf("%d of %d creators have an ORCID.", length(creator_ORCIDs), length(creators))
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "The user identifier for any creator is not an ORCID."
        }
    }

    # There could be multiple creators, but just one creator with an email will satisfy this check
    email <- lapply(c(1:length(creators)), function(i) {length(creators[[i]]@electronicMailAddress@.Data)})
    if (all(email == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An email address for any creator is not present."
    } else {
        creator_emails <- unlist(EML::eml_get(creators, "electronicMailAddress"))
        has_email <-  grepl("@", creator_emails)
        if (suppressWarnings(any(has_email))) {
            messages[[length(messages) + 1]] <- sprintf("%d of %d creators have email addresses.", length(creator_emails), length(creators))
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "An email address for any creator is not present."
        }
    }

    # There could be multiple creators, but just one creator with an address will satisfy this check
    address <- lapply(c(1:length(creators)), function(i) {length(creators[[i]]@address@.Data)})
    if (all(address == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An address for any creator is not present."
    } else {
        creator_addresses <- unlist(EML::eml_get(creators, "deliveryPoint"))
        messages[[length(messages) + 1]] <- sprintf("%d of %d creators have addresses.", length(creator_addresses), length(creators))
    }

    return(list(status = status,
                output = messages))
}


#' Check if contact is present
#'
#' This function checks if a contact is present.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_contact <- function(input) {
    if (methods::is(input, "eml")) {
        contacts <- input@dataset@contact
    } else if (any(c("ListOfcontact", "character") %in% class(input))) {
        contacts <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfcontact', or 'character'.")
    }

    if (length(contacts) == 0) {
        return(list(status = "FAILURE",
                    output = "No contacts are present."))
    } else {
        if (length(contacts) == 1) {
            return(list(status = "SUCCESS",
                        output = "One contact is present."))
        } else {
            return(list(status = "SUCCESS",
                        output = sprintf("%d contacts are present.", length(contacts))))
        }
    }
}


#' Check if contact information is present
#'
#' This function checks if contact information is present, including an ID, email, and address.
#'
#' @import EML
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_contact_info <- function(input) {
    if (methods::is(input, "eml")) {
        contacts <- input@dataset@contact
    } else if (methods::is(input, "ListOfcontact")) {
        contacts <- input
    } else {
        stop("Input should be of class 'eml' or 'ListOfcontact'.")
    }

    if (length(contacts) <= 0) {
        return(list(status = "FAILURE",
                    output = "A contact entry is not present. Unable to check for an ORCID, email, or address."))
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Output messages will be stored in a vector
    messages <- c()

    # There could be multiple contacts, but just one contact with an ID will satisfy this check
    userId <- lapply(c(1:length(contacts)), function(i) {length(contacts[[i]]@userId@.Data)})
    if (all(userId == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "A user identifier for any contact is not present. Unable to check for an ORCID."
    } else {
        contact_ORCIDs <- unlist(EML::eml_get(contacts, "userId"))
        has_ORCID <-  grepl("http[s]?:\\/\\/orcid.org\\/[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}", contact_ORCIDs)
        if (suppressWarnings(any(has_ORCID))) {
            messages[[length(messages) + 1]] <- sprintf("%d of %d contacts have an ORCID.", length(contact_ORCIDs), length(contacts))
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "The user identifier for any contact is not an ORCID."
        }
    }

    # There could be multiple contacts, but just one contact with an email will satisfy this check
    email <- lapply(c(1:length(contacts)), function(i) {length(contacts[[i]]@electronicMailAddress@.Data)})
    if (all(email == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An email address for any contact is not present."
    } else {
        contact_emails <- unlist(EML::eml_get(contacts, "electronicMailAddress"))
        has_email <-  grepl("@", contact_emails)
        if (suppressWarnings(any(has_email))) {
            messages[[length(messages) + 1]] <- sprintf("%d of %d contacts have an email.", length(contact_emails), length(contacts))
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "An email address for any contact is not present."
        }
    }

    # There could be multiple contacts, but just one contact with an address will satisfy this check
    address <- lapply(c(1:length(contacts)), function(i) {length(contacts[[i]]@address@.Data)})
    if (all(address == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An address for any contact is not present."
    } else {
        contact_addresses <- unlist(EML::eml_get(contacts, "deliveryPoint"))
        messages[[length(messages) + 1]] <- sprintf("%d of %d contacts have an address.", length(contact_addresses), length(contacts))
    }

    return(list(status = status,
                output = messages))
}


#' Check if geographic coverage is present with description
#'
#' This function checks if geographic coverage is present with a description.
#'
#' @import EML
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_geographic <- function(input) {
    if (methods::is(input, "eml")) {
        geo <- input@dataset@coverage@geographicCoverage
    } else if (any(c("ListOfgeographicCoverage", "character") %in% class(input))) {
        geo <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfgeographicCoverage', or 'character'.")
    }

    if (length(geo) == 0) {
        return(list(status = "FAILURE",
                    output = "Geographic coverage is not present."))
    }

    if (class(input) == "character") { # For class 'eml', is.character(input) returns TRUE; use alternative method instead
        desc <- input
    } else {
        desc <- EML::eml_get(input, "geographicDescription")
    }

    if (length(desc) != length(geo)) {
        return(list(status = "FAILURE",
                    output = sprintf("A textual description is present for %d of %d geographic coverages.",
                                     length(desc), length(geo))))
    } else {
        return(list(status = "SUCCESS",
                    output = sprintf("A textual description is present for %d of %d geographic coverages.",
                                     length(desc), length(geo))))
    }
}


#' Check if geographic coverage bounding coordinates are present
#'
#' This function checks if geographic coverage bounding coordinates are present.
#'
#' @import EML
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_geographic_coord <- function(input) {
    if (methods::is(input, "eml")) {
        geo <- input@dataset@coverage@geographicCoverage
    } else if (any(c("ListOfgeographicCoverage", "character") %in% class(input))) {
        geo <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfgeographicCoverage', or 'character'.")
    }

    if (length(geo) == 0) {
        return(list(status = "FAILURE",
                    output = "Geographic coverage is not present. Unable to check for bounding coordinates."))
    }

    # Both bounding boxes and single points should have four coordinates
    # Single points have duplicates for north/south and east/west
    if (class(input) == "character") { # For class 'eml', is.character(input) returns TRUE; use alternative method instead
        four_coords <- geo
        if (length(input) == 4) {logicals <- TRUE} else {logicals <- FALSE}
        true_sum <- 1
    } else {
        four_coords <- function(x) {
            coord <- EML::eml_get(x, "westBoundingCoordinate")
            coord <- append(coord, EML::eml_get(x, "eastBoundingCoordinate"))
            coord <- append(coord, EML::eml_get(x, "northBoundingCoordinate"))
            coord <- append(coord, EML::eml_get(x, "southBoundingCoordinate"))
            length(coord) == 4 # TRUE/FALSE
        }

        logicals <- lapply(geo, four_coords)
        true_sum <- sum(unlist(logicals))
    }

    if (any(logicals == FALSE)) {
        return(list(status = "FAILURE",
                    output = sprintf("A complete set of bounding coordinates is not present for %d of %d geographic coverages.",
                                     true_sum, length(geo))))
    } else {
        return(list(status = "SUCCESS",
                    output = sprintf("A complete set of bounding coordinates is present for %d of %d geographic coverages.",
                                     true_sum, length(geo))))
    }
}


#' Check if geographic coverage intersects with Arctic
#'
#' This function checks if geographic coverage intersects with the Arctic.
#'
#' @import EML
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_geographic_arctic <- function(input) {
    if (methods::is(input, "eml")) {
        geo <- input@dataset@coverage@geographicCoverage
    } else if (any(c("ListOfgeographicCoverage", "numeric", "character") %in% class(input))) {
        geo <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfgeographicCoverage', 'numeric', or 'character'.")
    }

    if (length(geo) == 0) {
        return(list(status = "FAILURE",
                    output = "Geographic coverage is not present. Unable to check for bounding coordinates."))
    }

    if (any(c("numeric", "character") %in% class(input))) {
        ncoord <- input
    } else {
        ncoord <- EML::eml_get(input, "northBoundingCoordinate")
    }

    if (any(is.na(suppressWarnings(as.numeric(ncoord))))) {
        return(list(status = "FAILURE",
                    output = "A northern bounding coordinate is not numeric."))
    } else if (any(as.numeric(ncoord) > 90)) {
        return(list(status = "FAILURE",
                    output = "A northern bounding coordinate is out of range. The valid range is 0 to +90."))
    } else {
        if (any(as.numeric(ncoord) >= 45)) {
            return(list(status = "SUCCESS",
                        output = "Geographic coverage is in the Arctic."))
        } else {
            return(list(status = "FAILURE",
                        output = "Geographic coverage is not in the Arctic."))
        }
    }
}


#' Check if temporal coverage is present with correct format
#'
#' This function checks if temporal coverage is present
#' and in the correct format (YYYY or YYYY-MM-DD).
#'
#' @import EML
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_temporal <- function(input) {
    if (methods::is(input, "eml")) {
        temp <- input@dataset@coverage@temporalCoverage
    } else if (any(c("ListOftemporalCoverage", "character") %in% class(input))) {
        temp <- input
    } else {
        stop("Input should be of class 'eml', 'ListOftemporalCoverage', or 'character'.")
    }

    if (length(temp) == 0) {
        return(list(status = "FAILURE",
                    output = "Temporal coverage is not present."))
    } else {
        dates <- EML::eml_get(temp, "calendarDate")
        if (length(dates) == 0) { # no dates
            return(list(status = "FAILURE",
                        output = "Temporal coverage is not present."))
        } else if (length(dates) == 1) { # single date
            return(list(status = "SUCCESS",
                        output = "Temporal coverage is present with one date."))
        } else { # date range, multiple single dates, multiple date ranges, etc.
            return(list(status = "SUCCESS",
                        output = "Temporal coverage is present with multiple dates."))
        }
    }
}


#' Check if taxonomic coverage is present
#'
#' This function checks if taxonomic coverage is present.
#'
#' @import EML
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_taxonomic <- function(input) {
    if (methods::is(input, "eml")) {
        taxo <- input@dataset@coverage@taxonomicCoverage
    } else if (any(c("ListOftaxonomicCoverage", "character") %in% class(input))) {
        taxo <- input
    } else {
        stop("Input should be of class 'eml', 'ListOftaxonomicCoverage', or 'character'.")
    }

    if (length(taxo) == 0) {
        return(list(status = "FAILURE",
                    output = "Taxonomic coverage is not present."))
    } else {
        class <- EML::eml_get(taxo, "taxonomicClassification")
        if (length(class) == 0) { # no taxonomic classifications
            return(list(status = "FAILURE",
                        output = "Taxonomic coverage is not present."))
        } else { # one or more taxonomic classifications
            return(list(status = "SUCCESS",
                        output = sprintf("Taxonomic coverage is present with %d classifications.", length(class))))
        }
    }
}


#' Check if methods are present and complete
#'
#' This function checks if methods are present and complete by checking
#' for method steps and sampling descriptions.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
# TODO: account for more than one sampling step (sampling[[i]])
qa_methods <- function(input) {
    if (methods::is(input, "eml")) {
        methods <- input@dataset@methods
    } else if (methods::is(input, "methods")) {
        methods <- input
    } else {
        stop("Input should be of class 'eml' or 'methods'.")
    }

    # No methods present
    if (length(methods@methodStep) == 0 && length(methods@sampling) == 0) {
        return(list(status = "FAILURE",
                    output = "Methods are not present."))
    }

    # Methods present but not complete
    if ((length(methods@methodStep) == 0 && length(methods@sampling) > 0) ||
        (length(methods@methodStep) > 0 && length(methods@sampling) == 0) ||
        ((length(methods@methodStep) > 0 && length(methods@sampling) > 0) &&
         length(methods@sampling[[1]]@studyExtent@description) == 0 ||
         (length(methods@sampling[[1]]@samplingDescription) == 0 &&
         length(methods@sampling[[1]]@samplingDescription@para) == 0))) {
        status <- "FAILURE"
        # Output messages will be stored in a vector
        messages <- c()

        if (length(methods@methodStep) == 0) {
            messages[[length(messages) + 1]] <- "Methods are present but do not include method steps."
        }

        if (length(methods@sampling) == 0) {
            messages[[length(messages) + 1]] <- "Methods are present but do not include a sampling description."
        } else {
            if (length(methods@sampling[[1]]@studyExtent@description) == 0) {
                messages[[length(messages) + 1]] <- "Methods are present but do not include the sampling area and frequency (studyExtent)."
            }
            # Usually 'samplingDescription' has a 'para' slot but sometimes not
            if (length(methods@sampling[[1]]@samplingDescription) == 0 &&
                length(methods@sampling[[1]]@samplingDescription@para) == 0) {
                messages[[length(messages) + 1]] <- "Methods are present but do not include a description of the sampling procedures (samplingDescription)."
            }
        }

        return(list(status = status,
                    output = messages))
    }

    # All methods present and complete
    if (length(methods@methodStep) > 0 && length(methods@sampling) > 0 &&
        length(methods@sampling[[1]]@studyExtent@description) > 0 &&
        (length(methods@sampling[[1]]@samplingDescription) > 0 ||
         length(methods@sampling[[1]]@samplingDescription@para) > 0)) {
        return(list(status = "SUCCESS",
                    output = "Methods are present, including method steps and a complete sampling description."))
    }
}


#' Check if project information is present and complete
#'
#' This function checks if project information is present and complete.
#' The minimum recommended fields are title, personnel, abstract, and funding.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_project <- function(input) {
    if (methods::is(input, "eml")) {
        project <- input@dataset@project
    } else if (methods::is(input, "project")) {
        project <- input
    } else {
        stop("Input should be of class 'eml' or 'project'.")
    }

    # Check if title exists to determine if project information is present
    if (length(project@title) == 0) {
        return(list(status = "FAILURE",
                    output = "Project information is not present."))
    } else {
        # Minimum recommended fields
        title <- length(project@title)
        personnel <- length(project@personnel)
        abstract <- length(project@abstract@para) # usually 'abstract' has a 'para' slot but sometimes not
        funding <- length(project@funding@para) # usually 'funding' has a 'para' slot but sometimes not

        if (title > 0 && personnel > 0 && abstract > 0 && funding > 0) {
            return(list(status = "SUCCESS",
                        output = "Project information is present and complete."))
        } else {
            return(list(status = "FAILURE",
                        output = paste0("Project information is present but not complete. ",
                                        "The following are missing: ",
                                        if (title == 0) {"'title' "} else {},
                                        if (personnel == 0) {"'personnel' "} else {},
                                        if (abstract == 0) {"'abstract' "} else {},
                                        if (funding == 0) {"'funding' "} else {}, ".")))
        }
    }
}


#' Check if name and description are present for data entities
#'
#' This function checks if a name and description are present for each data entity.
#' Additionally, entity names should be 100 characters or less.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_entity <- function(input) {
    if (methods::is(input, "eml")) {
        entity <- list(input@dataset@dataTable,
                       input@dataset@otherEntity,
                       input@dataset@spatialVector)
    } else if (any(c("ListOfdataTable", "ListOfotherEntity", "ListOfspatialVector") %in% class(input))) {
        entity <- list(input)
    } else if (any(c("dataTable", "otherEntity", "spatialVector") %in% class(input))) {
        entity <- list(list(input))
    } else {
        stop("Input should be of class 'eml', 'ListOfdataTable', 'ListOfotherEntity', 'ListOfspatialVector', 'dataTable', 'otherEntity', or 'spatialVector'.")
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Output messages will be stored in a vector
    messages <- c()

    for (i in seq_along(entity)) { # each data type
        for (j in seq_along(entity[[i]])) { # each entity of a data type
            if (length(entity[[i]]) == 0) {
                next
            } else {
                if (length(entity[[i]][[j]]@entityName@.Data) == 0) {
                    status <- "FAILURE"
                    messages[[length(messages) + 1]] <- paste0("The entity name for ",
                                                               class(entity[[i]][[j]])[[1]],"[[", j, "]]",
                                                               " is missing.")
                } else if (nchar(entity[[i]][[j]]@entityName@.Data) > 100) {
                    status <- "FAILURE"
                    messages[[length(messages) + 1]] <- paste0("The entity name for ",
                                                               class(entity[[i]][[j]])[[1]],"[[", j, "]]",
                                                               " is present but greater than 100 characters.")
                } else if (length(entity[[i]][[j]]@entityDescription@.Data) == 0) {
                    status <- "FAILURE"
                    messages[[length(messages) + 1]] <- paste0("The entity description for ",
                                                               class(entity[[i]][[j]])[[1]],"[[", j, "]]",
                                                               " is missing.")
                } else {
                    messages[[length(messages) + 1]] <- paste0("The entity name and description are present for ",
                                                               class(entity[[i]][[j]])[[1]],"[[", j, "]]", ".")
                }
            }
        }
    }

    return(list(status = status,
                output = messages))
}


#' Check for duplicated data entities
#'
#' This function checks if data entities have been duplicated
#' based on matching PIDs.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_entity_dup <- function(input) {
    if (methods::is(input, "eml")) {
        entity <- list(input@dataset@dataTable,
                       input@dataset@otherEntity,
                       input@dataset@spatialVector)
    } else if (any(c("ListOfdataTable", "ListOfotherEntity", "ListOfspatialVector") %in% class(input))) {
        entity <- list(input)
    } else if (any(c("dataTable", "otherEntity", "spatialVector") %in% class(input))) {
        entity <- list(list(input))
    } else {
        stop("Input should be of class 'eml', 'ListOfdataTable', 'ListOfotherEntity', 'ListOfspatialVector', 'dataTable', 'otherEntity', or 'spatialVector'.")
    }

    pids <- character()
    for (i in seq_along(entity)) { # each data type
        for (j in seq_along(entity[[i]])) { # each entity of a data type
            if (length(entity[[i]]) == 0 || length(entity[[i]][[j]]@id@.Data) == 0) {
                next
            } else {
                pids[[length(pids) + 1]] <- entity[[i]][[j]]@id@.Data
            }
        }
    }

    if (length(pids) != length(unique(pids))) {
        return(list(status = "FAILURE",
                    output = paste0("Data entities have been duplicated based on matching PIDs: ",
                                    paste0(pids[which(duplicated(pids))], collapse = " + "))))
    } else {
        return(list(status = "SUCCESS",
                    output = "No entities are duplicated."))
    }
}


#' Check if physical of data entity is present and complete
#'
#' This function checks the physical of a data entity for presence, completeness, and mismatches.
#'
#' @import EML
#' @importFrom methods is
#' @importFrom stringr str_detect str_subset str_split
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @noRd
qa_physical <- function(input) {
    if (methods::is(input, "eml")) {
        entity <- list(input@dataset@dataTable,
                       input@dataset@otherEntity,
                       input@dataset@spatialVector)
    } else if (any(c("ListOfdataTable", "ListOfotherEntity", "ListOfspatialVector") %in% class(input))) {
        entity <- list(input)
    } else if (any(c("dataTable", "otherEntity", "spatialVector") %in% class(input))) {
        entity <- list(list(input))
    } else {
        stop("Input should be of class 'eml', 'ListOfdataTable', 'ListOfotherEntity', 'ListOfspatialVector', 'dataTable', 'otherEntity', or 'spatialVector'.")
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Output messages will be stored in a vector
    messages <- c()

    for (i in seq_along(entity)) { # each data type
        for (j in seq_along(entity[[i]])) { # each entity of a data type
            if (length(entity[[i]]) == 0) {
                next
            } else {
                if (length(entity[[i]][[j]]@physical@.Data) == 0) {
                    status <- "FAILURE"
                    messages[[length(messages) + 1]] <- paste0("The physical for ",
                                                               class(entity[[i]][[j]])[[1]],"[[", j, "]]",
                                                               " is missing.")
                } else {
                    # Check for presence and completeness of elements in physical
                    result <- character(5)
                    elements <- c("objectName", "size", "authentication", "formatName", "url")
                    for (x in elements) {
                        result[which(elements == x)] <- if (length(EML::eml_get(entity[[i]][[j]], x)) == 1)
                                                                {paste(x, "exists")} else {paste(x, "does not exist")}
                    }
                    if (any(stringr::str_detect(result, "not"))) {
                        status <- "FAILURE"
                        messages[[length(messages) + 1]] <- paste0("The physical for ", class(entity[[i]][[j]])[[1]],"[[", j, "]]",
                                                                   " is present but not complete: ",
                                                                   paste0(stringr::str_subset(result, "not"), collapse = " + "), ".")
                    } else {
                        messages[[length(messages) + 1]] <- paste0("The physical for ", class(entity[[i]][[j]])[[1]],"[[", j, "]]",
                                                                   " is present and complete.")
                    }

                    # Check if entityName matches objectName
                    if (entity[[i]][[j]]@entityName@.Data != entity[[i]][[j]]@physical[[1]]@objectName@.Data) {
                        status <- "FAILURE"
                        messages[[length(messages) + 1]] <- paste0("The names for ",
                                                                   class(entity[[i]][[j]])[[1]],"[[", j, "]]",
                                                                   " do not match.")
                    } else {
                        messages[[length(messages) + 1]] <- paste0("The names for ",
                                                                   class(entity[[i]][[j]])[[1]],"[[", j, "]]",
                                                                   " match.")
                    }

                    # Check if entity PID matches PID in URL in physical
                    if (!stringr::str_detect(entity[[i]][[j]]@physical[[1]]@distribution[[1]]@online@url@.Data, "urn") ||
                        length(entity[[i]][[j]]@id@.Data) == 0) {
                        messages[[length(messages) + 1]] <- paste0("Unable to check if entity PID and URL PID match for ",
                                                                   class(entity[[i]][[j]])[[1]],"[[", j, "]]", ".")
                        next
                    } else {
                        url_pid <- stringr::str_split(entity[[i]][[j]]@physical[[1]]@distribution[[1]]@online@url@.Data, "(?=urn.)", simplify = TRUE)[[2]]
                        if (entity[[i]][[j]]@id@.Data != url_pid) {
                            status <- "FAILURE"
                            messages[[length(messages) + 1]] <- paste0("The entity PID and URL PID for ",
                                                                       class(entity[[i]][[j]])[[1]],"[[", j, "]]",
                                                                       " do not match.")
                        } else {
                            messages[[length(messages) + 1]] <- paste0("The entity PID and URL PID for ",
                                                                       class(entity[[i]][[j]])[[1]],"[[", j, "]]",
                                                                       " match.")
                        }
                    }
                }
            }
        }
    }

    return(list(status = status,
                output = messages))
}


#' Quality assurance for EML metadata
#'
#' This function checks EML fields for presence, completeness, correct formatting, etc.
#'
#' @import EML
#' @importFrom methods is
#' @importFrom stringr str_detect
#'
#' @param input (eml) An EML object.
#' @param all_results (logical) Return all results. If FALSE, only returns results with FAILURE status.
#'
#' @return (list) A list of results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pkg <- get_package(adc_test, rm_pid, file_names = TRUE)
#' eml <- read_eml(getObject(adc_test, pkg$metadata))
#' results <- qa_eml(eml) }
qa_eml <- function(input, all_results = FALSE) {
    if (!methods::is(input, "eml")) {
        stop("Input should be of class 'eml'.")
    }

    val <- EML::eml_validate(input)
    val_results <- list(status = if (length(attr(val, "errors")) == 0) {"SUCCESS"} else {"FAILURE"},
                        output = attr(val, "errors"))

    # Use tryCatch to return ERROR status if error is encountered
    err <- function(e) {list(status = "ERROR")}

    results <- list("qa_title" = tryCatch(qa_title(input), error = err),
                    "qa_pubDate" = tryCatch(qa_pubDate(input), error = err),
                    "qa_abstract" = tryCatch(qa_abstract(input), error = err),
                    "qa_keywordSet" = tryCatch(qa_keywordSet(input), error = err),
                    "qa_intellectualRights" = tryCatch(qa_intellectualRights(input), error = err),
                    "qa_creator" = tryCatch(qa_creator(input), error = err),
                    "qa_creator_info" = tryCatch(qa_creator_info(input), error = err),
                    "qa_contact" = tryCatch(qa_contact(input), error = err),
                    "qa_contact_info" = tryCatch(qa_contact_info(input), error = err),
                    "qa_geographic" = tryCatch(qa_geographic(input), error = err),
                    "qa_geographic_coord" = tryCatch(qa_geographic_coord(input), error = err),
                    "qa_geographic_arctic" = tryCatch(qa_geographic_arctic(input), error = err),
                    "qa_temporal" = tryCatch(qa_temporal(input), error = err),
                    "qa_taxonomic" = tryCatch(qa_taxonomic(input), error = err),
                    "qa_methods" = tryCatch(qa_methods(input), error = err),
                    "qa_project" = tryCatch(qa_project(input), error = err),
                    "qa_entity" = tryCatch(qa_entity(input), error = err),
                    "qa_entity_dup" = tryCatch(qa_entity_dup(input), error = err),
                    "qa_physical" = tryCatch(qa_physical(input), error = err),
                    "eml_validate" = val_results)

    # Default to only return results with FAILURE or ERROR status
    if (all_results) {
        return(results)
    } else {
        results <- Filter(function(x) {stringr::str_detect(x$status, "FAILURE|ERROR")}, results)
        if (length(results) > 0) {
            return(results)
        } else {
            return(list("qa" = list(status = "SUCCESS",
                                    output = "All QA functions were successful.")))
        }
    }
}
