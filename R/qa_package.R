#' Check package including congruence of attributes and data
#'
#' This function checks that the attributes listed in the metadata match the values in the data for each
#' tabular data object. It may also optionally check if all creators have ORCIDs and have full access
#' to all elements of the data package.
#'
#' @importFrom methods is
#' @importFrom methods slot
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
#' @param mn (MNode) The Member Node to query.
#' @param resource_map_pid (character) The PID of a resource map.
#' @param read_all_data (logical) Default TRUE. Read all data from remote and check that column types match attributes,
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
#' qa_package(mn, pid, read_all_data = FALSE, check_attributes = TRUE,
#'            check_creators = FALSE, check_access = FALSE)
#' }
qa_package <- function(mn, resource_map_pid, read_all_data = TRUE, check_attributes = TRUE, check_creators = FALSE, check_access = FALSE) {
    stopifnot(class(mn) %in% c("MNode", "CNode"))
    stopifnot(is.character(resource_map_pid), nchar(resource_map_pid) > 0)
    stopifnot(is.logical(read_all_data))
    stopifnot(is.logical(check_attributes))
    stopifnot(is.logical(check_creators))
    stopifnot(is.logical(check_access))

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
        suppressWarnings(arcticdatautils::get_package(mn, resource_map_pid, file_names = TRUE))
    },
    error = function(e) {
        stop("\nFailed to get package. Is your DataONE token set?")
    })

    eml <- EML::read_eml(rawToChar(dataone::getObject(mn, package$metadata)))

    cat(crayon::green(paste0("\n.....Processing package ", package$resource_map, "...............")))

    # Check creators
    if (check_creators || check_access) {
        creator_ORCIDs <- qa_creator_ORCIDs(eml)
    }

    # Check access
    if (check_access && length(creator_ORCIDs) > 0) {
        # Check metadata
        sysmeta <- dataone::getSystemMetadata(mn, package$metadata)
        qa_access(sysmeta, creator_ORCIDs)
        # Check resource_map
        sysmeta <- dataone::getSystemMetadata(mn, package$resource_map)
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
            cat(crayon::red(paste0("\nThe URL/distribution for ", datapid, " is missing/incongruent in the physical section of the EML.\n")))
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

        sysmeta <- dataone::getSystemMetadata(mn, objectpid)

        if (check_access && length(creator_ORCIDs) > 0) {
            qa_access(sysmeta, creator_ORCIDs)
        }

        if (!check_attributes) next

        # If object is not tabular data, skip to next object
        format <- sysmeta@formatId
        if (!format %in% supported_file_formats) next

        cat(crayon::green(paste0("\n\n..........Processing object ", objectpid,
                                 " (", dataTable@physical[[1]]@objectName, ")...............")))

        if (is.null(EML::get_attributes(dataTable@attributeList)$attributes) && length(slot(dataTable@attributeList, 'references')) == 0) {
            cat(crayon::red(paste0("\nEmpty attribute table for object at ", dataTable@physical[[1]]@distribution[[1]]@online@url)))
            cat(crayon::green(paste0("\n..........Processing complete for object ", objectpid,
                                     " (", dataTable@physical[[1]]@objectName, ")...............")))
            next
        }

        # If package is public, read directly from the file; otherwise, use DataONE API
        isPublic <- datapack::hasAccessRule(sysmeta, "public", "read")

        if (read_all_data) {
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
                        cat(crayon::green(paste0("\n..........Processing complete for object ", objectpid,
                                                 " (", dataTable@physical[[1]]@objectName, ")...............")))
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
                    cat(crayon::green(paste0("\n..........Processing complete for object ", objectpid,
                                             " (", dataTable@physical[[1]]@objectName, ")...............")))
                    next
                } else if (format == "netCDF-4" || format == "netCDF-3" || format == "CF-1.4" || format == "CF-1.3" ||
                           format == "CF-1.2" || format == "CF-1.1" || format == "CF-1.0") {
                    tmp <- tempfile()
                    writeBin(dataone::getObject(mn, objectpid), tmp)
                    nc <- ncdf4::nc_open(tmp)
                    data <- netcdf_to_dataframe(nc)
                    unlink(tmp)
                    rm(nc) # clean up now because many netCDF files are large
                    data
                } else {
                    utils::read.csv(textConnection(rawToChar(dataone::getObject(mn, objectpid))),
                                    nrows = rowsToRead, check.names = FALSE, stringsAsFactors = FALSE)
                }
            }
        },
        error = function(e) {
            stop(paste0("\nFailed to read file at ", urls[i]))
        })

        qa_attributes(eml, dataTable, data, read_all_data)

        cat(crayon::green(paste0("\n..........Processing complete for object ", objectpid,
                                 " (", dataTable@physical[[1]]@objectName, ")...............")))
    }

    cat(crayon::green(paste0("\n\n.....Processing complete for package ",
                             package$resource_map, "...............")))

    return(invisible())
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
#' @param eml (S4) The entire EML object.  This is necessary if attributes with references are being checked.
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
qa_attributes <- function(eml, dataTable, data, checkEnumeratedDomains = TRUE) {
    attributeTable <- EML::get_attributes(dataTable@attributeList)
    # Check for references
    if (is.null(attributeTable$attributes)) {
        ref_index <- match_reference_to_attributeList(eml, dataTable)
        if (length(ref_index) > 0) {
            entity <- methods::slot(eml@dataset, class(dataTable))[[ref_index]]
            attributeTable <- EML::get_attributes(entity@attributeList)
        }
    }
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
    # remove duplicates
    dup_indices <- which(duplicated(tolower(var_names)))
    if (length(dup_indices) > 0) {
        var_names <- var_names[-dup_indices]
    }

    data <- lapply(var_names, function(x) ncdf4::ncvar_get(nc, x))
    max_length <- max(unlist(lapply(data, function(x) length(x))))

    results <- data.frame(matrix(ncol = length(data), nrow = max_length))
    names(results) <- var_names
    for (i in seq_along(results)) {
        results[,i] <- rep_len(data[[i]], length.out = max_length)
    }

    return(results)
}

# Helper function for matching a reference to an attributeList
# Returns the index of the match
match_reference_to_attributeList <- function(eml, entity) {
    # Get list of 'dataTable', 'otherEntity', etc.
    entity_list <- methods::slot(eml@dataset, class(entity))
    # Get the ref we want to match
    ref <- methods::slot(entity@attributeList, 'references')
    # Get all of the references present
    references <- entity_list %>%
        purrr::map(methods::slot, 'attributeList') %>%
        purrr::map(methods::slot, 'id') %>%  # two lists - so we need two map() calls
        unlist()
    # Get the index of the reference we want - use regex anchors to specify start and end of string
    index <- which(stringr::str_detect(references, paste0('^', ref, '$')))
    return(index)
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
