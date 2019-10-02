#' Check package including congruence of attributes and data
#'
#' This function checks that the attributes listed in the metadata match the values in the data for each
#' tabular data object. It may also optionally check if all creators have ORCIDs and have full access
#' to all elements of the data package.
#'
#' @param mn (MNode) The Member Node to query.
#' @param resource_map_pid (character) The PID for a resource map.
#' @param read_all_data (logical) Read all data from remote and check that column types match attributes. If `FALSE`,
#'   only read first 10 rows. Only applicable to public packages (private packages will read complete dataset).
#'   If `check_attributes = FALSE`, no rows will be read.
#' @param check_attributes (logical) Check congruence of attributes and data.
#' @param check_creators (logical) Check if each creator has an ORCID. Will also run if `check_access = TRUE`.
#' @param check_access (logical) Check if each creator has full access to the metadata, resource map, and data objects.
#'   Will not run if the checks associated with `check_creators` fail.
#'
#' @return `NULL`
#'
#' @import arcticdatautils
#' @import dataone
#' @import EML
#' @importFrom crayon green red yellow
#' @importFrom datapack hasAccessRule
#' @importFrom methods is slot
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom readxl read_excel
#' @importFrom sf read_sf st_set_geometry
#' @importFrom utils read.csv read.delim read.table download.file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run all QA checks
#'
#' qa_package(mn, pid, read_all_data = TRUE, check_attributes = TRUE,
#'            check_creators = TRUE, check_access = TRUE)
#' }
qa_package <- function(mn, resource_map_pid, read_all_data = TRUE, check_attributes = TRUE, check_creators = FALSE, check_access = FALSE) {
    stopifnot(class(mn) %in% c("MNode", "CNode"))
    stopifnot(is.character(resource_map_pid), nchar(resource_map_pid) > 0)
    stopifnot(is.logical(read_all_data))
    stopifnot(is.logical(check_attributes))
    stopifnot(is.logical(check_creators))
    stopifnot(is.logical(check_access))

    package <- tryCatch(suppressWarnings(arcticdatautils::get_package(mn, resource_map_pid, file_names = TRUE)),
                        error = function(e) stop("\nFailed to get package. Is the Member Node correct? Is your DataONE token set?"))

    cat(crayon::green(paste0("\n.....Processing package ", package$resource_map, "...............")))

    doc <- EML::read_eml(dataone::getObject(mn, package$metadata))

    # Check creators
    if (check_creators || check_access) {
        creator_ORCIDs <- qa_creator_ORCIDs(doc)
    }

    # Check access
    if (check_access && length(creator_ORCIDs) > 0) {
        # Check metadata
        sysmeta <- dataone::getSystemMetadata(mn, package$metadata)
        qa_access(sysmeta, creator_ORCIDs)
        # Check resource_map
        sysmeta <- dataone::getSystemMetadata(mn, package$resource_map)
        qa_access(sysmeta, creator_ORCIDs)
        # Check data objects
        for (object in package$data) {
            sysmeta <- dataone::getSystemMetadata(mn, object)
            qa_access(sysmeta, creator_ORCIDs)
        }
    }

    if (!is.null(names(doc$dataset$dataTable)) & length(doc$dataset$dataTable) > 0) {
        doc$dataset$dataTable <- list(doc$dataset$dataTable)
    }
    if (!is.null(names(doc$dataset$otherEntity)) & length(doc$dataset$otherEntity) > 0) {
        doc$dataset$otherEntity <- list(doc$dataset$otherEntity)
    }
    if (!is.null(names(doc$dataset$spatialVector)) & length(doc$dataset$spatialVector) > 0) {
        doc$dataset$spatialVector <- list(doc$dataset$spatialVector)
    }

    if (is.null(eml_get_simple(doc$dataset, "dataTable"))) doc$dataset$dataTable <- list()
    if (is.null(eml_get_simple(doc$dataset, "otherEntity"))) doc$dataset$otherEntity <- list()
    if (is.null(eml_get_simple(doc$dataset, "spatialVector"))) doc$dataset$spatialVector <- list()


    eml_objects <- c(doc$dataset$dataTable, doc$dataset$otherEntity, doc$dataset$spatialVector)

    if (length(eml_objects) == 0) {
        cat(crayon::red("\nNo data objects of a supported format were found in the EML."))
        cat(crayon::green(paste0("\n\n.....Processing complete for package ",
                                 package$resource_map, "...............")))
        return()
    }

    # Preserve order of getting data objects based on data type for correct name assignment
    # Entity names may not match data object names, so use objectName to ensure matches with data names
    names(eml_objects) <- c(arcticdatautils::eml_get_simple(doc$dataset$dataTable, "objectName"),
                                   arcticdatautils::eml_get_simple(doc$dataset$otherEntity, "objectName"),
                                   arcticdatautils::eml_get_simple(doc$dataset$spatialVector, "objectName"))
    # If object names are missing, use entity names instead
    if (is.null(names(eml_objects)) || any(is.na(names(eml_objects)))) {
        names(eml_objects) <-c(arcticdatautils::eml_get_simple(doc$dataset$dataTable, "entityName"),
                                       arcticdatautils::eml_get_simple(doc$dataset$otherEntity, "entityName"),
                                       arcticdatautils::eml_get_simple(doc$dataset$spatialVector, "entityName"))
    }

    data_objects <- dl_and_read_all_data(mn, package, doc, read_all_data)

    # If missing fileName, assign name to data objects
    for (i in seq_along(data_objects)) {
        if (is.na(names(data_objects)[[i]])) {
            id <- package$data[[i]]
            urls <- unique(arcticdatautils::eml_get_simple(eml_objects, "url"), recursive = TRUE) %>%
                grep("http", ., value = T)
            j <- which(stringr::str_detect(urls, id))
            names(data_objects)[[i]] <-
                if (!is.na(EML::eml_get(eml_objects[[j]], "objectName"))) {
                    EML::eml_get(eml_objects[[j]], "objectName")
                } else {
                    EML::eml_get(eml_objects[[j]], "entityName")
                }
        }
    }
    # what does this even do? this seems wrong
    # if (length(eml_objects) != length(data_objects)) {
    #     cat(crayon::red("\nThe number of downloaded data objects does not match the number of EML data objects."))
    #     cat(crayon::green(paste0("\n\n.....Processing complete for package ",
    #                              package$resource_map, "...............")))
    #     return()
    # }

    #eml_objects <- list(doc$dataset$dataTable, doc$dataset$otherEntity, doc$dataset$spatialVector)

    # Filter out data objects that have SKIP status
    data_objects <- Filter(function(x) suppressWarnings(length(x$status) == 0) || suppressWarnings(x$status != "SKIP"), data_objects)
    eml_objects <- Filter(function(x) EML::eml_get(x, "objectName") %in% names(data_objects) ||
                              EML::eml_get(x, "entityName") %in% names(data_objects), eml_objects)

    # Index objects in parallel based on names (in ascending order) for correct processing in iterations
    eml_objects <- eml_objects[order(names(eml_objects))]
    data_objects <- data_objects[order(names(data_objects))]

    if (check_attributes) mapply(qa_attributes, eml_objects, data_objects, MoreArgs = list(doc = doc))

    cat(crayon::green(paste0("\n\n.....Processing complete for package ",
                             package$resource_map, "...............")))
}


# Helper function for downloading and reading all data objects in a data package
dl_and_read_all_data <- function(mn, package, doc, read_all_data) {
    stopifnot(class(mn) %in% c("MNode", "CNode"))
    stopifnot(is.list(package), length(package) > 0)
    stopifnot(methods::is(doc, "emld"))
    stopifnot(is.logical(read_all_data))

    urls <- unique(arcticdatautils::eml_get_simple(doc$dataset, "url"), recursive = TRUE) %>%
        grep("http", ., value = T)

    # Check that each data object has a matching URL in the EML
    wrong_URL <- FALSE
    for (datapid in package$data) {
        n <- which(grepl(paste0(datapid, "$"), urls))
        if (length(n) != 1) {
            cat(crayon::red(paste("\nThe distribution URL for object", datapid, "is missing or incongruent in the physical section of the EML.\n")))
            wrong_URL <- TRUE
        }
    }

    if (length(urls) != length(package$data) || wrong_URL) {
        # Stop here to ensure proper ordering in the following iterations
        stop("\nAll distribution URLs for data objects must match the data PIDs to continue.")
    }

    if (read_all_data) {
        rows_to_read <- -1
    } else {
        rows_to_read <- 10
    }

    objects <- lapply(package$data, dl_and_read_data, doc, mn, rows_to_read)

    return(objects)
}


# Helper function for downloading and reading a data object
dl_and_read_data <- function(objectpid, doc, mn, rows_to_read) {
    supported_file_formats <- c("text/csv",
                                "text/tsv",
                                "text/plain",
                                "application/vnd.ms-excel",
                                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                "application/zip",
                                "netCDF-4",
                                "netCDF-3",
                                "CF-1.4", "CF-1.3", "CF-1.2", "CF-1.1", "CF-1.0")

    if (!is.null(names(doc$dataset$dataTable)) & length(doc$dataset$dataTable) > 0) {
        doc$dataset$dataTable <- list(doc$dataset$dataTable)
    }
    if (!is.null(names(doc$dataset$otherEntity)) & length(doc$dataset$otherEntity) > 0) {
        doc$dataset$otherEntity <- list(doc$dataset$otherEntity)
    }
    if (!is.null(names(doc$dataset$spatialVector)) & length(doc$dataset$spatialVector) > 0) {
        doc$dataset$spatialVector <- list(doc$dataset$spatialVector)
    }

    if (!is.null(doc$dataset$dataTable)){
        urls_dataTable <- unique(arcticdatautils::eml_get_simple(doc$dataset$dataTable, "url"), recursive = TRUE) %>%
            grep("http", ., value = T)
        n_dT <- which(grepl(paste0(objectpid, "$"), urls_dataTable))
    } else n_dt <- list()
    if (!is.null(doc$dataset$otherEntity)){
        urls_otherEntity <- unique(arcticdatautils::eml_get_simple(doc$dataset$otherEntity, "url"), recursive = TRUE) %>%
            grep("http", ., value = T)
        n_oE <- which(grepl(paste0(objectpid, "$"), urls_otherEntity))
    } else n_oE <- list()
    if (!is.null(doc$dataset$spatialVector)){
        urls_spatialVector <- unique(arcticdatautils::eml_get_simple(doc$dataset$spatialVector, "url"), recursive = TRUE) %>%
            grep("http", ., value = T)
        n_sV <- which(grepl(paste0(objectpid, "$"), urls_spatialVector))
    } else n_sV <- list()


    if (length(n_dT) == 1) {
        entity <- doc$dataset$dataTable[[n_dT]]
        urls <- urls_dataTable
        i <- n_dT
    } else if (length(n_oE) == 1) {
        entity <- doc$dataset$otherEntity[[n_oE]]
        urls <- urls_otherEntity
        i <- n_oE
    } else if (length(n_sV) == 1) {
        entity <- doc$dataset$spatialVector[[n_sV]]
        urls <- urls_spatialVector
        i <- n_sV
    } else {
        cat(crayon::yellow("\nData object is not tabular or not a supported format. Skipped."))
        cat(crayon::green(paste0("\n..........Download complete for object ", objectpid,
                                 " (", entity$physical$objectName, ")...............")))
        return(list(status = "SKIP"))
    }

    cat(crayon::green(paste0("\n\n..........Downloading object ", objectpid,
                             " (", entity$physical$objectName, ")...............")))

    # If object is not tabular data, skip to next object
    format <- arcticdatautils::eml_get_simple(entity, "formatName")
    if (length(format) == 0) {
        cat(crayon::red("\nData object has no given format ID in EML. Unable to check if supported format. Skipped"))
        cat(crayon::green("\n..........Object not downloaded.............................."))
        return(list(status = "SKIP"))
    } else if (!(format %in% supported_file_formats)) {
        cat(crayon::red("\nData object is not tabular or not a supported format. Skipped."))
        cat(crayon::green("\n..........Object not downloaded.............................."))
        return(list(status = "SKIP"))
    }

    if (is.null(EML::get_attributes(entity$attributeList)$attributes) && length(slot(entity$attributeList, 'references')) == 0) {
        cat(crayon::red(paste0("\nEmpty attribute table for data object. Skipped.")))
        cat(crayon::green("\n..........Object not downloaded.............................."))
        return(list(status = "SKIP"))
    }

    # If package is public, read directly from the file; otherwise, use DataONE API
    sysmeta <- dataone::getSystemMetadata(mn, objectpid)
    isPublic <- datapack::hasAccessRule(sysmeta, "public", "read")

    tryCatch({
        if (isPublic) {
            if (format == "text/csv") {
                data <- utils::read.csv(urls[i], nrows = rows_to_read, check.names = FALSE, stringsAsFactors = FALSE)
            } else if (format == "text/tsv") {
                data <- utils::read.delim(urls[i], nrows = rows_to_read)
            } else if (format == "text/plain") {
                data <- utils::read.table(urls[i], nrows = rows_to_read)
            } else if (format == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" || format == "application/vnd.ms-excel") {
                tmp <- tempfile()
                utils::download.file(url = urls[i], destfile = tmp, mode = "wb", quiet = TRUE)
                data <- readxl::read_excel(tmp, n_max = if (rows_to_read == -1) Inf else rows_to_read)
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
                    cat(crayon::yellow("\nNote: Shapefiles have attribute name limits of 10 characters."))
                    data <- suppressWarnings(sf::read_sf(t[grep("*\\.shp", t)]) %>% sf::st_set_geometry(NULL))
                } else if (any(grep("*\\.gdb", t))) {
                    data <- suppressWarnings(sf::read_sf(list.dirs(tmp2)[2]) %>% sf::st_set_geometry(NULL))
                } else {
                    cat(crayon::red("\nSpatial data not present within .zip file. Skipped."))
                    cat(crayon::green("\n..........Object not downloaded.............................."))
                    unlink(c(tmp, tmp2), recursive = TRUE)
                    return(list(status = "SKIP"))
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
                cat(crayon::red("\nThis function uses the DataONE API to read private data objects and currently cannot read .xls or .xlsx files.\nSkipped. Check attributes manually."))
                cat(crayon::green("\n..........Object not downloaded.............................."))
                return(list(status = "SKIP"))
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
                data <- utils::read.csv(textConnection(rawToChar(dataone::getObject(mn, objectpid))),
                                        nrows = rows_to_read, check.names = FALSE, stringsAsFactors = FALSE)
            }
        }

        cat(crayon::green("\n..........Download complete.............................."))
        return(data)
    },
    error = function(e) {
        cat(crayon::red(paste0("\nFailed to read file at ", urls[i], ". Skipped.")))
        cat(crayon::green("\n..........Object not downloaded.............................."))
        return(list(status = "SKIP"))
    })
}


# Helper function for converting 2D data from a netCDF to a data.frame object
netcdf_to_dataframe <- function(nc) {
    att_names <- names(nc$var)
    dims <- nc$dim
    dim_names <- c()
    for (i in seq_along(dims)) {
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
        results[ , i] <- rep_len(data[[i]], length.out = max_length)
    }

    return(results)
}


#' Check congruence of data and metadata attributes for a tabular data object
#'
#' This function checks the congruence of data and metadata attributes
#' for a tabular data object. Supported objects include `dataTable`, `otherEntity`,
#' and `spatialVector` entities. It can be used on its own but is also
#' called by [qa_package()] to check all tabular data objects in a data package.
#'
#' This function checks the following:
#' * Names: Check that column names in attributes match column names in data frame. Possible conditions to check for:
#'     * attributeList does not exist for data frame
#'     * Some of the attributes that exist in the data do not exist in the attributeList
#'     * Some of the attributes that exist in the attributeList do not exist in the data
#'     * Typos in attribute or column names resulting in nonmatches
#' * Domains: Check that attribute types in EML match attribute types in data frame. Possible conditions to check for:
#'     * nominal, ordinal, integer, ratio, dateTime
#'     * If domain is enumerated domain, enumerated values in the data are accounted for in the enumerated definition
#'     * If domain is enumerated domain, enumerated values in the enumerated definition are all represented in the data
#'     * Type of data does not match attribute type
#' * Values: Check that values in data are reasonable. Possible conditions to check for:
#'     * Accidental characters in the data (e.g., one character in a column of integers)
#'     * If missing values are present, missing value codes are also present
#'
#' @param entity (emld) An EML `dataTable`, `otherEntity`, or `spatialVector` associated with the data object.
#' @param data (data.frame) A data frame of the data object.
#' @param doc (emld) The entire EML object. This is necessary if attributes with references are being checked.
#'
#' @return `NULL`
#'
#' @import arcticdatautils
#' @import EML
#' @importFrom crayon green red yellow
#' @importFrom lubridate parse_date_time
#' @importFrom methods is slot
#' @importFrom stats na.omit
#' @importFrom stringr str_split
#' @importFrom utils capture.output head
#'
#' @export
#'
#' @seealso [qa_package()]
#'
#' @examples
#' \dontrun{
#' # Checking a .csv file
#' dataTable <- doc$dataset$dataTable[[1]]
#' data <- readr::read_csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:...")
#'
#' qa_attributes(dataTable, data)
#' }
qa_attributes <- function(entity, data, doc = NULL) {
    stopifnot(is.data.frame(data))
    if (!is.null(doc) && !methods::is(doc, "emld")) {
        stop("Input should be of class 'emld'.")
    }

    objectpid <- stringr::str_split(entity$physical$distribution$online$url$url, "(?=urn.)", simplify = TRUE)[[2]]

    cat(crayon::green(paste0("\n\n..........Processing object ", objectpid,
                             " (", entity$physical$objectName, ")...............")))

    entity_list <- doc$dataset[names(doc$dataset) %in% c("dataTable", "otherEntity", "spatialVector")]
    names(entity_list) <- 'entity'

    tryCatch({
        suppressWarnings(attributeTable <- EML::get_attributes(entity$attributeList))
        # Check for references
        if (nrow(attributeTable$attributes) == 0) {
            ref_index <- match_reference_to_attributeList(doc, entity)
            if (length(ref_index) > 0) {
                entity2 <- entity_list$entity[[ref_index]]
                attributeTable <- EML::get_attributes(entity2$attributeList)
            }
        }
        attributeNames <- attributeTable$attributes$attributeName

        # Check if attributes are present
        if (is.null(attributeNames)) {
            cat(crayon::red(paste("\nEmpty attribute table for object at", entity$physical$distribution$online$url)))
        }

        # Check for duplicated attributes based on names
        if (any(duplicated(attributeNames))) {
            cat(crayon::red(paste("\nThere are duplicated attribute names in the EML.")))
        }

        header <- as.numeric(entity$physical$dataFormat$textFormat$numHeaderLines)

        if (length(header) > 0 && !is.na(header) && header > 1) {
            names(data) <- NULL
            names(data) <- data[(header - 1), ]
        }

        data_cols <- colnames(data)

        # Check for attribute correctness according to the EML schema
        att_output <- utils::capture.output(arcticdatautils::eml_validate_attributes(entity$attributeList))
        att_errors <- which(grepl("FALSE", utils::head(att_output, length(attributeNames))))

        if (length(att_errors) > 0) {
            print(att_output[att_errors])
        }

        # Check that attribute names match column names
        allequal <- isTRUE(all.equal(data_cols, attributeNames))

        if (!allequal) {
            intersection <- intersect(attributeNames, data_cols)
            nonmatcheml <- attributeNames[!attributeNames %in% intersection]
            nonmatchdata <- data_cols[!data_cols %in% intersection]

            # EML has values that data does not have
            if (length(nonmatcheml) > 0) {
                cat(crayon::red(paste0("\nThe EML includes attributes '", toString(nonmatcheml, sep = ", "), "' that are not present in the data.")))
                cat(crayon::yellow("\nContinuing attribute and data matching without mismatched attributes - fix issues and re-run after first round completion."))
            }

            # Data has values that EML does not have
            if (length(nonmatchdata) > 0) {
                cat(crayon::red(paste0("\nThe data includes attributes '", toString(nonmatchdata, sep = ", "), "' that are not present in the EML.")))
                cat(crayon::yellow("\nContinuing attribute and data matching without mismatched attributes - fix issues and re-run after first round completion."))
            }

            # Values match but are not ordered correctly
            if (length(nonmatcheml) == 0 && length(nonmatchdata) == 0 && allequal == FALSE) {
                cat(crayon::yellow("\nAttribute names match column names but are incorrectly ordered."))
            }

            data <- data[ , which(colnames(data) %in% intersection)]
            attributeTable$attributes <- attributeTable$attributes[which(attributeTable$attributes$attributeName %in% intersection), ]
            attributeTable$attributes <- attributeTable$attributes[order(match(attributeTable$attributes$attributeName, colnames(data))), ]
        }

        # Check that type of column matches type of data based on acceptable DataONE formats
        for (i in seq_along(data)) {
            matchingAtt <- attributeTable$attributes[i, ]
            attClass <- class(data[ , i])
            # If matchingAtt has a dateTime domain, coerce the column based on the date/time format
            if (matchingAtt$measurementScale == "dateTime") {
                attClass <- class(suppressWarnings(lubridate::parse_date_time(data[ , i], orders = c("ymd", "HMS", "ymd HMS", "y", "m", "d", "ym", "md", "m/d/y",
                                                                                                     "d/m/y", "ymd HM", "yq",  "j", "H", "M", "S", "MS", "HM", "I",
                                                                                                     "a", "A", "U", "w", "W"))))
            }

            if (attClass == "numeric" || attClass == "integer" || attClass == "double") {
                if (matchingAtt$measurementScale != "ratio" && matchingAtt$measurementScale != "interval" && matchingAtt$measurementScale != "dateTime") {
                    cat(crayon::yellow(paste0("\nMismatch in attribute type for the attribute '", matchingAtt$attributeName,
                                              "'. Type of data is ", attClass, " which should probably have interval or ratio measurementScale in EML, not ",
                                              matchingAtt$measurementScale, ".")))
                }
            } else if (attClass == "character" || attClass == "logical") {
                if (matchingAtt$measurementScale != "nominal" && matchingAtt$measurementScale != "ordinal") {
                    cat(crayon::yellow(paste0("\nMismatch in attribute type for the attribute '", matchingAtt$attributeName,
                                              "'. Type of data is ", attClass, " which should probably have nominal or ordinal measurementScale in EML, not ",
                                              matchingAtt$measurementScale, ".")))
                }
            } else if (any(attClass %in% c("POSIXct", "POSIXt", "Date", "Period"))) {
                if (matchingAtt$measurementScale != "dateTime") {
                    cat(crayon::yellow(paste0("\nMismatch in attribute type for the attribute '", matchingAtt$attributeName,
                                              "'. Type of data is ", attClass, " which should probably have dateTime measurementScale in EML, not ",
                                              matchingAtt$measurementScale, ".")))
                }
            }
        }

        # Check that enumerated domains match values in data
        if (length(attributeTable$factors) > 0) {
            for (i in seq_along(unique(attributeTable$factors$attributeName))) {
                emlAttName <- unique(attributeTable$factors$attributeName)[i]
                emlUniqueValues <- attributeTable$factors[attributeTable$factors$attributeName == emlAttName, "code"]

                dataUniqueValues <- unique(stats::na.omit(data[[which(colnames(data) == emlAttName)]])) # omit NAs in unique values

                intersection <- intersect(dataUniqueValues, emlUniqueValues)
                nonmatcheml <- emlUniqueValues[!emlUniqueValues %in% intersection]
                nonmatchdata <- dataUniqueValues[!dataUniqueValues %in% intersection]

                if (length(nonmatcheml) > 0) {
                    cat(crayon::yellow(paste0("\nThe EML contains the following enumerated domain values for the attribute '",
                                              as.character(emlAttName), "' that do not appear in the data: ", toString(nonmatcheml, sep = ", "))))
                }

                if (length(nonmatchdata) > 0) {
                    cat(crayon::yellow(paste0("\nThe data contains the following enumerated domain values for the attribute '",
                                              as.character(emlAttName), "' that do not appear in the EML: ", toString(nonmatchdata, sep = ", "))))
                }
            }
        }

        # If there are any missing values in the data, check that there is an associated missing value code in the EML
        for (i in which(colSums(is.na(data)) > 0)) { # only checks for NA values but others like -99 or -999 could be present
            attribute <- attributeTable$attributes[i, ]
            if (is.na(attribute$missingValueCode)) {
                cat(crayon::red(paste0("\nThe attribute '", attribute$attributeName, "' contains missing values but does not have a missing value code.")))
            }
        }
    },
    error = function(e) cat(crayon::red("\nError. Processing for object stopped."))
    )

    cat(crayon::green("\n..........Processing complete.............................."))
}


# Helper function for matching a reference to an attributeList
# Returns the index of the match
match_reference_to_attributeList <- function(doc, entity) {
    # Get list of 'dataTable', 'otherEntity', etc.
    entity_list <- doc$dataset[names(doc$dataset) %in% c("dataTable", "otherEntity", "spatialVector")]
    names(entity_list) <- 'entity'
    # Get the ref we want to match
    ref <- eml_get_simple(entity, "references")
    # Get all of the references present
    att_lists <- eml_get(entity_list, "attributeList")
    # Get the index of the reference we want
    ids <- lapply(att_lists, eml_get_simple, "id")
    ids <- ids[!names(ids) == "@context"]
    ids <- lapply(ids, function(x){if (length(x > 1)) x[!(x == "@id")]})
    suppressWarnings(
        index <- which(str_detect(ids, paste0('^', ref, '$')) == T))

    return(index)
}


#' Check the ORCIDs of creators in a given EML
#'
#' This function is called by \code{\link{qa_package}}.
#' See \code{\link{qa_package}} documentation for more details.
#'
#' @param doc (emld) Package metadata.
#'
#' @return creator_ORCIDs (character) Returns \code{character(0)} if any tests fail.
#'
#' @noRd
qa_creator_ORCIDs <- function(doc) {
    # Check creators
    creators <- doc$dataset$creator

    if (!is.null(names(creators))){
        creators <- list(doc$dataset$creator)
    }

    creator_ORCIDs <- unlist(arcticdatautils::eml_get_simple(creators, "userId")) %>%
        grep("orcid", ., value = T)

    creator_ORCIDs <- creator_ORCIDs[which(names(creator_ORCIDs) == "userId")]

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
#'
#' @noRd
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
        creator_rightsHolder <- sysmeta@rightsHolder %in% creator
        access <- c(creator_read, creator_write, creator_changePermission)

        if (!all(access) & !creator_rightsHolder) {
            cat(crayon::yellow("\nFull access for", sysmeta@identifier, "is not set for creator with ORCID", creator))
        }
    }
}
