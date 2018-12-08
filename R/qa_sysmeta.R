#' Check rights and access for creators in system metadata
#'
#' This function checks if the creators of a data package have
#' rights and access set in the system metadata.
#'
#' @param sysmeta (SystemMetadata) A system metadata object.
#' @param eml (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom datapack hasAccessRule
#' @importFrom methods is
#' @importFrom stringr str_extract_all
#'
#' @noRd
qa_rights_access <- function(sysmeta, eml) {
    stopifnot(methods::is(sysmeta, "SystemMetadata"))
    stopifnot(methods::is(eml, "eml"))

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Store output messages in a vector
    messages <- c()

    # Check if the rightsHolder is a creator
    rightsHolder <- sysmeta@rightsHolder
    userIds <- unlist(EML::eml_get(eml@dataset@creator, "userId"))
    orcids <- unlist(stringr::str_extract_all(userIds, "http[s]?:\\/\\/orcid.org\\/[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}"))
    orcids <- sub("^https://", "http://", orcids)

    if (length(orcids) == 0) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "No creators have an ORCID. Unable to check if rights holder is one of the creators."
    } else {
        if (rightsHolder %in% orcids) {
            messages[[length(messages) + 1]] <- sprintf("The rights holder, %s, is one of the creators.", rightsHolder)
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- sprintf("The rights holder, %s, is not one of the creators.", rightsHolder)
        }
    }

    # Check if creators have full access
    for (creator in orcids) {
        creator_read <- datapack::hasAccessRule(sysmeta, creator, "read")
        creator_write <- datapack::hasAccessRule(sysmeta, creator, "write")
        creator_changePermission <- datapack::hasAccessRule(sysmeta, creator, "changePermission")
        access <- c(creator_read, creator_write, creator_changePermission)

        if (all(access)) {
            messages[[length(messages) + 1]] <- sprintf("Full access is set for creator: %s", creator)
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- sprintf("Full access is not set for creator: %s", creator)
        }
    }

    return(list(status = status,
                output = messages))
}


#' Check if format IDs match in EML and system metadata
#'
#' This function checks if the format IDs for data objects match in the EML and system metadata.
#'
#' @param sysmeta (SystemMetadata) A system metadata object.
#' @param entity (eml) An EML entity element associated with a data object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom methods is
#'
#' @noRd
qa_formatId <- function(sysmeta, entity) {
    stopifnot(methods::is(sysmeta, "SystemMetadata"))
    stopifnot(any(c("dataTable", "otherEntity", "spatialVector", "spatialRaster", "storedProcedure", "view") %in% class(entity)))

    sysmeta_format <- sysmeta@formatId
    eml_format <- EML::eml_get(entity, "formatName")

    if (length(sysmeta_format) == 0) {
        return(list(status = "FAILURE",
                    output = "Format ID is missing in system metadata."))
    } else if (length(eml_format) == 0) {
        return(list(status = "FAILURE",
                    output = "Format ID is missing in EML."))
    } else if (!identical(sysmeta_format, eml_format)) {
        return(list(status = "FAILURE",
                    output = "Format IDs in EML and system metadata are not identical."))
    } else {
        return(list(status = "SUCCESS",
                    output = "Format IDs in EML and system metadata are identical."))
    }
}


#' Quality assurance for system metadata
#'
#' This function checks the quality of system metadata for all elements of a data package,
#' including the resource map, metadata, and all data objects.
#'
#' @param mn (MNode) The Member Node to query.
#' @param resource_map_pid (character) The PID for a resource map.
#' @param all_results (logical) Return all results. If `FALSE`, only returns results with FAILURE or ERROR status.
#'
#' @return (list) A list of results.
#'
#' @import arcticdatautils
#' @import dataone
#' @import EML
#' @importFrom stringr str_detect
#'
#' @export
#'
#' @seealso [qa_eml()]
#'
#' @examples
#' \dontrun{
#' # Results with FAILURE or ERROR status
#' qa_sysmeta(knb_prod, "resource_map_urn:uuid:...")
#'
#' # All results
#' qa_sysmeta(knb_prod, "resource_map_urn:uuid:...", all_results = TRUE)
#' }
qa_sysmeta <- function(mn, resource_map_pid, all_results = FALSE) {
    stopifnot(class(mn) %in% c("MNode", "CNode"))
    stopifnot(is.character(resource_map_pid), nchar(resource_map_pid) > 0)
    stopifnot(is.logical(all_results))

    package <- tryCatch(suppressWarnings(arcticdatautils::get_package(mn, resource_map_pid, file_names = TRUE)),
                        error = function(e) stop("Failed to get package. Is the Member Node correct? Is your DataONE token set?"))

    eml <- EML::read_eml(dataone::getObject(mn, package$metadata))
    eml_objects <- c(EML::eml_get(eml, "dataTable"),
                     EML::eml_get(eml, "otherEntity"),
                     EML::eml_get(eml, "spatialVector"))
    if (length(eml_objects) == 0) {
        return(list("qa_sysmeta" = list(status = "FAILURE",
                                        output = "No data objects of a supported format were found in the EML.")))
    }
    # Preserve order of getting data objects based on data type for correct name assignment
    # Entity names may not match object names, so use objectName to ensure matches with sysmeta names
    names(eml_objects) <- unlist(c(EML::eml_get(eml@dataset@dataTable, "objectName"),
                                   EML::eml_get(eml@dataset@otherEntity, "objectName"),
                                   EML::eml_get(eml@dataset@spatialVector, "objectName")))
    # If object names are missing, use entity names instead
    if (is.null(names(eml_objects)) || any(is.na(names(eml_objects)))) {
        names(eml_objects) <- unlist(c(EML::eml_get(eml@dataset@dataTable, "entityName"),
                                       EML::eml_get(eml@dataset@otherEntity, "entityName"),
                                       EML::eml_get(eml@dataset@spatialVector, "entityName")))
    }

    sysmeta_rm <- dataone::getSystemMetadata(mn, package$resource_map)
    sysmeta_md <- dataone::getSystemMetadata(mn, package$metadata)
    sysmeta_data <- lapply(package$data, function(x) dataone::getSystemMetadata(mn, x))

    # If missing fileName, assign name to sysmeta for data objects
    for (i in seq_along(sysmeta_data)) {
        if (is.na(names(sysmeta_data)[[i]])) {
            id <- sysmeta_data[[i]]@identifier
            j <- which(stringr::str_detect(EML::eml_get(eml_objects, "url"), id))
            names(sysmeta_data)[[i]] <-
                if (!is.na(EML::eml_get(eml_objects[[j]], "objectName"))) {
                    EML::eml_get(eml_objects[[j]], "objectName")
                } else {
                    EML::eml_get(eml_objects[[j]], "entityName")
                }
        }
    }

    sysmeta_all <- c(sysmeta_rm, sysmeta_md, sysmeta_data)
    names(sysmeta_all)[[1]] <- "Resource_Map"
    names(sysmeta_all)[[2]] <- "Metadata"

    # Index objects in parallel based on names (in ascending order) for correct processing in iterations
    eml_objects <- eml_objects[order(names(eml_objects))]
    sysmeta_data <- sysmeta_data[order(names(sysmeta_data))]

    # Use tryCatch to return ERROR status if error is encountered
    err <- function(e) list(status = "ERROR")

    rights_access <- tryCatch(lapply(sysmeta_all, qa_rights_access, eml), error = err)
    format <- tryCatch(mapply(qa_formatId, sysmeta_data, eml_objects,
                              SIMPLIFY = FALSE), error = err) # do not simplify to ensure a list is returned

    results <- c(list("Rights_and_Access" = rights_access),
                 list("Format_ID" = format))

    # Default to only return results with FAILURE or ERROR status
    if (all_results) {
        return(results)
    } else {
        results <- lapply(results, function(x) Filter(function(x) stringr::str_detect(x$status, "FAILURE|ERROR"), x))
        # If all statuses are SUCCESS for certain checks, returns empty named list, so remove before returning results
        if (length(results[[2]]) == 0) results[[2]] <- NULL # Format_ID
        if (length(results[[1]]) == 0) results[[1]] <- NULL # Rights_and_Access
        if (length(results[[1]]) > 0) {
            return(results)
        } else {
            return(list("System_Metadata" = list(status = "SUCCESS",
                                                 output = "All QA checks were successful.")))
        }
    }
}
