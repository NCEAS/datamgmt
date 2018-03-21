#' Return the object metadata of each data object in a Data Package
#'
#' This function returns the formatId, fileName and identifier of each data
#' object in a Data Package.  This is a helper function for the function
#' 'clone_package' - which copies a dataOne Data Package from one member node to
#' another.
#'
#' @param mn (MNode/CNode) The Node to query for Object sizes
#' @param resource_map_pid (character) The identifier of the Data Package's Resource Map
#' @param formatType (character) Optional. Filter to just Objects of the given
#' formatType. One of METADATA, RESOURCE, or DATA or * for all types
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @return (character) The formatId, fileName, and identifier of each data object in a package.
get_object_metadata <- function(mn, resource_map_pid, formatType = "DATA") {
    query <- dataone::query(mn,
                            paste0("q=resourceMap:\"",
                                   resource_map_pid,
                                   "\"+AND+formatType:",
                                   formatType,
                                   "&fl=formatId+AND+fileName+AND+identifier"),
                            as = "data.frame")

    if (nrow(query) == 0) {
        return(0)
    }

    # Replace NA fields
    query$fileName[which(is.na(query$fileName))] <- query$identifier

    return(query)
}

#' Return data identifiers from within an EML
#'
#' This function returns data object identifers present within an EML (electronic
#' metadata language) document.
#'
#' @param eml () an EML object
#'
#' @return
#' Returns a list of data object pids in the eml
get_eml_pids <- function(eml) {
    # message if no dataTables / otherEntites are found

    # Get urls from EML
    dataTable_urls <- EML::eml_get(eml@dataset@dataTable, "url")
    otherEntity_urls <- EML::eml_get(eml@dataset@otherEntity, "url")

    urls <- vector("character")
    urls <- c(urls, unlist(dataTable_urls), unlist(otherEntity_urls))

    # Select the characters after the last "/" (the pid)
    pids <- sapply(urls, function(url) {
        split_string <- strsplit(url, split = "\\/")
        utils::tail(split_string[[1]], n = 1)
    })

    return(pids)
}

#' Clone a Data Package between Member Nodes without its child packages.
#'
#' @description The datamgmt wrapper function `clone_package` should be used instead.
#'
#' @param resource_map_pid (chraracter) The identifier of the Resource Map for the package to download.
#' @param from (MNode) The Member Node to download from.
#' @param to (MNode) The Member Node to upload to.
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @return (list) List of all the identifiers in the new Data Package.
clone_one_package <- function(resource_map_pid, from, to) {
    #' TODO switch remaining for loops to applys
    #' TODO add more messages
    stopifnot(is.character(resource_map_pid))
    stopifnot(methods::is(from, "MNode"))
    stopifnot(methods::is(to, "MNode"))

    package <- arcticdatautils::get_package(from, resource_map_pid)

    # Initialize data pids vector
    data_pids <- vector("character")
    if (length(package$data) != 0) {
        data_pids <- package$data
    }

    # Prepare the response object
    response <- list()

    # Solr query data formatId, fileName, and identifier
    data_meta <- get_object_metadata(from, resource_map_pid, formatType = "DATA")
    eml_meta <- get_object_metadata(from, resource_map_pid, formatType = "METADATA")

    # Write metadata
    meta_path <- file.path(tempdir(), eml_meta$fileName)
    writeBin(dataone::getObject(from, package$metadata), meta_path)
    new_eml_pid <- arcticdatautils::publish_object(to,
                                                   meta_path,
                                                   eml_meta$formatId)
    response[["metadata"]] <- new_eml_pid

    # Download pids, save in tempfiles, and publish to new node
    n_data_pids <- length(data_pids)

    if (n_data_pids > 0) {
        message(paste0("Uploading data objects from package: ", package$metadata))

        file_names <- solr_query$fileName
        format_ids <- solr_query$formatId
        data_paths <- file.path(tempdir(), file_names)

        # download data objects
        new_data_pids <- vector("character", length = n_data_pids)
        for (i in seq_len(n_data_pids)) {  # change to sapply

            # Attempt getObject up to 3 times
            n_attempts <- 0
            dataObj <- "upload_error"

            while (dataObj[1] == "upload_error" & n_attempts < 3) {
                dataObj <- tryCatch({
                    dataone::getObject(from, data_pids[i])

                    # Write object to temporary file
                    writeBin(dataObj, data_paths[i])

                    new_data_pids[i] <- arcticdatautils::publish_object(to,
                                                                        data_paths[i],
                                                                        format_ids[i])
                },
                error = function(e) {return("upload_error")})

                n_attempts <- n_attempts + 1
            }
        }
        response[["data"]] <- new_data_pids

    } else {
        response[["data"]] <- character(0)
    }

    # Create resource map
    if (length(new_data_pids) > 0) {
        new_resource_map_pid <- arcticdatautils::create_resource_map(to,
                                                                     new_eml_pid,
                                                                     new_data_pids)
    } else {
        new_resource_map_pid <- arcticdatautils::create_resource_map(to,
                                                                     new_eml_pid)
    }

    response[["resource_map"]] <- new_resource_map_pid

    return(response)
}

#' Clone a Data Package between Member Nodes.
#'
#' @description This function copies a Data Package from one DataOne member node to another.
#' This will not update the information in the metadata object.  This can also be used
#' to restore an older version of a Package to a member node, provided that the user subsequently
#' obsoletes the version of the package that they used to create the clone.
#'
#' @param resource_map_pid (chraracter) The PID of the Resource Map for the package to download.
#' @param from (MNode) The Member Node to download from.
#' @param to (MNode) The Member Node to upload to.
#' @param clone_child_packages (logical) Whether or not to clone the child packages.  Defaults to \code{FALSE}
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' First we set up the member nodes we're cloning between:
#' Member node we're cloning from:
#' cn_from <- dataone::CNode("PROD")
#' from <- dataone::getMNode(cn_from, "urn:node:ARCTIC")
#'
#' Member node we're cloning to:
#' cn_to <- dataone::CNode('STAGING')
#' to <- dataone::getMNode(cn_to,'urn:node:mnTestARCTIC')
#'
#' Clone the package using a resource map pid:
#' clone_package(resource_map_doi:10.18739/A2RZ6X", from, to)
#'
#'
#' Restore a previous version of a package:
#' This takes place on the same node, so we use the same node for 'from' and 'to':
#' cn <- dataone::CNode('STAGING')
#' from <- dataone::getMNode(cn_to,'urn:node:mnTestARCTIC')
#' to <- from
#'
#' This is the package we would like to restore:
#' package <- arcticdatautils::get_package(from, "resource_map")
#' clone <- clone_package(package$resource_map, from, to)
#'
#' Now we need to obsolete 'package' with 'clone' in the system metadata:
#' First we need to identify the most recent version of the package we cloned:
#' versions <- arcticdatautils::get_all_versions(from, package$resource_map)
#' most_recent_package <- arcticdatautils::get_package(from, tail(versions, n=1))
#'
#' s1 <- dataone::getSystemMetadata(to, most_recent_package$metadata)
#' This should be NA:
#' s1@@obsoletedBy
#' s1@@obsoletedBy <- clone$metadata
#' This should evaluate to TRUE:
#' dataone::updateSystemMetadata(to, most_recent_package$metadata, s1)
#'
#' s2 <- dataone::getSystemMetadata(to, clone$metadata)
#' This should be NA:
#' s2@@obsoletes
#' s2@@obsoletes <- most_recent_package$metadata
#' This should evaluate to TRUE:
#' dataone::updateSystemMetadata(to, clone$metadata, s2)
#'
#' }
#'
#' @export
clone_package <- function(resource_map_pid,
                          from,
                          to,
                          clone_child_packages = FALSE) {
    #' TODO - create dynamic structure that allows for more than one level of
    #' children (3+ nesting levels)
    #' TODO - add messages per child package?
    #' TODO - query all pids for unique rightsHolders and add to Sysmeta
    stopifnot(is.logical(clone_child_packages))

    # Get package information
    package <- arcticdatautils::get_package(from, resource_map_pid)

    # Clone the parent package
    response <- clone_one_package(resource_map_pid, from, to)

    if (clone_child_packages == TRUE) {
        n_child_packages <- length(package$child_packages)

        if (n_child_packages > 0) {

            # Clone child packages
            child_packages <- unlist(lapply(seq_len(n_child_packages), function(i) {
                clone_one_package(package$child_packages[i], from, to)
            }))

            # Select resource_map_pid(s) of child packages
            indices <- which(names(child_packages) == "resource_map")
            child_resource_map_pids <- child_packages[indices]

            # Nest child packages
            updated_resource_map_pid <- arcticdatautils::update_resource_map(to,
                                                                             response$resource_map,
                                                                             response$metadata,
                                                                             response$data,
                                                                             child_resource_map_pids)

            response[["resource_map"]] <- updated_resource_map_pid
            response[["child_packages"]] <- unname(child_resource_map_pids)
        }
    }

    return(response)
}
