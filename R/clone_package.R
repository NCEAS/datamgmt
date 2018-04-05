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
#' @return (character) The formatId, fileName, identifier, and size of each object in a package.
get_package_metadata <- function(mn, resource_map_pid, formatType = "DATA") {
    results <- dataone::query(mn,
                              paste0("q=resourceMap:\"",
                                     resource_map_pid,
                                     "\"+AND+formatType:",
                                     formatType,
                                     "&fl=formatId+AND+fileName+AND+identifier+AND+size"),
                              as = "list")

    if (nrow(results) == 0) {
        return(0)
    }

    # Replace NA fields
    results$fileName[which(is.na(results$fileName))] <- results$identifier

    return(results)
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

#' Clones objects between Dataone Member Nodes.
#'
#' This is a helper function for datamgmt::clone_package
#'
#' @param resource_map_pid (character) Resource map that the target object pids.
#' @param pids (character) Object pids.
#' @param from (MNode) Dataone Member Node to clone objects from.
#' @param to (MNode) Dataone Member Node to clone objects to.
#' @param public (logical) Optional.  Set public read access.  Defaults to \code{FALSE}.
#' @param n_max (integer) Optional. Number of tries to download a DataOne object.
#' Can fail due to internet connectivity, suggested n_max >= 3.
#'
#' @return (list) Vector of data object pids.
clone_objects <- function(resource_map_pid,
                          pids,
                          from,
                          to,
                          public,
                          n_max = 3L) {
    stopifnot(is.character(resource_map_pid))
    stopifnot(is.character(pids))
    stopifnot(length(data_pids) > 0)
    stopifnot(methods::is(from, "MNode"))
    stopifnot(methods::is(to, "MNode"))
    stopifnot(is.numeric(n_max))

    metadata <- get_package_metadata(from, resource_map_pid, formatType = "DATA")
    format_ids <- metadata$formatId
    file_sizes <- metadata$size
    file_paths <- file.path(tempdir(), metadata$fileName)

    me <- arcticdatautils:::get_token_subject()

    accessPolicy <- dataone::getSystemMetadata(from, resource_map_pid)@accessPolicy
    if (public == TRUE) {
        sysmeta <- datapack::addAccessRule(sysmeta, "public", "read")
    }

    new_pids <- sapply(seq_along(pids), function(x) {paste0("urn:uuid:", UUIDgenerate())})

    for (i in seq_along(pids)) {

        # Initialize sysmeta
        sysmeta <- new("SystemMetadata", identifier = new_pids[i], size = file_sizes[i],
                       formatId = format_id, submitter = me, rightsHolder = me,
                       fileName = file_paths[i], originMemberNode = to@identifier,
                       authoritativeMemberNode = to@identifier)

        n_tries <- 0
        dataObj <- "error"

        while (dataObj[1] == "error" & n_tries < n_max) {
            dataObj <- tryCatch({
                dataObj <- dataone::getObject(from, pids[i])

                writeBin(dataObj, file_paths[i])

                new_data_pids[i] <- arcticdatautils::publish_object(to,
                                                                    file_paths[i],
                                                                    format_ids[i])
            },
            error = function(e) {return("error")})
            n_tries <- n_tries + 1
        }
    }

    return(new_pids)
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
    stopifnot(is.character(resource_map_pid))
    stopifnot(methods::is(from, "MNode"))
    stopifnot(methods::is(to, "MNode"))

    package <- arcticdatautils::get_package(from, resource_map_pid)

    # Prepare the response object
    response <- list()

    # Solr query data formatId, fileName, and identifier
    meta_eml <- get_package_metadata(from, package$resource_map, formatType = "METADATA")

    # Write metadata
    meta_path <- file.path(tempdir(), meta_eml$fileName)
    writeBin(dataone::getObject(from, package$metadata), meta_path)
    new_eml_pid <- arcticdatautils::publish_object(to,
                                                   meta_path,
                                                   meta_eml$formatId)
    response[["metadata"]] <- new_eml_pid

    # Initialize data pids vector
    data_pids <- vector("character")
    n_data_pids <- length(package$data)
    if (length(n_data_pids) > 0) {
        data_pids <- package$data
    }

    if (n_data_pids > 0) {
        message(paste0("\nUploading data objects from package: ", package$metadata))

        new_data_pids <- clone_objects(package$resource_map, package$data, from, to)
        response[["data"]] <- new_data_pids
    } else {
        response[["data"]] <- character(0)
    }

    # Create resource map
    if (n_data_pids > 0) {
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
#' @param resource_map_pid (chraracter) The PID of the Resource Map of the package to clone between nodes.
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
    #' TODO - query all pids for unique rightsHolders and add to Sysmeta
    stopifnot(is.logical(clone_child_packages))

    # Get package information
    package <- arcticdatautils::get_package(from, resource_map_pid)

    # Clone the parent package
    response <- clone_one_package(package$resource_map, from, to)

    if (clone_child_packages == TRUE) {
        n_child_packages <- length(package$child_packages)

        if (n_child_packages > 0) {
            progressBar <- utils::txtProgressBar(min = 0, max = n_child_packages, style = 3)

            # Clone child packages
            child_packages <- unlist(lapply(seq_len(n_child_packages), function(i) {
                pids <- clone_one_package(package$child_packages[i], from, to)
                utils::setTxtProgressBar(progressBar, i)
                pids
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
