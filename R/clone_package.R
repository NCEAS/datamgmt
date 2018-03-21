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

    # Replace NA fields
    query$fileName[which(is.na(query$fileName))] <- query$identifier

    if (nrow(query) == 0) {
        return(0)
    }

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

#' Clone a Data Package
#'
#' This function copies a Data Package from one DataOne member node to another.
#' It can also be used to copy an older version of a Data Package to the same
#' member node in order to restore it, provided that the old Package is then
#' obsoleted by the copied version.
#'
#' @param mn_pull (MNode) The Member Node to download from.
#' @param mn_push (MNode) The Member Node to upload to.
#' @param resource_map_pid (chraracter) The identifier of the Resource Map for the package to download.
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' cn_pull <- CNode("PROD")
#' mn_pull <- getMNode(cn_pull, "urn:node:ARCTIC")
#' cn_push <- CNode('STAGING')
#' mn_push <- getMNode(cn_push,'urn:node:mnTestARCTIC')
#' clone_package(mn_pull, mn_push, "resource_map_doi:10.18739/A2RZ6X")
#' }
#'
#' @export
clone_packages <- function(mn_pull, mn_push, resource_map_pid) {
    #' TODO - create dynamic structure that allows for more than one level of
    #' children (3+ nesting levels)
    #' TODO - add messages per child package?
    #' TODO - possible function names? 1) clone_package 2) duplicate_package
    #' 3) copy_package
    #' TODO - query all pids for unique rightsHolders and add to Sysmeta
    # Clone initial package without children
    package <- clone_one_package(mn_pull, mn_push, resource_map_pid)

    if (length(package$child_packages) != 0) {

        n_child_packages <- length(package$child_packages)

        # Clone child packages
        child_packages <- unlist(lapply(seq_len(n_child_packages), function(i) {
            clone_one_package(mn_pull, mn_push, package$child_packages[i])
        }))

        # Select resource_map_pid(s) of child packages
        indices <- which(names(child_packages) == "resource_map")
        child_resource_map_pids <- child_packages[indices]

        # Nest child packages
        updated_resource_map_pid <- arcticdatautils::update_resource_map(mn_push,
                                                                         package$resource_map,
                                                                         package$metadata,
                                                                         package$data,
                                                                         child_resource_map_pids)

        package[["resource_map"]] = updated_resource_map_pid
    }

    return(package)
}

#' Clone a Data Package between Member Nodes without its child packages.
#'
#' @param resource_map_pid (chraracter) The identifier of the Resource Map for the package to download.
#' @param from (MNode) The Member Node to download from.
#' @param to (MNode) The Member Node to upload to.
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' cn_from <- CNode("PROD")
#' from <- getMNode(cn_from, "urn:node:ARCTIC")
#' cn_to <- CNode('STAGING')
#' to <- getMNode(cn_to,'urn:node:mnTestARCTIC')
#' clone_package("resource_map_doi:10.18739/A2RZ6X", from, to)
#' }
#'
#' @export
#'
#' @return (list) List of all the identifiers in the new Data Package.
clone_package <- function(resource_map_pid, from, to) {
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

    response <- vector("list", length = 3)

    # Solr query data formatId, fileName, and identifier
    solr_query <- get_object_metadata(from, resource_map_pid)
    file_names <- solr_query$fileName
    format_ids <- solr_query$formatId

    # Create temporary file paths
    data_paths <- file.path(tempdir(), file_names)

    # Download pids, save in tempfiles, and publish to new node
    n_data_pids <- length(data_pids)
    if (n_data_pids > 0) {
        message(paste0("Uploading data objects from package: ", package$metadata))

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

    # Write EML
    eml <- EML::read_eml(rawToChar(getObject(from, package$metadata)))
    eml_path <- file.path(tempdir(), "science_metadata.xml")
    EML::write_eml(eml, eml_path)
    new_eml_pid <- arcticdatautils::publish_object(to,
                                                   eml_path,
                                                   arcticdatautils::format_eml())
    response[["metadata"]] <- new_eml_pid

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
    names(response) <- c("data", "metadata", "resource_map")

    return(response)
}
