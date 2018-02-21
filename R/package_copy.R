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
    query$formatId[which(is.na(query$formatId))] <- "application/octet-stream"
    query$fileName[which(is.na(query$fileName))] <- query$identifier

    if (nrow(query) == 0) {
        return(0)
    }

    return(query)
}

#' Return data object identifiers from 'dataTable' and 'otherEntity' objects
#' in an EML file
#'
#' This function is a helper function for 'clone_package'.  It checks that all
#' of the data objects pids present in the metadata match those that the
#' resource map points to.  It also returns the data pids in the order in which
#' they appear in the EML: dataTable pids first and then otherEntity pids.  We
#' cannot update the new .xml file without this information.
#'
#' @param eml (EML) EML object
#' @param package_data_pids (character) All of the Data identifiers in a Data
#' Package.  These can be selected with get_package(mn, resource_map)$data
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @return (list) List of all the identifiers in the EML.  Sorted into dataTable
#' and otherEntity identifiers
compare_eml_to_package_pids <- function(eml, package_data_pids) {
    #' TODO better name for this function?

    n_dataTable = length(eml@dataset@dataTable)
    n_otherEntity = length(eml@dataset@otherEntity)
    pids_from_eml <- list()

    # Check that number of dataTable and OtherEntity objects == number of pids in the Package
    if ((n_dataTable + n_otherEntity) != length(package_data_pids)) {
        stop(message("Number of 'dataTable' and 'otherEntity' objects ",
                     n_dataTable + n_otherEntity,
                     " does not equal the number of data objects ",
                     length(package_data_pids)))
    }

    if(n_dataTable > 0 | n_otherEntity > 0) {

        # format dataTable pids
        dataTable_pids <- unlist(lapply(seq_len(n_dataTable), function(i) {
            pid_link <- eml@dataset@dataTable[[i]]@physical@.Data[[1]]@distribution@.Data[[1]]@online@url
            pid_link <- as.character(pid_link)

            # Select the pid --> characters after the last "/"
            split_string <- strsplit(pid_link, split = "\\/")
            pid <- utils::tail(split_string[[1]], n = 1)

        }))
        pids_from_eml[["dataTable"]] <- dataTable_pids

        # format otherEntity pids
        otherEntity_pids <- unlist(lapply(seq_len(n_otherEntity), function(i) {
            pid_link <- eml@dataset@otherEntity[[i]]@physical@.Data[[1]]@distribution@.Data[[1]]@online@url
            pid_link <- as.character(pid_link)

            # Select the pid --> characters after the last "/"
            split_string <- strsplit(pid_link, split = "\\/")
            pid <- utils::tail(split_string[[1]], n = 1)

        }))
        pids_from_eml[["otherEntity"]] <- otherEntity_pids

        # Check that pids from EML are in package$data
        pids_in_eml <- unlist(pids_from_eml)
        if (any(!(pids_in_eml %in% package_data_pids))) {

            # Select which pids in EML are not in package$data
            indices <- which(!(pids_in_eml %in% package_data_pids))
            incorrect_pids <- paste(pids_in_eml[indices], collapse = ", ")

            stop(message(paste0("Incorrect data object pids present in the EML: ",
                                incorrect_pids,
                                ". Please check that all 'dataTable' and 'otherEntity' objects have accurate physical sections.")))
        }

    } else {

        stop(message("No 'dataTable' or 'otherEntity' objects present in EML"))
    }

    return(pids_from_eml)
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

#' Clone a Data Package without its child packages.
#'
#' The wrapper function 'clone_package' should be used instead. This function
#' copies a data package from one DataOne member node to another, excluding any
#' child data packages.
#'
#' @param mn_pull (MNode) The Member Node to download from.
#' @param mn_push (MNode) The Member Node to upload to.
#' @param resource_map_pid (chraracter) The identifier of the Resource Map for the package to download.
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @return (list) List of all the identifiers in the new Data Package.
clone_one_package <- function(mn_pull, mn_push, resource_map_pid) {
    #' TODO switch remaining for loops to applys
    #' TODO add more messages
    #' TODO better way to set physical in Update Metadata section?
    #' TODO pull/push terminology could potentially be confusing. perhaps consider download/upload, from/to, source/new could be better?  I do like that they match well (both 4-letter p-words)
    stopifnot(is.character(resource_map_pid))
    stopifnot(methods::is(mn_pull, "MNode"))
    stopifnot(methods::is(mn_push, "MNode"))

    package <- arcticdatautils::get_package(mn_pull, resource_map_pid)

    response <- list()
    response[["child_packages"]] <- package$child_packages

    # Download EML
    message(paste0("Downloading metadata from package: ", package$metadata))
    #' TODO since messages print in red (scary!), you might want to consider the crayon workaround you found. maybe it's worth having a discussion on our package 'style'?
    eml <- EML::read_eml(rawToChar(dataone::getObject(mn_pull, package$metadata)))

    # Initialize data pids vector
    data_pids <- vector("character")
    if (length(package$data) != 0) {
        data_pids <- package$data
    }

    # Solr query data formatId, fileName, and identifier
    solr_query <- get_object_metadata(mn_pull, resource_map_pid)
    file_names <- solr_query$fileName
    format_ids <- solr_query$formatId

    # Create temporary file paths
    data_paths <- file.path(tempdir(), file_names)

    # Download pids, save in tempfiles, and publish to new node
    n_data_pids <- length(data_pids)
    if (n_data_pids > 0) {
        message(paste0("Uploading data objects from package: ", package$metadata))

        # Compare pids in EML to pids in Data Package and return list
        old_data_pids <- compare_eml_to_package_pids(eml, package$data)

        # This sets the upload order to all dataTables first, then otherEnts.
        # Packages can contain dataTable and otherEntity objects - this sets the
        # order of the pids to the order in which they appear in first: dataTables
        # and second: otherEntities.  We can then systematically update the EML.
        data_pids <- unlist(old_data_pids)

        # Upload data objects and update metadata
        new_data_pids <- vector("character")
        for (i in seq_len(n_data_pids)) {

            # Attempt getObject up to 3 times
            n_attempts <- 0
            dataObj <- "upload_error"

            while (dataObj[1] == "upload_error" & n_attempts < 3) {
                dataObj <- tryCatch({
                    dataone::getObject(mn_pull, data_pids[i])

                    # Write object to temporary file
                    writeBin(dataObj, data_paths[i])

                    new_data_pids[i] <- arcticdatautils::publish_object(mn_push,
                                                                        data_paths[i],
                                                                        format_ids[i])
                },
                error = function(e) {return("upload_error")})

                n_attempts <- n_attempts + 1
            }
        }

        response[["data"]] <- new_data_pids

        ## Update physical metadata for new_data_pids
        message("Updating metadata")

        n_dataTable <- length(old_data_pids$dataTable)
        n_otherEntity <- length(old_data_pids$otherEntity)
        new_physical <- arcticdatautils::pid_to_eml_physical(mn_push, new_data_pids)

        # First update dataTables using the number from old_data_pids
        for(i in seq_len(n_dataTable)) {
            eml@dataset@dataTable@.Data[[i]]@physical <- methods::new("ListOfphysical",
                                                                      list(new_physical[[i]]))
        }

        for(i in seq_len(n_otherEntity)) {
            eml@dataset@otherEntity[[i]]@physical <- methods::new("ListOfphysical",
                                                                  list(new_physical[[i+ n_dataTable]]))
        }


    } else {

        response[["data"]] <- character(0)
    }

    # Write EML
    eml_path <- file.path(tempdir(), "science_metadata.xml")
    EML::write_eml(eml, eml_path)
    new_eml_pid <- arcticdatautils::publish_object(mn_push,
                                                   eml_path,
                                                   arcticdatautils::format_eml())
    response[["metadata"]] <- new_eml_pid

    # Create resource map
    if (length(new_data_pids) > 0) {
        new_resource_map_pid <- arcticdatautils::create_resource_map(mn_push, new_eml_pid, new_data_pids)
    } else {
        new_resource_map_pid <- arcticdatautils::create_resource_map(mn_push, new_eml_pid)
    }

    response[["resource_map"]] <- new_resource_map_pid

    return(response)
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
clone_package <- function(mn_pull, mn_push, resource_map_pid) {
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
