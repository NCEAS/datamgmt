#' Clone a Data Package between Member Nodes without its child packages.
#'
#' @param resource_map_pid (chraracter) The identifier of the Resource Map for the package to download.
#' @param from (MNode) The Member Node to download from.
#' @param to (MNode) The Member Node to upload to.
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @return (list) List of all the identifiers in the new Data Package.
clone_package <- function(resource_map_pid, from, to) {
    #' TODO switch remaining for loops to applys
    #' TODO add more messages
    #' TODO better way to set physical in Update Metadata section?
    #' TODO pull/push terminology could potentially be confusing. perhaps consider download/upload, from/to, source/new could be better?  I do like that they match well (both 4-letter p-words)
    stopifnot(is.character(resource_map_pid))
    stopifnot(methods::is(from, "MNode"))
    stopifnot(methods::is(to, "MNode"))

    package <- arcticdatautils::get_package(from, resource_map_pid)

    # Initialize data pids vector
    data_pids <- vector("character")
    if (length(package$data) != 0) {
        data_pids <- package$data
    }

    meta_pid <- package$metadata

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

        # Compare pids in EML to pids in Data Package and return list
        old_data_pids <- compare_eml_to_package_pids(eml, package$data)

        # This sets the upload order to all dataTables first, then otherEnts.
        # Packages can contain dataTable and otherEntity objects - this sets the
        # order of the pids to the order in which they appear in first: dataTables
        # and second: otherEntities.  We can then systematically update the EML.
        data_pids <- unlist(old_data_pids)

        # Upload data objects and update metadata
        new_data_pids <- vector("character", length = n_data_pids)
        for (i in seq_len(n_data_pids)) {

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

        ## Update physical metadata for new_data_pids
        message("Updating metadata")

        n_dataTable <- length(old_data_pids$dataTable)
        n_otherEntity <- length(old_data_pids$otherEntity)
        new_physical <- arcticdatautils::pid_to_eml_physical(to, new_data_pids)

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
    new_eml_pid <- arcticdatautils::publish_object(to,
                                                   eml_path,
                                                   arcticdatautils::format_eml())
    response[["metadata"]] <- new_eml_pid

    # Create resource map
    if (length(new_data_pids) > 0) {
        new_resource_map_pid <- arcticdatautils::create_resource_map(to, new_eml_pid, new_data_pids)
    } else {
        new_resource_map_pid <- arcticdatautils::create_resource_map(to, new_eml_pid)
    }

    response[["resource_map"]] <- new_resource_map_pid

    return(response)
}