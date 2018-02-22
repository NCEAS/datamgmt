#' Update dataTable/otherEntity of an updated data object
#'
#' This function updates the EML with the new physical
#' location of a data object once it has been updated.
#' This is a helper function for
#' \code{\link{update_package_object}}.
#'
#' @param eml An EML class object
#' @param mn (MNode) Member Node of the data package
#' @param data_pid (character) The identifier of the data
#' object to be updated
#' @param new_data_pid (character) The new identifier of the
#' updated data object.

update_physical <- function(eml,
                            mn,
                            data_pid,
                            new_data_pid) {

    if (!requireNamespace("stringr")) {
        stop(call. = FALSE,
             "The stringr package is required. Please install it and try again.")
    }

    dataTable_url <- unlist(EML::eml_get(eml@dataset@dataTable, "url"))

    if(!is.null(dataTable_url) && stringr::str_detect(dataTable_url, data_pid)){
        position <- which(stringr::str_detect(dataTable_url, data_pid))
        new_phys <- arcticdatautils::pid_to_eml_physical(mn, new_data_pid)
        eml@dataset@dataTable[[position]]@physical@.Data <- new_phys
    }

    otherEntity_url <- unlist(EML::eml_get(eml@dataset@otherEntity, "url"))

    if(!is.null(dataTable_url) && stringr::str_detect(otherEntity_url, data_pid)){
        position <- which(stringr::str_detect(otherEntity_url, data_pid))
        new_phys <- arcticdatautils::pid_to_eml_other_entity(mn, new_data_pid)
        eml@dataset@otherEntity[[1]] <- new_phys
    }

    invisible(eml)
}

#' Update a data object and accompanying package resource map
#'
#' This function updates an data object, and then automatically
#' updates the package resource map with the new data pid. It
#' is a convenience wrapper around \link[arcticdatautils]{update_object}}
#' and \link[arcticdatautils]{update_resource_map}}.
#'
#' @param mn (MNode) Member node
#' @param data_pid (character) PID for data object to update
#' @param new_data_path (character) Path to new data object
#' @param resource_map_pid (character) PID for resource map to update
#' @param format_id (character) Optional. The format ID to set for the object.
#' When not set, \link[arcticdatautils]{guess_format_id}} will be used
#' to guess the format ID. Should be a \href{https://cn.dataone.org/cn/v2/formats}{DataONE format ID}.
#' @param public (logical) Optional. Make the update public. If FALSE,
#' will set the metadata and resource map to private (but not the data objects).
#' This applies to the new metadata PID and its resource map and data object.
#' Access policies are not affected.
#' @param use_doi (logical) Optional. If TRUE, a new doi will be minted.
#'
#' @keywords update_object update_resource_map
#'
#' @import arcticdatautils
#' @import dataone
#'
#' @export

update_package_object <- function(mn,
                                  data_pid,
                                  new_data_path,
                                  resource_map_pid,
                                  format_id = NULL,
                                  public = FALSE,
                                  use_doi = FALSE) {

    #argument checks:
    stopifnot(is(mnTest, "MNode"))
    stopifnot(is.character(data_pid))
    stopifnot(is.character(new_data_path))
    stopifnot(file.exists(new_data_path))
    stopifnot(is.character(resource_map_pid))
    stopifnot(is.logical(public))

    pkg <- arcticdatautils::get_package(mn, resource_map_pid)

    new_data_pid <- arcticdatautils::update_object(mn,
                                                   data_pid,
                                                   new_data_path,
                                                   format_id = format_id)

    cat("The new data pid is", new_data_pid)

    #store new pids (nonupdated pids + new pid) as a vector
    other_data_pids <- pkg$data[which(pkg$data != data_pid)] #wrapped in which for better NA handling
    new_data_pids <- c(other_data_pids, new_data_pid)

    #update EML
    eml <- EML::read_eml(rawToChar(dataone::getObject(mn, pkg$metadata)))

    eml <- update_physical(eml = eml,
                           mn = mn,
                           data_pid = data_pid,
                           new_data_pid = new_data_pid)

    eml_path <- tempfile(fileext = ".xml")
    write_eml(eml, eml_path)

    pkg_new <- publish_update(mn,
                              metadata_pid = pkg$metadata,
                              resource_map_pid = pkg$resource_map,
                              metadata_path = eml_path,
                              data_pid = new_data_pids,
                              use_doi = use_doi,
                              public = public)

    return(pkg_new)
}
