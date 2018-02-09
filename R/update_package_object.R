#' Update a data object and accompanying package resource map
#'
#' This function updates an data object, and then automatically
#' updates the package resource map with the new data pid. It
#' is a convenience wrapper around \code{\link{arcticdatautils::update_object}}
#' and \code{\link{arcticdatautils::update_resource_map}}.
#'
#' @param mn (MNode) Member node
#' @param data_pid (character) PID for data object to update
#' @param new_data_path (character) Path to new data object
#' @param resource_map_pid (character) PID for resource map to update
#' @param format_id (character) Optional. The format ID to set for the object.
#' When not set, \code{\link{arcticdatautils::guess_format_id}} will be used
#' to guess the format ID. Should be a \href{https://cn.dataone.org/cn/v2/formats}{DataONE format ID}.
#' @param public (logical) Optional. Make the update public. If FALSE,
#' will set the metadata and resource map to private (but not the data objects).
#' This applies to the new metadata PID and its resource map and data object.
#' Access policies are not affected.
#'
#' @keywords update_object update_resource_map
#'
#' @export

update_package_object <- function(mn,
                           data_pid,
                           new_data_path,
                           resource_map_pid,
                           format_id = NULL,
                           public = FALSE) {

    #argument checks:
    library(assertthat)
    library(arcticdatautils)
    assertthat::assert_that(is(mnTest, "MNode"),
                is.character(data_pid),
                is.character(new_data_path),
                file.exists(new_data_path),
                is.character(resource_map_pid),
                is.logical(public))

    new_data_pid <- arcticdatautils::update_object(mn,
                                  data_pid,
                                  new_data_path,
                                  format_id = format_id)

    pkg <- arcticdatautils::get_package(mn, resource_map_pid)
    other_data_pids <- pkg$data[pkg$data != data_pid]
    new_data_pids <- c(other_data_pids,
                       new_data_pid)

    new_resource_map_pid <- arcticdatautils::update_resource_map(mn,
                        pkg$resource_map,
                        pkg$metadata,
                        new_data_pids,
                        public = public)

    #output package info
    get_package(mn, new_resource_map_pid)
}
