#' update_package_object
#'
#' This function updates an data object, and then automatically
#' updates the package resource map with the new data pid. It
#' is a convenience wrapper around 'arcticdatautils::update_object'
#' and 'arcticdatautils::update_resource_map'.
#'
#' @param mn (MNode) Member node
#' @param data_pid (character) PID for data object to update
#' @param new_data_path (character) Path to new data object
#' @param rm_pid (character) PID for resource map to update.
#'
#' @keywords update_object update_resource_map
#'
#' @export

update_package_object <- function(mn,
                           data_pid,
                           new_data_path,
                           rm_pid,
                           format_id = NULL,
                           public = FALSE) {

    #function checks:
    stopifnot(is.character(data_pid))
    stopifnot(is.character(new_data_path))
    stopifnot(is.character(rm_pid))

    new_data_pid <- update_object(mn,
                                  data_pid,
                                  new_data_path,
                                  format_id = format_id)

    pkg <- get_package(mn, rm_pid)
    other_data_pids <- pkg$data[pkg$data != data_pid]
    new_data_pids <- c(other_data_pids,
                       new_data_pid)

    new_rm <- update_resource_map(mn,
                        pkg$resource_map,
                        pkg$metadata,
                        new_data_pids,
                        public = public)

    #output package info
    get_package(mn, new_rm)
}
