#' Obsolete a DataOne Package with a new version.
#'
#' @description This function obsoletes a DataOne package with a newer version.
#' The ideal use case for this function is a broken metadata obsolescence chain.
#' In other cases \code{NCEAS/arcticdatautils::publish_update} should be used.
#'
#' @param mn (MNode) The DataOne member node
#' @param metadata_old (character) The metadata pid of the old version.
#' @param metadata_new (character) The metadata pid of the new version.
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('STAGING')
#' mn <- dataone::getMNode(cn,'urn:node:mnTestARCTIC')
#' pkg_old <- arcticdatautils::create_dummy_package(mn)
#' pkg_new <- arcticdatautils::create_dummy_package(mn)
#'
#' obsolete_package(mn, pkg_old$metadata, pkg_new$metadata)
#'}
obsolete_package <- function(mn, metadata_old, metadata_new) {
    # Argument checks
    stopifnot(methods::is(mn, "MNode"))
    stopifnot(is.character(metadata_old))
    stopifnot(is.character(metadata_new))
    stopifnot(arcticdatautils::object_exists(mn, metadata_old))
    stopifnot(arcticdatautils::object_exists(mn, metadata_new))

    # Pull system metadata
    message("Getting system metadata from member node\n")
    sys_old <- dataone::getSystemMetadata(mn, metadata_old)
    versions <- arcticdatautils::get_all_versions(mn, metadata_new)
    metadata_new <- versions[1]
    sys_new <- dataone::getSystemMetadata(mn, metadata_new)

    # Check that fields to update are NA
    if (!is.na(sys_old@obsoletedBy)) {
        stop(message("pid: ", metadata_old, " already obsoleted by: ",
                     sys_old@obsoletedBy, ". If you still wish to obsolete this version chain please use the last pid in the version chain."))
    }
    if (!is.na(sys_new@obsoletes)) {
        stop(message("pid: ", metadata_new, " already obsoletes: ",
                     sys_new@obsoletes, "."))
    }

    # Update system metadata
    message("Updating obsolescence chain in system metadata\n")
    sys_old@obsoletedBy <- metadata_new
    sys_new@obsoletes   <- metadata_old
    dataone::updateSystemMetadata(mn, metadata_old, sys_old)
    dataone::updateSystemMetadata(mn, metadata_new, sys_new)
}
