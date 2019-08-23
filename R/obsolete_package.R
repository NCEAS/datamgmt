#' Obsolete a DataONE Package with a new version
#'
#' This function obsoletes a DataONE package with a newer version
#' by merging the two version chains. The ideal use case for this function is
#' when the only option to fix a broken package is by re-uploading a previous
#' version and merging the two version chains. In other cases
#' [arcticdatautils::publish_update()] should be used.
#'
#' @param mn (MNode) The DataONE member node.
#' @param metadata_obsolete (character) The metadata PID of the old, or broken, version.
#' Sets the PID to the beginning of the version chain (the entry at index=1 of the vector
#' returned by \code{arcticdatautils::get_all_versions}.
#' @param metadata_new (character) The metadata PID of the new version. Sets the PID to
#' the end of the version chain (the last item in the vector returned by
#' \code{arcticdatautils::get_all_versions}).
#'
#' @return (logical) `TRUE`/`FALSE`
#'
#' @importFrom utils tail
#'
#' @export
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode("STAGING")
#' mn <- dataone::getMNode(cn,"urn:node:mnTestARCTIC")
#'
#' pkg_old <- arcticdatautils::create_dummy_package(mn)
#' pkg_new <- arcticdatautils::create_dummy_package(mn)
#'
#' obsolete_package(mn, pkg_old$metadata, pkg_new$metadata)
#' }
obsolete_package <- function(mn, metadata_obsolete, metadata_new) {
    # Check that token is set
    if (!arcticdatautils::is_token_set(mn)) {
        stop("Token is not set")
    }

    # shorten for readability
    metadata_obs <- metadata_obsolete

    # Argument checks
    stopifnot(methods::is(mn, "MNode"))
    stopifnot(is.character(metadata_obs))
    stopifnot(is.character(metadata_new))
    stopifnot(arcticdatautils::object_exists(mn, metadata_obs))
    stopifnot(arcticdatautils::object_exists(mn, metadata_new))

    # Get all versions
    versions_obs <- arcticdatautils::get_all_versions(mn, metadata_obs)
    versions_new <- arcticdatautils::get_all_versions(mn, metadata_new)

    # Check that the pids are not in the same chain already
    if (metadata_obs %in% versions_new || metadata_new %in% versions_obs) {
        stop(message("PID: ", metadata_obs, " and PID: ", metadata_new,
                     " are already in the same version chain."))
    }

    # Check that pids are at the end and beginning of respective chains, if not then update them.
    if (metadata_obs != utils::tail(versions_obs, 1)) {
        message(warning("'metadata_obsolete' argument is not at the end of the version chain. Setting the 'metadata_obsolete' argument from: ",
                metadata_obs, " to: ", utils::tail(versions_obs, 1)))
        metadata_obs <- utils::tail(versions_obs, 1)
    }
    if (metadata_new != versions_new[1]) {
        message(warning("'metadata_new' argument is not at the start of the version chain. Setting the 'metadata_new' argument from: ",
                metadata_new, " to: ", versions_new[1]))
        metadata_new <- versions_new[1]
    }

    # Pull system metadata
    message("Getting system metadata from member node\n")
    sys_obs <- dataone::getSystemMetadata(mn, metadata_obs)
    sys_new <- dataone::getSystemMetadata(mn, metadata_new)

    # Check that sysmeta fields to update are NA
    if (!is.na(sys_obs@obsoletedBy)) {
        stop(message("pid: ", metadata_obs, " already obsoleted by: ",
                     sys_obs@obsoletedBy, ". If you still wish to obsolete this version chain please use the last PID in the version chain."))
    }
    if (!is.na(sys_new@obsoletes)) {
        stop(message("pid: ", metadata_new, " already obsoletes: ",
                     sys_new@obsoletes, "."))
    }

    # Update system metadata
    message("Updating version chain in system metadata\n")
    sys_obs@obsoletedBy <- metadata_new
    sys_new@obsoletes   <- metadata_obs
    dataone::updateSystemMetadata(mn, metadata_obs, sys_obs)
    dataone::updateSystemMetadata(mn, metadata_new, sys_new)
}
