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
#' @return (character)
#'
#' @examples
#' \dontrun{
#' package_to_obsolete <- "doi:10.18739/A2FJ29C23"
#'
#' pkg_old <- arcticdatautils::create_dummy_package(mn_test)
#' pkg_new <- arcticdatautils::create_dummy_package(mn_test)
#'
#' obsolete_package(mn_test, pkg_old$metadata, pkg_new$metadata)
#'}
obsolete_package <- function(mn, metadata_old, metadata_new) {
    # Argument checks
    stopifnot(methods::is(mn, "MNode"))
    stopifnot(is.character(metadata_old))
    stopifnot(is.character(metadata_new))
    stopifnot(arcticdatautils::object_exists(mn, metadata_old))
    stopifnot(arcticdatautils::object_exists(mn, metadata_new))

    # Update version chain of metadata_old
    sys1 <- dataone::getSystemMetadata(mn, metadata_old)
    if (!is.na(sys1@obsoletedBy)) {
        stop(message("pid: ", metadata_old, " already obsoleted by: ",
                     sys1@obsoletedBy, ". If you still wish to obsolete this version
                     chain please use the newest version."))
    }
    sys1@obsoletedBy <- metadata_new
    dataone::updateSystemMetadata(mn, metadata_old, sys1)

    # Set to first version of metadata_new in obsolescence chain
    versions <- arcticdatautils::get_all_versions(mn, metadata_new)
    metadata_new <- versions[1]

    # Update sysmeta
    sys2 <- dataone::getSystemMetadata(mn, metadata_new)
    stopifnot(is.na(sys2@obsoletes))
    sys2@obsoletes <- metadata_old
    dataone::updateSystemMetadata(mn, metadata_new, sys2)
}
