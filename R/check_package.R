#' check_package
#'
#' This function perform checks on a package before publishing
#' @param mn MNode
#' @param resource_map resource_map pid
check_package <- function(mn, resource_map) {

    stopifnot(is(mn, "MNode"))
    stopifnot(is.character(resource_map))

    cat("\nRunning Check on Package...")
    pkg <- arcticdatautils::get_package(mn, resource_map, file_names = TRUE)
    eml <- EML::read_eml(rawToChar(getObject(mn, pkg$metadata)))

    pids <- vapply(unlist(eml_get(eml, "url")), function(x) {
        stringr::str_match_all(x, "https?:\\/\\/.+\\.dataone\\.org\\/cn\\/v\\d\\/resolve\\/(.+)")[[1]][2]
    }, "", USE.NAMES = FALSE)

    if(length(pids) != length(pkg$data)){
        stop("Number of dataTables and otherEntities is ", length(pids),
                ". Number of data in package is ", length(pids))
    }

    pkg_in_pid <- (pkg$data %in% pids)
    if(!all(pkg_in_pid)){
        stop("The following pids are in the package and not in the EML.", paste("\n",pkg$data[!pkg_in_pid]))
    }

    cat("\nThe pids in the package are the same as the pids in the EML.")

    objectNames <- unlist(eml_get(eml, "objectName"))
    pkg_Names <- names(pkg$data)

    Names_in_EML <- (pkg_Names %in% objectNames)
    if(!all(Names_in_EML)){
        stop("The following file names are listed in the package and not the EML", paste("\n",pkg_Names[!Names_in_EML]))
    }

    cat("\nThe data names in the package are the same as the data names in the EML.")

    creators <- eml@dataset@creator
    if (length(creators) == 0) {
        stop("The EML needs a creator.")
    }

    creator_ORCIDs <- unlist(eml_get(creators,"userId"))

    isORCID <-  grepl("https:\\/\\/orcid.org\\/[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}",creator_ORCIDs)

    if(!all(isORCID)){
        stop(creator_ORCIDs[!isORCID], " is not of the form https://orcid.org/AAAA-BBBB-CCCC-DDDD")
    }

    if(length(creator_ORCIDs)!=length(creators)){
        warning("Each Creator should have an ORCID.")
    }

    creator_ORCIDs <- sub("https://","http://",creator_ORCIDs,fixed = T)


    all_pids <- c(pkg$metadata, pkg$resource_map, pkg$data)
    permissions <- c("read","write","changePermission")

    for (pid in all_pids) {
        sysmeta <- dataone::getSystemMetadata(mn, pid)

        if (!(sysmeta@rightsHolder %in% creator_ORCIDs)) {
            stop("rightsHolder is not set to one of the creators for ", pid)
        }

        for (c in seq_along(creator_ORCIDs)) {
            for (permission in permissions) {

                if (!(datapack::hasAccessRule(sysmeta, creator_ORCIDs[[c]], permission))) {
                    stop(paste0(creator_ORCIDs[[c]]," does not have ", permission, " access in ", pid))

                }
            }
        }

        cat("\nrightsHolder and access set correctly for ", pid)

    }

}


