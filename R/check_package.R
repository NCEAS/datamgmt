#' check_package
#'
#' This function perform checks on a package before publishing
#' @param mn MNode
#' @param resourcemap resource_map pid
check_package <- function(mn, resource_map) {

    stopifnot(is(mn, "MNode"))
    stopifnot(is.character(resource_map))

    pkg <- arcticdatautils::get_package(mn, resource_map)
    eml <- EML::read_eml(rawToChar(getObject(mn, pkg$metadata)))

    creators <- eml@dataset@creator
    if (length(creators) == 0) {
        stop("No creators are included in eml")
    }

    creator_ORCIDs <- c()
    for (c in seq_along(creators)) {
        userId_List <- creators[[c]]@userId
        if (length(userId_List)==0){
            stop("Each creator needs an ORCID")
        }
        for (u in seq_along(userId_List)) {
            userId <- userId_List[[u]]@.Data
            isORCID <- grepl("https://orcid.org/[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}",userId)
            if (isORCID) {
                creator_ORCIDs <- append(creator_ORCIDs,sub("https://","http://",userId,fixed = T))
            } else {
                stop(paste0(userId," is not of the ORCID form https://orcid.org/AAAA-BBBB-CCCC-DDDD"))
            }
        }

    }

    pids <- c(pkg$metadata, pkg$resource_map, pkg$data)
    for (pid in pids) {
        sysmeta <- dataone::getSystemMetadata(mn, pid)
        if (!(sysmeta@rightsHolder %in% creator_ORCIDs)) {
            stop(paste0("rightsHolder is not set to one of the creators in ", pid))
        }
        permissions <- c("read","write","changePermission")
        for (c in seq_along(creator_ORCIDs)) {
            for (permission in permissions) {
                if (!(datapack::hasAccessRule(sysmeta, creator_ORCIDs[[c]], permission))) {
                    stop(paste0(creator_ORCIDs[[c]]," does not have ", permission, " access in ", pid))
                }
            }
        }
    }

    cat("\nAll creators are rightsHolders for all objects\n")
    cat("\nAll creators have read, write, changePermission access for all objects\n")

    test_data <- function(object) {
        for (i in seq_along(object)) {
            physical <- object[[i]]@physical
            for (p in seq_along(physical)) {
                distribution <- physical[[p]]@distribution
                for (d in seq_along(distribution)) {
                    url <- distribution[[d]]@online@url@.Data
                    url_pid <- stringr::str_extract(url, "[^/]*$")
                    inData <- (url_pid %in% pkg$data)
                    if (!inData) {
                        stop(paste0(physical[[p]]@objectName, " has different physical and sysmeta pids"))
                    } else {
                        datanum <- which(url_pid == pkg$data)
                        sysmeta_data <- dataone::getSystemMetadata(mn, pkg$data[datanum])
                        isName <- (sysmeta_data@fileName == physical[[p]]@objectName)
                        if(!isName) {
                            stop(paste0(physical[[p]]@objectName," has sysmeta fileName ", sysmeta_data@fileName))
                        }
                    }
                }
            }
        }
    }

    dataTables <- length(eml@dataset@dataTable)
    otherEntities <- length(eml@dataset@otherEntity)

    if (dataTables>0) {
        test_data(eml@dataset@dataTable)
    }

    cat("\nAll dataTables have the correct filename and pid set\n")

    if (otherEntities > 0) {
        test_data(eml@dataset@otherEntity)
    }

    cat("\nAll otherEntities have the correct filename and pid set\n")
}

