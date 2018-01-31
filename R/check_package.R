#' check_package
#'
#' This function perform checks on a package before publishing
#' @param x MNode
#' @param resourcemap resourcemap pid
check_package <- function(x, resourcemap) {
    pkg <- arcticdatautils::get_package(x, resource_map)
    eml <- EML::read_eml(rawToChar(getObject(x, pkg$metadata)))

    creators <- eml@dataset@creator
    if (length(creators) == 0) {
        stop("No creators are listed")
    }
    
    creator_ORCIDs <- c()
    for (c in seq_along(creators)) {
        userId_List <- creators[[c]]@userId
        for (u in seq_along(userId_List)) {
            userId <- userId_List[[u]]@.Data
            hasORCID <- grepl("https://orcid.org/[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}",userId)
            if (hasORCID) {
                creator_ORCIDs <- append(creator_ORCIDs,sub("https://","http://",userId,fixed = T))
            } else {
                stop("Each creator needs an ORCID")
            }
        }

    }

    pids <- c(pkg$metadata, pkg$resource_map, pkg$data)
    for (pid in pids) {
        sysmeta <- dataone::getSystemMetadata(x, pid)
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
                        stop(physical[[p]]@objectName, " Online Distribution Info pid not in package data pid")
                    } else {
                        datanum <- which(url_pid == pkg$data)
                        sysmeta_data <- dataone::getSystemMetadata(x, pkg$data[datanum])
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

