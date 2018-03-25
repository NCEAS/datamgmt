#' Download a csv file from a DataOne node
#'
#' Downloads a csv file from a DataOne node; allows you to access the data in your
#' scripts without downloading the data onto your hard drive. Ensures that the original
#' data stays intact. This function is a wrapper for \link[dataone]{getObject}.
#'
#' @param mn A member node
#' @param pid The identifier of the data object
#'
#' @import dataone
#' @import tidyverse
#'
#' @export
#'
#' @author Irene Steves
#'
#' @example
#' \dontrun{
#' cn <- CNode('PROD')
#' mn <- getMNode(cn,'urn:node:ARCTIC')
#'
#' data <- get_csv(mn, "urn:uuid:087bf762-ea3d-444f-ba59-1c0bc02fd415")
#' }
#'

get_csv <- function(mn, pid) {
    data_raw <- getObject(mn, pid)
    # same as: getDataObject(d1c, pid) %>% getData()
    data_char <- rawToChar(obj) %>%
        str_split("\r\n") %>%
        unlist() %>%
        str_split(",")
    data_combined <- do.call(rbind, data_char)
    data_clean <- data.frame(data_combined[-1,])
    colnames(data_clean) <- data_combined[1,]
    return(data_clean)
}

#' Download an EML file from a DataOne node
#'
#' Downloads an EML file from a DataOne node. This function is a wrapper for
#' \link[dataone]{getObject}.
#'
#' @param mn A member node
#' @param pid The identifier of the data object
#'
#' @import dataone
#' @import tidyverse
#'
#' @export
#'
#' @example
#' \dontrun{
#' cn <- CNode('PROD')
#' mn <- getMNode(cn,'urn:node:ARCTIC')
#'
#' eml <- get_eml(mn, "doi:10.18739/A2TP3N")
#' }
#'
#'

get_eml <- function(mn, pid) {
    dataone::getObject(mn, pid) %>%
        rawToChar() %>%
        EML::read_eml()
}
