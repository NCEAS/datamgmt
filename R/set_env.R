
#' set environment for publishing to a member node
#' loads in dataone, arcticdatautils, datamgmt, EML packages
#' @param coordinating_node: Coordinating node, as would be passed to a CNode call
#' @param member_node: Member node, as would be passed to a MNode call.
#' @return cn, mn: coordinating and member node instances (saved as global variables)
#' @examples set_env("PROD", "KNB")
#' set_env("PROD", "ARCTIC")
#' set_env("STAGING2", "mnTestKNB")
#' set_env("STAGING", "mnTestARCTIC")

set_env <- function(coordinating_node, member_node){
    library(dataone)
    library(arcticdatautils)
    library(datamgmt)
    library(EML)

    cn <<- CNode(coordinating_node)
    mn <<- getMNode(cn, paste('urn:node:', member_node, sep = ""))
}

