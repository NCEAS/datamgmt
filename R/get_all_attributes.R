#' Get all attribute from an EML document.  Document for the function \code{get_attributes}
#' from the EML(url) Package
#'
#' @param eml (S4) Optional. The full eml document. Required if \code{url} is not specified.
#' @param url (character) Optional. The url to the Dataone Package. Required if \code{eml} is not specified.
#' @param node (MNode/CNode) Optional.  Defaults to Arctic Data Center Production node.
#' @return (list) A list of all attribute metadata from the EML in data.frame objects
#'
#' @export
#'
#' @author Dominic Mullen
#'
#' @examples
#' \dontrun{
#' install_github("ropensci/EML)
#' library(EML)
#' library(dataone)
#' cn <- dataone::CNode('PROD')
#' node <- dataone::getMNode(cn,'urn:node:ARCTIC')
#' eml <- EML::read_eml(rawToChar(dataone::getObject(node, "doi:10.18739/A23W02")))
#' attributes <- datamgmt::get_all_attributes(eml)
#'
#' attributes <- datamgmt::get_all_attributes(link = "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02")
#'
#' # switch nodes
#' cn <- dataone::CNode('PROD')
#' knb <- dataone::getMNode(cn,"urn:node:KNB")
#' attributes <- get_all_attributes(url = "https://knb.ecoinformatics.org/#view/doi:10.5063/F1639MWV",
#'                                  node = knb)
#' }
get_all_attributes <- function(eml = NULL, url = NULL, node = NULL) {
    if (is.null(eml) & is.null(url)) {
        stop("One of the arguments 'eml' or 'url' must be specified.")
    }
    if (!is.null(eml)) {
        stopifnot(isS4(eml))
    }

    # If node is specified run checks, otherwise set node = ADC production
    if (!is.null(node)) {
        stopifnot(methods::is(node, "MNode") || is(node, "CNode"))
    } else {
        cn <- dataone::CNode('PROD')
        node <- dataone::getMNode(cn,'urn:node:ARCTIC')
    }

    # If url is specified extract pid and download eml
    if (!is.null(url)) {
        stopifnot(is.character(url))

        pid <- unlist(strsplit(url, "view/"))[[2]]
        eml <- EML::read_eml(rawToChar(dataone::getObject(node, pid)))
    }

    n <- length(eml@dataset@dataTable)
    if (n == 0){
        stop("EML does not contain attribute metadata")
    }

    results <- lapply(eml@dataset@dataTable, function(dataTable) {
        att <- EML::eml_get(dataTable, "attributeList", "entityName")
    })

    names(results) <- lapply(eml@dataset@dataTable, function(dataTable) {
        EML::eml_get(dataTable, "entityName")
    })

    return(results)
}
