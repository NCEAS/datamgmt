#' Get all attribute from an EML document.  Document for the function \code{get_attributes}
#' from the EML(url) Package
#'
#'@param node (MNode/CNode) The Dataone Node that stores the Metadata object.
#' @param eml (S4 / character) Optional. Either the full eml S4 object document or the url of the Dataone Package.
#' The S4 object input is a more reliable method.
#' @return (list) A list of all attribute metadata from the EML in data.frame objects
#'
#' @export
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' devtools::install_github("ropensci/EML")
#' library(EML)
#' library(dataone)
#' cn <- dataone::CNode('PROD')
#' node <- dataone::getMNode(cn,'urn:node:ARCTIC')
#' eml <- EML::read_eml(rawToChar(dataone::getObject(node, "doi:10.18739/A23W02")))
#' attributes <- datamgmt::get_attributes_url(eml)
#'
#' attributes <- datamgmt::get_attributes_url(node, "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02")
#'
#' # switch nodes
#' cn <- dataone::CNode('PROD')
#' knb <- dataone::getMNode(cn,"urn:node:KNB")
#' attributes <- get_attributes_url(node = knb,
#'                                  url = "https://knb.ecoinformatics.org/#view/doi:10.5063/F1639MWV")
#' }
get_attributes_url <- function(node, eml) {
    # TODO - load RData objects
    # TODO - write to individual csvs
    # TODO - write to one excel workbook
    # TODO - make sure it works for otherEntities
    stopifnot(methods::is(node, "MNode") || is(node, "CNode"))
    stopifnot(any(isS4(eml), is.character(eml)))

    # If url input is specified extract pid and download eml
    if (is.character(eml)) {
        pid <- unlist(strsplit(eml, "view/"))[[2]]
        eml <- EML::read_eml(rawToChar(dataone::getObject(node, pid)))
    }

    n1 <- length(eml@dataset@dataTable)
    n2 <- length(eml@dataset@otherEntity)
    if (n1 == 0 & n2 == 0){
        stop("EML does not contain attribute metadata")
    }

    results <- EML::eml_get(eml, "attributeList")
    names(results) <- EML::eml_get(eml, "entityName")

    return(results)
}
