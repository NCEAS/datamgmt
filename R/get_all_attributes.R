#' Get all attribute from an EML document.  Document for the function \code{get_attributes}
#' from the EML(LINK) Package
#'
#' @param eml (S4) Optional. The full eml document. Required if \code{link} is not specified.
#' @param link (character) Optional. The link to the Dataone Package. Required if \code{eml} is not specified.
#' @param node (MNode/CNode) Optional.  Defaults to Arctic Data Center Production node.
#' @return (list) A list of all attribute metadata from the EML in data.frame objects
#'
#' @export
#'
#' @author Dominic Mullen
#'
#' @examples
#' \dontrun{
#' eml_pid <- "doi:10.18739/A23W02"
#' eml <- read_eml(rawToChar(getObject(mn, eml_pid)))
#' }
get_all_attributes <- function(eml = NULL, link = NULL, node = NULL) {
    if (is.null(eml) & is.null(link)) {
        stop("One of the arguments 'eml' or 'link' must be specified.")
    }
    if (!is.null(eml)) {
        stopifnot(isS4(eml))
    }

    # If node is specified run checks, otherwise set node = ADC production
    if (!is.null(node)) {
        stopifnot(methods::is(node, "MNode") || is(node, "CNode"))
    } else {
        cn <- dataone::CNode('PROD')
        mn <- dataone::getMNode(cn,'urn:node:ARCTIC')
    }

    # If link is specified extract pid and download eml
    if (!is.null(link)) {
        stopifnot(is.character(link))

        pid <- unlist(strsplit(link, "view/"))[[2]]
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