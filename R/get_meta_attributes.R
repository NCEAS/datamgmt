#' Get all attribute from an EML document.  Document for the function \code{get_attributes}
#' from the EML(url) Package
#'
#' @param node (MNode/CNode) The Dataone Node that stores the Metadata object, from \url{https://cn.dataone.org/cn/v2/node}
#' @param metadata (S4 / character) Optional. Either the full eml S4 object document or the url of the Dataone Package.
#' The S4 object input is a more reliable method.
#' @return (list) A list of all attribute metadata from the EML in data.frame objects
#'
#' @export
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#  cn <- dataone::CNode('PROD')
#' node <- dataone::getMNode(cn, 'urn:node:ARCTIC')
#' eml <- EML::read_eml(rawToChar(dataone::getObject(node, "doi:10.18739/A23W02")))
#' attributes <- datamgmt::get_attributes_url(metadata = eml)
#'
#' attributes <- datamgmt::get_attributes_url("ADC", "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02")
#'
#' # switch nodes
#' cn <- dataone::CNode('PROD')
#' knb <- dataone::getMNode(cn,"urn:node:KNB")
#' attributes <- get_attributes_url("KNB", "https://knb.ecoinformatics.org/#view/doi:10.5063/F1639MWV")
#' }
get_meta_attributes <- function(node = "ADC", metadata) {
    # TODO - make all TODO's individual functions
    # TODO - load RData objects
    # TODO - write to individual csvs
    # TODO - write to one excel workbook
    # TODO - make sure it works for otherEntities
    # TODO - switch nodes with "ADC", etc.
    stopifnot(any(isS4(metadata), is.character(metadata)))

    switch(node,
           "ADC" = {
               cn <- dataone::CNode('PROD')
               mn <- dataone::getMNode(cn, 'urn:node:ARCTIC')
           })

    # If url input is specified extract pid and download eml
    if (isS4(metadata)) {
        eml <- metadata
    } else {
        pid <- unlist(strsplit(metadata, "view/"))[[2]]
        eml <- EML::read_eml(rawToChar(dataone::getObject(mn, pid)))
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
