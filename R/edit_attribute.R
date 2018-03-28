#' Edit a single attribute.
#'
#' This function edits the slots of a single attribute in an existing attribute table.
#'
#' Can only be used on attributes entirely defined within the "attributes" slot of attributeList; it cannot be used to edit the factor table of an enumeratedDomain.
#' In cases with very large attribute lists, user may want to use \link[datamgmt]{which_in_eml} function first to locate the attribute position.
#'
#' @param eml The eml object containing the attributeList
#' @param dataTableNumber The number of the dataTable containing the attributeList; in other words its position in the dataTable list.
#' @param attributeNumber The number of the attribute of interest; in other words its position in the attributeList.
#' @param attributeName The new name to give to the attribute
#' @param attributeDefinition The new attributeDefinition to give to the attribute
#' @param domain The new domain to give to the attribute
#' @param measurementScale The new measurementScale to give to the attribute
#' @param unit The new unit to give to the attribute
#' @param numberType The new numberType to give to the attribute
#' @param definition The new definition (for textDomain) to give to the attribute
#' @param formatString The new formatString to give to the attribute
#' @param missingValueCode The new missing value code to give to the attribute
#' @param missingValueCodeExplanation The new missing value code explaination to give to the attribute
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #To change an attribute's name and add a missing value code where there hadn't been one:
#' new_eml <- edit_attribute(eml, 2, 8, attributeName = "new name", missingValueCode = "NA",
#'                           missingValueCodeExplanation = "Data unavailable")
#' #To change an attribute's attributeDefinition, as well as change measurementScale from nominal to ratio
#' #(requires also changing domain, unit, and numberType, as well as setting definition to NA):
#' new_eml <- edit_attribute(eml, 1, 2, attributeDefinition = "new definition", domain = "numericDomain",
#' measurementScale = "ratio", unit = "dimensionless", numberType = "whole", definition = NA)
#' }
#'
#'
edit_attribute <- function(eml, dataTableNumber, attributeNumber, attributeName = NULL, attributeDefinition = NULL, domain=NULL,
                           measurementScale = NULL, unit = NULL, numberType = NULL, definition = NULL, formatString = NULL,
                           missingValueCode = NULL, missingValueCodeExplanation = NULL){

    data <- EML::get_attributes(eml@dataset@dataTable[[dataTableNumber]]@attributeList)
    attributeTable <- data.frame(data$attributes) #this excludes the factor table from enumerated domain.

    if(!is.null(attributeName)) {
        attributeTable$attributeName[attributeNumber] <- attributeName
    }
    if(!is.null(attributeDefinition)) {
        attributeTable$attributeDefinition[attributeNumber] <- attributeDefinition
    }
    if(!is.null(measurementScale)) {
        attributeTable$measurementScale[attributeNumber] <- measurementScale
    }
    if(!is.null(domain)) {
        attributeTable$domain[attributeNumber] <- domain
    }
    if(!is.null(unit)) {
        attributeTable$unit[attributeNumber] <- unit
    }
    if(!is.null(numberType)) {
        attributeTable$numberType[attributeNumber] <- numberType
    }
    if(!is.null(definition)) {
        attributeTable$definition[attributeNumber] <- definition
    }
    if(!is.null(formatString)) {
        attributeTable$formatString[attributeNumber] <- formatString
    }
    if(!is.null(missingValueCode)) {
        attributeTable$missingValueCode[attributeNumber] <- missingValueCode
    }
    if(!is.null(missingValueCodeExplanation)) {
        attributeTable$missingValueCodeExplanation[attributeNumber] <- missingValueCodeExplanation
    }

    attribute_list <- EML::set_attributes(attributeTable, factors = data$factors)
    eml@dataset@dataTable[[dataTableNumber]]@attributeList <- attribute_list
    return(eml)
    EML:::check_and_complete_attributes(attributeTable, NULL)

}
