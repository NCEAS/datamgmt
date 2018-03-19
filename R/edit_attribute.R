#'Edit a single attribute.
#'
#'This function edits the slots of a single attribute in an existing attribute table.
#'
#'Can only be used on attributes entirely defined within the "attributes" slot of attributeList; it cannot be used to edit the factor table of an enumeratedDomain.
#'In cases with very large attribute lists, user may want to use which_in_eml function first to locate the attribute position.
#'
#'@param eml The eml object containing the attributeList
#'@param dataTableNumber The number of the dataTable containing the attributeList; in other words its position in the dataTable list.
#'@param attributeNumber The number of the attribute of interest; in other words its position in the attributeList.
#'@param attributeName... Arguments for specifying new values of attribute slots.
#'
#'@examples
#' \dontrun{
#' #To change an attribute's name and add a missing value code where there hadn't been one:
#' new_eml <- edit_attribute(eml, 2, 8, attributeName = "new name", missingValueCode = "NA", missingValueCodeExplanation = "Data unavailable")
#' #To change an attribute's attributeDefinition, as well as change measurementScale from nominal to ratio (requires also changing domain, unit, and numberType, as well as setting definition to NA):
#' new_eml <- edit_attribute(eml, 1, 2, attributeDefinition = "new definition", domain = "numericDomain",
#' measurementScale = "ratio", unit = "dimensionless", numberType = "whole", definition = NA)
#' }
#'
#'@export


devtools::install_github("ropensci/EML", force = TRUE)

edit_attribute <- function(eml, dataTableNumber, attributeNumber, attributeName = NULL, attributeDefinition = NULL, domain=NULL,
                           measurementScale = NULL, unit = NULL, numberType = NULL, definition = NULL, formatString = NULL,
                           missingValueCode = NULL, missingValueCodeExplanation = NULL){

    data<-EML::get_attributes(eml@dataset@dataTable[[dataTableNumber]]@attributeList)
    attributeTable<-data.frame(data$attributes) #this excludes the factor table from enumerated domain.

    if(!is.null(attributeName)==TRUE){
        attributeTable[attributeNumber,1] <- attributeName
    }
    if(!is.null(attributeDefinition)==TRUE){
        attributeTable[attributeNumber,17] <- attributeDefinition
    }
    if(!is.null(measurementScale)==TRUE){
        attributeTable[attributeNumber,16] <- measurementScale
    }
    if(!is.null(domain)==TRUE){
        attributeTable[attributeNumber,2] <- domain
    }
    if(!is.null(unit)==TRUE){
        attributeTable[attributeNumber,6] <- unit
    }
    if(!is.null(numberType)==TRUE){
        attributeTable[attributeNumber,7] <- numberType
    }
    if(!is.null(definition)==TRUE){
        attributeTable[attributeNumber,9] <- definition
    }
    if(!is.null(formatString)==TRUE){
        attributeTable[attributeNumber,8] <- formatString
    }
    if(!is.null(missingValueCode)==TRUE){
        attributeTable[attributeNumber,14] <- missingValueCode
    }
    if(!is.null(missingValueCodeExplanation)==TRUE){
        attributeTable[attributeNumber,15] <- missingValueCodeExplanation
    }

    attribute.list<-EML::set_attributes(attributeTable, factors = data$factors)
    eml@dataset@dataTable[[dataTableNumber]]@attributeList <- attribute.list
    return(eml)
    EML:::check_and_complete_attributes(attributeTable, NULL)

}
