##Function to edit an attribute in an existing attributes table.

library(arcticdatautils)
library(dataone)
library(EML)
library(devtools)

cnTest <- dataone::CNode('STAGING')
mnTest <- dataone::getMNode(cnTest,'urn:node:mnTestARCTIC')

#dummy package
pkg <- arcticdatautils::create_dummy_package(mnTest,
                                             size = 2)
#dummy data table:
attributes1 <- data.frame(
    attributeName = c('col1', 'col2', 'col3', 'col4', 'col5'),
    attributeDefinition = c('Numbers', 'Letters in the alphabet', 'Levs', 'IntervalNums', 'SampleDate'),
    measurementScale = c('ratio', 'nominal','ordinal','interval','dateTime'),
    domain = c('numericDomain', 'textDomain', 'enumeratedDomain', 'numericDomain', 'dateTimeDomain'),
    formatString = c(NA,NA,NA,NA,'YYYY-MM-DD'),
    definition = c(NA,'ABCDEFG...',NA,NA,NA),
    unit = c('dimensionless', NA,NA,'dimensionless',NA),
    numberType = c('integer', NA,NA,'whole',NA),
    missingValueCode = c(NA,NA,NA,NA,NA),
    missingValueCodeExplanation = c(NA,NA,NA,NA,NA),
    stringsAsFactors = FALSE)
Levs <- c(A = 'high', B = 'medium', C = 'low')
factors1 <- data.frame(attributeName = 'col3', code = names(Levs), definition = unname(Levs))
factors1
attributeList1 <- set_attributes(attributes1, factors=factors1)
phys <- pid_to_eml_physical(mnTest, pkg$data[1])

dummy_data_table <- new('dataTable',
                        entityName = 'Dummy Data Table',
                        entityDescription = 'Dummy Description',
                        physical = phys,
                        attributeList = attributeList1)

eml <- read_eml(rawToChar(getObject(mnTest, pkg$metadata)))
eml@dataset@dataTable <- c(dummy_data_table)
eml@dataset@dataTable


#Function below includes only required slots.
#Need to add checks to ensure that fields match measurementScale (e.g. if measurementScale = ratio, unit =/= NULL and definiton must be NA)

#User needs to:
#specify the eml object in the argument "eml"
#specify which dataTable (by position) in the argument "dataTableNumber"
#specify position of attribute in attributeList for argument "attributeNumber"
#In cases with large attribute lists, user may want to use which_element function to locate the attribute position.


##Trying out with additional statements for NULL
edit_attribute <- function(eml, dataTableNumber, attributeNumber, attributeName = NULL, attributeDefinition = NULL, domain=NULL,
                           measurementScale = NULL, unit = NULL, numberType = NULL, definition = NULL, formatString = NULL,
                           missingValueCode = NULL, missingValueCodeExplanation = NULL){

    data<-get_attributes(eml@dataset@dataTable[[dataTableNumber]]@attributeList)
    attributeTable<-data.frame(data$attributes) #this excludes the factor table from enumerated domain.

    if(!is.null(attributeName)==TRUE){
        attributeTable[attributeNumber,1] <- attributeName
    }
    else if(is.null(attributeName)==TRUE){
        attributeTable[attributeNumber,1] <- attributeTable[attributeNumber,1]
    }
    if(!is.null(attributeDefinition)==TRUE){
        attributeTable[attributeNumber,17] <- attributeDefinition
    }
    else if(is.null(attributeDefinition)==TRUE){
        attributeTable[attributeNumber,17] <- attributeTable[attributeNumber,17]
    }
    if(!is.null(measurementScale)==TRUE){
        attributeTable[attributeNumber,16] <- measurementScale
    }
    else if(is.null(measurementScale)==TRUE){
        attributeTable[attributeNumber,16] <- attributeTable[attributeNumber,16]
    }
    if(!is.null(domain)==TRUE){
        attributeTable[attributeNumber,2] <- domain
    }
    else if(is.null(domain)==TRUE){
        attributeTable[attributeNumber,2] <- attributeTable[attributeNumber,2]
    }
    if(!is.null(unit)==TRUE){
        attributeTable[attributeNumber,6] <- unit
    }
    else if(is.null(unit)==TRUE){
        attributeTable[attributeNumber,6] <- attributeTable[attributeNumber,6]
    }
    if(!is.null(numberType)==TRUE){
        attributeTable[attributeNumber,7] <- numberType
    }
    else if(is.null(numberType)==TRUE){
        attributeTable[attributeNumber,7] <- attributeTable[attributeNumber,7]
    }
    if(!is.null(definition)==TRUE){
        attributeTable[attributeNumber,9] <- definition
    }
    else if(is.null(definition)==TRUE){
        attributeTable[attributeNumber,9] <- attributeTable[attributeNumber,9]
    }
    if(!is.null(formatString)==TRUE){
        attributeTable[attributeNumber,8] <- formatString
    }
    else if(is.null(formatString)==TRUE){
        attributeTable[attributeNumber,8] <- attributeTable[attributeNumber,8]
    }
    if(!is.null(missingValueCode)==TRUE){
        attributeTable[attributeNumber,14] <- missingValueCode
    }
    else if(is.null(missingValueCode)==TRUE){
        attributeTable[attributeNumber,14] <- attributeTable[attributeNumber,14]
    }
    if(!is.null(missingValueCodeExplanation)==TRUE){
        attributeTable[attributeNumber,15] <- missingValueCodeExplanation
    }
    else if(is.null(missingValueCodeExplanation)==TRUE){
        attributeTable[attributeNumber,15] <- attributeTable[attributeNumber,15]
    }
    return(attributeTable)

}


newAttributeTable<-edit_attribute(eml, 1, 2, attributeDefinition = "I hope this works", domain = "numericDomain",
                                  measurementScale = "ratio", unit = "dimensionless", numberType = "whole")
newAttributeTable

attribute.list<-set_attributes(newAttributeTable)
eml@dataset@dataTable[[1]]@attributeList <- attribute.list
eml@dataset@dataTable

#Fails to set attributes.