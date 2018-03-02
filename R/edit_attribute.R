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
#Currently works for all measurementTypes, but not functional yet for enumeratedDomain.
#Need to add in lines to reset the attributes table after editing.
#Need to add checks to ensure that fields match measurementScale (e.g. if measurementScale = ratio, unit =/= NULL and definiton must be NA)

data<-get_attributes(eml@dataset@dataTable[[1]]@attributeList)
data1<-data.frame(data$attributes) #this excludes the factor table for the enumerated domain in col3!!
data1

#User needs to:
#specify the eml object in the argument "eml"
#specify which dataTable (by position) in the argument "dataTableNumber"
#specify position of attribute in attributeList for argument "attributeNumber"
#In cases with large attribute lists, user may want to use which_element function to locate the attribute position.

edit_attribute <- function(eml, dataTableNumber, attributeNumber, attributeName = NULL, attributeDefinition = NULL, domain=NULL,
                           measurementScale = NULL, unit = NULL, numberType = NULL, definition = NULL, formatString = NULL,
                           missingValueCode = NULL, missingValueCodeExplanation = NULL){

    data<-get_attributes(eml@dataset@dataTable[[dataTableNumber]]@attributeList)
    attributeTable<-data.frame(data$attributes)

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
    if(!is.null(formatString)==TRUE){
        attributeTable[attributeNumber,14] <- missingValueCode
    }
    if(!is.null(formatString)==TRUE){
        attributeTable[attributeNumber,15] <- missingValueCodeExplanation
    }
    return(attributeTable)

}

#testing this out on changing attribute 2 ("col2", a nominal attribute) to ratio
newAttributeTable<-edit_attribute(eml, 1, 2, attributeDefinition = "I hope this works", domain = "numericDomain",
                      measurementScale = "ratio", unit = "dimensionless", numberType = "whole")

newAttributeTable

#Function works for changing the attributes in the dataframe (minus enumerated domain factor table...).
#Failure when I try to reset the attributes using set_attributes (below).

attribute.list<-set_attributes(newAttributeTable)
eml@dataset@dataTable[[1]]@attributeList <- attribute.list
eml@dataset@dataTable