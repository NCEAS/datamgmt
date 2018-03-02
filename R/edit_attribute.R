##Function to edit an attribute in an existing attributes table.

# for now:
devtools::install_github("ropensci/EML", force = TRUE)

#cnTest <- dataone::CNode('STAGING')
#mnTest <- dataone::getMNode(cnTest,'urn:node:mnTestARCTIC')

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
attributeList1 <- EML::set_attributes(attributes1, factors=factors1)
phys <- arcticdatautils::pid_to_eml_physical(mnTest, pkg$data[1])

dummy_data_table <- new('dataTable',
                        entityName = 'Dummy Data Table',
                        entityDescription = 'Dummy Description',
                        physical = phys,
                        attributeList = attributeList1)

eml <- EML::read_eml(rawToChar(getObject(mnTest, pkg$metadata)))
eml@dataset@dataTable <- c(dummy_data_table)
eml@dataset@dataTable


#Function below includes only required slots.
#Need to add checks to ensure that fields match measurementScale (e.g. if measurementScale = ratio, unit =/= NULL and definiton must be NA)
#Also does not currently include a way to change the codes in the factor table for an enumerated domain; may need to be its own function?

#User needs to:
###specify the eml object in the argument "eml"
###specify which dataTable (by position) in the argument "dataTableNumber"
###specify position of attribute in attributeList for argument "attributeNumber"
###In cases with large attribute lists, user may want to use which_element function to locate the attribute position.


##Trying out with additional statements for NULL
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

    attribute.list<-set_attributes(attributeTable, factors = data$factors)
    eml@dataset@dataTable[[dataTableNumber]]@attributeList <- attribute.list
    return(eml)

}


eml.test<-edit_attribute(eml, 1, 2, attributeDefinition = "I hope this works", domain = "numericDomain",
                                  measurementScale = "ratio", unit = "dimensionless", numberType = "whole", definition = 'NA')
eml.test
