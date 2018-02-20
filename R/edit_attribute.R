##Script for writing a function to edit an attribute in an existing attributes table.

library(arcticdatautils)
library(dataone)
library(EML)
library(devtools)

cnTest <- dataone::CNode('STAGING')
mnTest <- dataone::getMNode(cnTest,'urn:node:mnTestARCTIC')

#dummy package
pkg <- arcticdatautils::create_dummy_package(mnTest,
                                             size = 5)
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

#checking out slots
sort(slotNames(eml@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale))
eml@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale
eml@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale@ordinal
eml@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale@nominal@nonNumericDomain@textDomain[[1]]@
sort(slotNames(eml@dataset@dataTable[[1]]@attributeList@attribute[[2]]@measurementScale@nominal@nonNumericDomain@textDomain))
sort(slotNames(eml@dataset@dataTable[[1]]@attributeList@attribute[[1]]@measurementScale@ordinal))
sort(slotNames(eml@dataset@dataTable[[1]]@attributeList@attribute[[1]]@missingValueCode[[1]]))
eml@dataset@dataTable[[1]]@attributeList@attribute[[5]]@measurementScale@dateTime
eml@dataset@dataTable[[1]]@attributeList@attribute[[1]]@measurementScale@ratio@unit@standardUnit[[1]]
eml@dataset@dataTable[[1]]@attributeList@attribute[[1]]@measurementScale@ratio

#Function below includes only the required slots.
#Fails if any arguments existing within a certain measurementScale are not specified
#Currently works for all measurementTypes, but not functional yet for enumeratedDomain.
#Not yet functional for custom units.

#dataTable is the data table number in the data table list, attribute is the attribute number in the attribute list for that data table.

edit_attribute <- function(eml, dataTable, attribute, attributeName = NA, attributeDefinition = NA,
                           measurementScale = NA, unit = NA, numberType = NA, definition = NA, formatString = NA,
                           missingValueCode = NA, missingValueCodeExplanation = NA){
    eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@attributeName <- attributeName
    eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@attributeDefinition <- attributeDefinition
    if (measurementScale == 'nominal') {
        eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@measurementScale@nominal@nonNumericDomain@textDomain[[1]]@definition <- definition
    } else if (measurementScale == 'ordinal') {
        eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@measurementScale@ordinal@nonNumericDomain@textDomain[[1]]@definition <- definition
    } else if (measurementScale == 'interval') {
        eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@measurementScale@interval@unit@standardUnit[[1]] <- unit
        eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@measurementScale@interval@numericDomain@numberType[[1]] <- numberType
    } else if (measurementScale == 'ratio') {
        eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@measurementScale@ratio@unit@standardUnit[[1]] <- unit
        eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@measurementScale@ratio@numericDomain@numberType[[1]] <- numberType
    } else if (measurementScale == 'dateTime') {
        eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@measurementScale@dateTime@formatString <- formatString
    }
    eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@missingValueCode[[1]]@code <- missingValueCode
    eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@missingValueCode[[1]]@codeExplanation <- missingValueCodeExplanation

    return(eml)
}

#Tests
eml2 <- edit_attribute(eml, 1, 2, attributeName = 'TestNominal', attributeDefinition = 'trying out if function works', measurementScale = 'nominal',
               definition = 'trying out if function works', missingValueCode = 'NA', missingValueCodeExplanation = 'data not available')

eml3 <- edit_attribute(eml, 1, 1, attributeName = 'TestRatio', attributeDefinition = 'trying out if function works', measurementScale = 'ratio',
                     unit = 'dimensionless', numberType = 'integer', missingValueCode = 'NA', missingValueCodeExplanation = 'data not available')

eml4 <- edit_attribute(eml, 1, 4, attributeName = 'TestInterval', attributeDefinition = 'trying out if function works', measurementScale = 'interval',
                     unit = 'dimensionless', numberType = 'whole', missingValueCode = 'NA', missingValueCodeExplanation = 'data not available')

eml5 <- edit_attribute(eml, 1, 5, attributeName = 'TestDate', attributeDefinition = 'trying out if function works', measurementScale = 'dateTime',
                     formatString = 'YYYY-MM', missingValueCode = 'NA', missingValueCodeExplanation = 'data not available')

eml6 <- edit_attribute(eml, 1, 3, attributeName = 'TestOrdinal', attributeDefinition = 'trying out if function works', measurementScale = 'ordinal',
                     definition = 'trying out if function works', missingValueCode = 'NA', missingValueCodeExplanation = 'data not available')

eml2@dataset@dataTable[[1]]
eml3@dataset@dataTable[[1]]
eml4@dataset@dataTable[[1]]
eml5@dataset@dataTable[[1]]
eml6@dataset@dataTable[[1]]
