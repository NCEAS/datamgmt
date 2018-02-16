##This is my script for writing a function to edit an attribute in an existing attributes table.

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
    attributeName = c('col1', 'col2'),
    attributeDefinition = c('Numbers', 'Letters in the alphabet'),
    measurementScale = c('ratio', 'nominal'),
    domain = c('numericDomain', 'textDomain'),
    formatString = c(NA,NA),
    definition = c(NA,'ABCDEFG...'),
    unit = c('dimensionless', NA),
    numberType = c('integer', NA),
    missingValueCode = c(NA, NA),
    missingValueCodeExplanation = c(NA, NA),
    stringsAsFactors = FALSE)

attributeList1 <- set_attributes(attributes1)
phys <- pid_to_eml_physical(mnTest, pkg$data[1])

dummy_data_table <- new('dataTable',
                        entityName = 'Dummy Data Table',
                        entityDescription = 'Dummy Description',
                        physical = phys,
                        attributeList = attributeList1)

eml <- read_eml(rawToChar(getObject(mnTest, pkg$metadata)))
eml@dataset@dataTable <- c(dummy_data_table)

pkg

#Code below includes only the required slots and does not contain any if statements for failing the function if e.g. the wrong domain goes into a measurement scale.
#This is being tested on a nominal text attribute and is presently only being built to work for this type. Will expand once this is successful.
#dataTable is the data table number in the data table list, attribute is the attribute number in the attribute list for that data table.
#domain_type must be nonNumericDomain or blank?
edit_attribute <- function(eml, dataTable, attribute, attributeName, attributeDefinition, measurementScale, domain_type, domain, definition,
                           missingValueCode, missingValueCodeExplanation){
    eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@attributeName <- attributeName
    eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@attributeDefinition <- attributeDefinition
    eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@measurementScale <-
        paste0("<",measurementScale,"><",domain_type,"><",domain,"><definition>",definition,"</definition></",domain,"></",domain_type,"></",measurementScale,">") ###lots of if statements will end up here
    eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@domain <- domain
    eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@definition <- definition
    eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@missingValueCode <- missingValueCode
    eml@dataset@dataTable[[dataTable]]@attributeList@attribute[[attribute]]@missingValueCodeExplanation <- missingValueCodeExplanation
}

edit_attribute(eml,1, 2, attributeName="TestAtt", attributeDefinition="trying out if function works", measurementScale="nominal",
               domain_type="nonNumericDomain", domain="textDomain", definition="trying out if function works",
               missingValueCode=NA, missingValueCodeExplanation=NA)


eml@dataset@dataTable[[1]]


