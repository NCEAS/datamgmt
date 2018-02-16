##This is my script for writing a function to edit an attribute in an existing attributes table.

library(testthat)
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

pkg #getting PIDs
# $metadata
# [1] "urn:uuid:83965425-2125-4663-bac7-d471fb03fb43"
# $resource_map
# [1] "urn:uuid:3a26404a-a8d0-4a6b-b081-3d6d96b40923"
# $data
# [1] "urn:uuid:9f1a87e5-0c1a-4498-b787-afe40d417be2"

eml@dataset@dataTable@.Data
#Need some way to access the <attributeList> inside this slot: maybe write in the xml text?

#arguments would be listed like so, where x is the attribute of interest
#AttName<-edit_attribute(x, attributeName="",attributeLabel="",attributeDefinition="",measurementScale="",
                            #domain="",formatString="",definition="",unit="",numberType="",
                            #missingValueCode="",missingValueCodeExplanation="")

##Don't use this
# edit_attribute<-function(attributeName="",attributeDefinition="",measurementScale="",
#                          domain="",formatString="",definition="",unit="",numberType="",
#                          missingValueCode="",missingValueCodeExplanation=""){
#     paste0('<attribute>
#              <attributeName>',attributeName,'</attributeName>',
#              '<attributeDefinition>',attributeDefinition,'</attributeDefinition>',
#              '<measurementScale><',measurementScale,'>',
#                 '<unit><standardUnit>',unit,'</standardUnit></unit>',
#                 '<',domain,'>','<numberType>',numberType,'</numberType></',domain,'>',
#              '</measurementScale></attribute>')
# }

#Code below includes only the required slots and does not contain any if statements for failing the function if e.g. the wrong domain goes into a measurement scale.
#x is the data table number in the data table list, y is the attribute number in the attribute list for that data table
edit_attribute<-function(x,y,attributeName,attributeDefinition,measurementScale,
                         domain,formatString,definition,unit,numberType,
                         missingValueCode,missingValueCodeExplanation){
    eml@dataset@dataTable[[x]]@attributeList@attribute[[y]]@attributeName=attributeName
    eml@dataset@dataTable[[x]]@attributeList@attribute[[y]]@attributeDefinition=attributeDefinition
    eml@dataset@dataTable[[x]]@attributeList@attribute[[y]]@measurementScale=measurementScale
    eml@dataset@dataTable[[x]]@attributeList@attribute[[y]]@domain=domain
    eml@dataset@dataTable[[x]]@attributeList@attribute[[y]]@formatString=formatString
    eml@dataset@dataTable[[x]]@attributeList@attribute[[y]]@definition=definition
    eml@dataset@dataTable[[x]]@attributeList@attribute[[y]]@unit=unit
    eml@dataset@dataTable[[x]]@attributeList@attribute[[y]]@numberType=numberType
    eml@dataset@dataTable[[x]]@attributeList@attribute[[y]]@missingValueCode=missingValueCode
    eml@dataset@dataTable[[x]]@attributeList@attribute[[y]]@missingValueCodeExplanation=missingValueCodeExplanation
    }

edit_attribute(1,2,attributeName="TestAtt",attributeDefinition="trying out if function works",measurementScale="nominal",
                       domain="textDomain",formatString=NA,definition="trying out if function works",unit=NA,numberType=NA,
                       missingValueCode=NA,missingValueCodeExplanation=NA)

eml@dataset@dataTable[[1]]@attributeList

#Don't use this stuff below
# eml@dataset@dataTable
# eml@dataset@dataTable[[1]]@attributeList@attribute[[2]]
# 
# NewAtt<-edit_attribute(attributeName="TestAtt",attributeDefinition="trying out if function works",measurementScale="nominal",
#                        domain="textDomain",formatString=NA,definition="trying out if function works",unit=NA,numberType=NA,
#                        missingValueCode=NA,missingValueCodeExplanation=NA)
# eml@dataset@dataTable[[1]]@attributeList@attribute[[2]]<-NewAtt

