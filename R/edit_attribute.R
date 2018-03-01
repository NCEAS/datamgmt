##Script for writing a function to edit an attribute in an existing attributes table.

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


#Function below includes only the required slots.
#Currently works for all measurementTypes, but not functional yet for enumeratedDomain.
#In cases with large attribute lists, user may want to use which_element function to locate the attribute position.

#Need to add checks to ensure that fields match measurementScale (e.g. if mS = ratio, unit =/= NULL)


###############################################################################
##Going to try out a potentially much easier route: load up the attribute table as a dataframe, edit the dataframe, and reset the attributes. Will need checks.
###############################################################################
data<-get_attributes(eml@dataset@dataTable[[1]]@attributeList)
data1<-data.frame(data$attributes) #this excludes the factor table for the enumerated domain in col3.
data1

#specify the dataframe in the argument "attributeList", specify position of attribute in attributeList for argument "attribute"
#need to stick some "if" statements in for optional arguments
edit_attribute <- function(attributeList, attribute, attributeName = NA, attributeDefinition = NULL, domain=NULL,
                           measurementScale = NULL, unit = NULL, numberType = NULL, definition = NULL, formatString = NULL,
                           missingValueCode = NULL, missingValueCodeExplanation = NULL){

    attributeList[attribute,1] <- attributeName
    attributeList[attribute,17] <- attributeDefinition
    attributeList[attribute,16] <- measurementScale
    attributeList[attribute,2] <- domain
    attributeList[attribute,6] <- unit
    attributeList[attribute,7] <- numberType
    attributeList[attribute,9] <- definition
    attributeList[attribute,8] <- formatString
    attributeList[attribute,14] <- missingValueCode
    attributeList[attribute,15] <- missingValueCodeExplanation
    if(!is.null(attributeName)){
        return(attributeList[attribute,1])
    }

    return(attributeList)

}

edit_attribute <- function(attributeList, attribute, attributeName = NA, attributeDefinition = NULL, domain=NULL,
                           measurementScale = NULL, unit = NULL, numberType = NULL, definition = NULL, formatString = NULL,
                           missingValueCode = NULL, missingValueCodeExplanation = NULL){

    if(!is.null(attributeName)){
        attributeList[attribute,1] <- attributeName
    } else
        {return(attributeList[attribute,1])}
    if(!is.null(attributeDefinition)){
        attributeList[attribute,17] <- attributeDefinition
    }
    if(!is.null(measurementScale)){
        attributeList[attribute,16] <- measurementScale
    }
    if(!is.null(domain)){
        attributeList[attribute,2] <- domain
    }
    if(!is.null(unit)){
        attributeList[attribute,6] <- unit
    }
    if(!is.null(numberType)){
        attributeList[attribute,7] <- numberType
    }
    if(!is.null(definition)){
        attributeList[attribute,9] <- definition
    }
    if(!is.null(formatString)){
        attributeList[attribute,8] <- formatString
    }
    if(!is.null(formatString)){
        attributeList[attribute,14] <- missingValueCode
    }
    if(!is.null(formatString)){
        attributeList[attribute,15] <- missingValueCodeExplanation
    }

    return(attributeList)

}

#testing this out on changing attribute 2 ("col2", a nominal attribute) to ratio
test1<-edit_attribute(data1, 2, attributeDefinition = "I hope this works", domain = "numericDomain",
                      measurementScale = "ratio", unit = "dimensionless", numberType = "whole")

test1



