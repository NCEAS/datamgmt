pkgname <- "datamgmt"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('datamgmt')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_creator_id")
### * add_creator_id

flush(stderr()); flush(stdout())

### Name: add_creator_id
### Title: add_creator_id
### Aliases: add_creator_id
### Keywords: creator eml id orcid

### ** Examples

library(dataone)
library(arcticdatautils)
library(EML)

cnTest <- dataone::CNode('STAGING')
mnTest <- dataone::getMNode(cnTest,'urn:node:mnTestARCTIC')
eml_pid <- arcticdatautils::create_dummy_metadata(mnTest)
eml1 <- EML::read_eml(rawToChar(getObject(mnTest, eml_pid)))
add_creator_id(eml1, orcid = "https://orcid.org/WWWW-XXXX-YYYY-ZZZZ")



cleanEx()
nameEx("clone_package")
### * clone_package

flush(stderr()); flush(stdout())

### Name: clone_package
### Title: Clone a Data Package
### Aliases: clone_package

### ** Examples

## Not run: 
##D cn_pull <- CNode("PROD")
##D mn_pull <- getMNode(cn_pull, "urn:node:ARCTIC")
##D cn_push <- CNode('STAGING')
##D mn_push <- getMNode(cn_push,'urn:node:mnTestARCTIC')
##D clone_package(mn_pull, mn_push, "resource_map_doi:10.18739/A2RZ6X")
## End(Not run)




cleanEx()
nameEx("create_attributes_table")
### * create_attributes_table

flush(stderr()); flush(stdout())

### Name: create_attributes_table
### Title: Allows editing of an attribute table and custom units table in a
###   shiny environment
### Aliases: create_attributes_table

### ** Examples

create_attributes_table(NULL, NULL)

data <- read.csv("Test.csv")
create_attributes_table(data, NULL)

attributes_table <- EML::get_attributes(eml@dataset@dataTable[[i]]@attributeList)$attributes
create_attributes_table(NULL, attributes_table)

create_attributes_table(data, attributes_table)



cleanEx()
nameEx("data_objects_exist")
### * data_objects_exist

flush(stderr()); flush(stdout())

### Name: data_objects_exist
### Title: Check if data objects exist in a list of Data Packages.
### Aliases: data_objects_exist

### ** Examples

## Not run: 
##D cn <- CNode("PROD")
##D mn <- getMNode(cn, "urn:node:ARCTIC")
##D data_objects_exist(mn,
##D c("doi:10.5065/D60P0X4S", "urn:uuid:3ea5629f-a10e-47eb-b5ce-de10f8ef325b"))
## End(Not run)




cleanEx()
nameEx("get_numberType")
### * get_numberType

flush(stderr()); flush(stdout())

### Name: get_numberType
### Title: Returns the numberType (either "real", "integer", "whole", or
###   "natural") of input values
### Aliases: get_numberType

### ** Examples

# To get numberType for each column in a data.frame \code{df}:
unlist(lapply(df, function(x) get_numberType(x)))



cleanEx()
nameEx("hello")
### * hello

flush(stderr()); flush(stdout())

### Name: hello
### Title: Hello, World!
### Aliases: hello

### ** Examples

hello()



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
