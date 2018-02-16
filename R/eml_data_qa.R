# Purpose: QA script to check that attributes match values in the data
#   The first iteration of this script will scrape all remote packages on various servers (get pids via solr query) KNB, ArcticData, etc
#   Output will be written to a file and should include resource map pid, pids of problem datasets, and the exact issues
#   Second iteration will be built to take in local data and eml (before it is pushed to KNB)
#
# Functions:
#   Names: Check that all column names in attributes match the column names in the csv
#       Possible conditions to account for:
#           - dataTable does not exist for a csv
#           - Physical has not been set and so URL id in dataTable is incorrect
#           - Some of the attributes that exist in the data don't exist in the attribute table
#           - Some of the attributes that exist in the attribute table don't exist in the data
#           - There is a typo in one of the attributes or column names so they don't match (maybe covered by above)
#   Domains: Check that all attribute types match attribute types in the csv
#       Possible conditions to account for:
#           - nominal, ordinal, integer, ratio, dateTime
#           - If domain is enumerated domain, not all enumerated values in the data are accounted for in the enumarated definition
#           - If domain is enumerated domain, not all enumerated values in the enumerated definition are actually represented in the data
#           - Type of data does not match type
#   Values: Check for accidental characters in the csv (one char in a column of ints)

library(arcticdatautils)
library(dataone)
library(EML)

# TEST
cn <- dataone::CNode('STAGING')
node <- dataone::getMNode(cn,'urn:node:mnTestARCTIC')
pid <- "resource_map_urn:uuid:8f0ba0d5-6f39-4bce-8c8c-0d5e85a9036d"

#' Create EML otherEntity objects for a set of PIDs
#'
#' Note this is a wrapper around sysmeta_to_other_entity which handles the task of
#' creating the EML otherEntity.
#'
#' @param mn (MNode) Member Node where the PID is associated with an object.
#' @param pids (character) The PID of a resource map to do QA on a package.
#' @param readData (logical) Default False. If True, pull all data from remote and check that column types match attributes. Only applicable to public packages (private packages will read complete dataset).
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # For a package, run QA checks
#' pid_to_other_entity(mn, pid, readData = TRUE)
#' }
qa_attributes <- function(node, pid, readData = FALSE) {
    stopifnot(class(node) %in% c("MNode", "CNode"))
    stopifnot(is.character(pid), nchar(pid) > 0)

    supported_formats <- c("text/csv",
                           "text/tsv",
                           "text/plain",
                           "application/vnd.ms-excel",
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")

    versions <- get_all_versions(node, pid)

    if(pid != versions[length(versions)]) {
        pid <- versions[length(versions)]
    }

    package <- get_package(node, pid)
    eml_raw <- rawToChar(getObject(node, package$metadata))
    eml <- read_eml(eml_raw)

    # TEST
    #objectpid <- package$data[[2]]

    urls <- sapply(eml@dataset@dataTable, function(data){ data@physical[[1]]@distribution[[1]]@online@url@.Data})

    for(objectpid in package$data)
    {
        # Does the given pid have an associated datatable
        sysmeta <- getSystemMetadata(node, objectpid)

        # If object is not tabular data, continue
        format <- sysmeta@formatId
        if(!format %in% supported_formats) next

        # If object is tabular data, should also have a dataTable, which is matched by the url
        i <- which(grepl(objectpid, urls))

        if (length(i) == 0) {
            print(paste0("The are no matching dataTables for object ", pid, " with format ", format))
            next
        }

        # If package is public, we can read directly from the csv, otherwise we use data one have to get all the data
        isPublic <- TRUE # TODO: read this from sysmeta@accessPolicy - not sure exactly what we need to check for here

        rowsToRead <- -1
        if (readData == FALSE) {
            rowsToRead <- 2
        }

        # TEST
        urls[i] <- paste0("https://test.arcticdata.io/metacat/d1/mn/v2/object/", objectpid)

        if (isPublic == TRUE) {
            x <- tryCatch({
                if (format == "text/csv") {
                    read.csv(urls[i], nrows=rowsToRead)
                } else if (format == "text/tsv") {
                    read.delim(urls[i], nrows=rowsToRead)
                } else if (format == "text/plain") {
                    fread(urls[i], nrows=rowsToRead)
                } else if (format == "application/vnd.ms-excel") {
                    readxl:::read_xls(urls[i], nrows=rowsToRead)
                } else if (format == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
                    readxl:::read_xlsx(urls[i], nrows=rowsToRead)
                }
            },
            error = function(e) {
                print(paste0("Failed to read file ", urls[i]))
            })
        } else {
            # instead, read data from DataOne
            data <- read.csv(textConnection(rawToChar(getObject(node, objectpid))), stringsAsFactors=FALSE)
        }

        dataTable1 <- eml@dataset@dataTable[[i]]
        attributeList1 <- dataTable1@attributeList

        dataCols <- colnames(data)
        attributeNames <- sapply(attributeList1@attribute, function(a){a@attributeName})

        # Check for attribute correctness according to the EML schema using arcticdatautils::eml_validate_attributes
        attOutput <- capture.output(eml_validate_attributes(attributeList1))
        attErrors <- which(grepl('FALSE', head(attOutput, -2)))
        if (length(attErrors) > 0) {
            print(attOutput[attErrors])
        }

        # Check that attribute names match column names
        nonmatch <- which(attributeNames != dataCols)
        if (length(nonmatch) > 0) {
            if (length(match(attributeNames, dataCols)) == length(attributeNames)) {
                print(paste0("Attributes in the attribute table match column names but are incorrectly ordered.
                      order name of data columns is ", dataCols, "order name of attributeList columns is ", attributeNames))
            } else if (length(dataCols) > length(attributeNames)) {
                print("Attribute definitions don't exist for some columns in the dataset.")
            }
            print(paste0("The column ", attributeNames[nonmatch], " doesn't have a matching attribute definition"))
        }

        # Check that

    }
    # Make sure these objects have data tables

    data <- package$data

    sysmeta <- lapply(pids, function(pid) { getSystemMetadata(mn, pid) })

    obj <- getObject(mn, pid)
    path <- paste0(DataFolder, pid, ".csv")
    writeBin(object=obj, con=path)
}


verify_metadata_data_linkage <- function(mn, pid)
{
    # This function checks that all data in the package is consistent between the EML and systemMetadata, and
    # checks that all data, metadata, and resource maps have proper rights and access set.
    datamgmt::check_package(mn, pid)
}

    # From eml, figure out which objects are csvs - these are the only that we care about
    urls <- sapply(eml@dataset@dataTable, function(data){ data@physical[[1]]@distribution[[1]]@online@url@.Data})
i <- which(grepl(objectpid, urls))

if (length(i) == 0) {
    next
    # TODO - check if it is in fact a csv and is just missing a physical
}


