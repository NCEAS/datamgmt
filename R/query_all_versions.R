#' Get all Solr fields
#'
#' @description Simple helper function for 'query_all_versions' function to retrive all Solr fields
#'
#' @return (character) List of Solr fields
#'
#' @references Written by Irene in the reference guide at https://github.com/NCEAS/datateam-training/blob/master/workflows/solr_queries/construct_a_query.Rmd
get_solr_fields <- function() {
    adc_solr <- httr::GET("https://arcticdata.io/metacat/d1/mn/v2/query/solr")
    suppressMessages(suppressWarnings(adc_solr <- adc_solr %>%
                                          stringr::str_extract_all("name>.*<") %>%
                                          unlist() %>%
                                          stringr::str_replace_all("name>|<", "")))
    return(adc_solr)
}

#' Create data frame with Solr fields
#'
#' This is a helper function for 'query_all_versions' function. It simplifies solr queries
#' and creates a one row data frame with the specified Solr fields as the columns.
#'
#' @author Sharis Ochs, \email{sharisnochs@@gmail.com}
#'
#' @param node (MNode) Specify the node where the object should be searched for.
#' @param object_pid (character) PID for the object that you want to return information about.
#' @param fields (character) List of fields that you want returned in the data frame. Default of "*"
#' returns all non NULL fields.
#'
#' @return (data.frame) One row data frame with query fields as columns.
#'
query_solr_metadata <- function(node, object_pid, fields = "*") {

    ## Checks =========================
    # Check that node exists
    if (!(methods::is(node, "MNode"))) {
        stop('Please enter a valid node')
    }
    # Check that object_pid is character
    if (!(is.character(object_pid))) {
        stop('object_pid should be of class "character"')
    }
    # Check that fields input is character
    if (!(is.character(fields))) {
        stop('fields should be of class "character"')
    }
    # Check that object exista
    if (!(arcticdatautils::object_exists(node, object_pid))) {
        stop('Object does not exist on specified node')
    }

    # Get all solr fields
    adc_solr <- get_solr_fields()

    # Check that all specified fields are valid
    suppressWarnings(if (fields != "*"){
        indices <- which(!(fields %in% adc_solr))
        if (length(indices)>0){
            if(length(indices) == 1){
                stop(fields[indices], " is not a valid field")
            }
            else {
                stop(paste(fields[indices], collapse=" and "), " are not valid fields")
            }
        }
    })

    fl <- paste(fields, collapse=", ")
    q <- paste0("documents:\"", object_pid, "\"")
    df_query <- dataone::query(node, list(q = q, fl = fl, rows = "5"),
                               as = "data.frame")

    return(df_query)
}

#' Solr query all versions of a PID
#'
#' This function uses a combination of get_all_versions and solr query to return the query fields
#' for all versions of the specified PID. Each row of the resulting data frame corresponds to a version
#' and the columns are the query fields.
#'
#' @author Sharis Ochs, \email{sharisnochs@@gmail.com}
#'
#' @param node (MNode) Specify the node where the object should be searched for.
#' @param object_pid (character) PID for the object that you want to return information about.
#' @param fields (character) List of fields that you want returned in the data frame. Default
#' returns all non NULL fields.
#'
#' @return (data.frame) Data frame with rows for each version of the PID and columns with each query field
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode("PROD")
#' mn <- dataone::getMNode(cn, "urn:node:ARCTIC")
#' df <- query_all_versions(mn, "doi:10.18739/A27D2Q670", c("id", "title", "origin", "submitter"))
#' View(df)
#' }
#' @export
query_all_versions <- function(node, object_pid, fields = "*") {

    ## Checks =========================
    # Check that node exist
    if (!(methods::is(node, "MNode"))) {
        stop('Please enter a valid node ')
    }
    # Check that object_pid is character
    if (!(is.character(object_pid))) {
        stop('object_pid should be of class "character"')
    }
    # Check that fields input is character
    if (!(is.character(fields))) {
        stop('fields should be of class "character"')
    }
    # Check that object exist
    if (!(arcticdatautils::object_exists(node, object_pid))) {
        stop('Object does not exist on specified node')
    }

    # Get all versions
    versions <- arcticdatautils::get_all_versions(node, object_pid)
    n <- length(versions)

    # Initialize list to hold query results
    datalist = list()
    for (i in 1:length(versions)) {
        current <- query_solr_metadata(node = node, object_pid = versions[i], fields = fields)
        datalist[[i]] <- current # Add query result to list
        }

    # Combine list into data frame
    df_query <- dplyr::bind_rows(datalist)

    return(df_query)
}