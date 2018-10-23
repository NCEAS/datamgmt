#' Get all Solr fields
#'
#' Simple helper function for [query_all_versions()] to retrieve all Solr fields.
#'
#' @return (character) A set of Solr fields.
#'
#' @importFrom stringr str_extract_all str_replace_all
#'
#' @noRd
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
#' This is a helper function for [query_all_versions()]. It simplifies Solr queries
#' and creates a one row data frame with the specified Solr fields as the columns.
#'
#' @author Sharis Ochs, \email{sharisnochs@@gmail.com}
#'
#' @param mn (MNode) The Member Node where the object should be searched for.
#' @param object_pid (character) PID for the object that you want to return information about.
#' @param fields (character) List of fields that you want returned in the data frame. Default of "*"
#'   returns all non NULL fields.
#'
#' @return (data.frame) One row data frame with query fields as columns.
#'
#' @noRd
query_solr_metadata <- function(mn, object_pid, fields = "*") {

    ## Checks =========================
    # Check that mn exists
    if (!(methods::is(mn, "MNode"))) {
        stop('Please enter a valid member node')
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
    if (!(arcticdatautils::object_exists(mn, object_pid))) {
        stop("Object does not exist on specified member node")
    }

    # Get all Solr fields
    adc_solr <- get_solr_fields()

    # Check that all specified fields are valid
    suppressWarnings(
        if (fields != "*") {
            indices <- which(!(fields %in% adc_solr))
            if (length(indices) > 0) {
                stop("Invalid solr fields: ", paste(fields[indices], collapse = ", "), call. = FALSE)
            }
        })

    fl <- paste(fields, collapse=", ")
    q <- paste0("identifier:\"", object_pid, "\"")
    df_query <- dataone::query(mn, list(q = q, fl = fl, rows = "5"),
                               as = "data.frame")

    return(df_query)
}


#' Solr query all versions of a PID
#'
#' This function uses a combination of [arcticdatautils::get_all_versions()] and a Solr query to return the query fields
#' for all versions of the specified PID. Each row of the resulting data frame corresponds to a version
#' and the columns are the query fields.
#'
#' @author Sharis Ochs, \email{sharisnochs@@gmail.com}
#'
#' @param mn (MNode) The Member Node where the object should be searched for.
#' @param object_pid (character) PID for the object that you want to return information about.
#' @param fields (character) List of fields that you want returned in the data frame. Default
#'   returns all non `NULL` fields.
#'
#' @return (data.frame) Data frame with rows for each version of the PID and columns with each query field.
#'
#' @import arcticdatautils
#' @importFrom dplyr bind_rows
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode("PROD")
#' mn <- dataone::getMNode(cn, "urn:node:ARCTIC")
#'
#' df <- query_all_versions(mn, "doi:10.18739/A27D2Q670", c("id", "title", "origin", "submitter"))
#' View(df)
#' }
query_all_versions <- function(mn, object_pid, fields = "*") {
    ## Checks =========================
    if (!arcticdatautils::is_token_set(mn)) {
        stop("Token is not set. Please set a token to query private versions of pids.")
    }
    # Check that mn exists
    if (!(methods::is(mn, "MNode"))) {
        stop("Please enter a valid member node")
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
    if (!(arcticdatautils::object_exists(mn, object_pid))) {
        stop("Object does not exist on specified member node")
    }

    # Get all versions and initialize results list
    versions <- arcticdatautils::get_all_versions(mn, object_pid)
    results <- vector("list", length(versions))

    for (i in seq_along(versions)) {
        current <- query_solr_metadata(mn, versions[i], fields)
        results[[i]] <- current # Add query result to list
        }

    # Combine list into data frame
    df_query <- dplyr::bind_rows(results)

    return(df_query)
}
