#' Get NSF Arctic/Polar program award information
#'
#' Uses the \href{https://www.research.gov/common/webapi/awardapisearch-v1.htm}{NSF API}
#' to get all records pertaining to the Arctic or Polar programs.
#'
#' @param from_date (character) Optional. Returns all
#' records with start date after specified date.
#' Format = \emph{mm/dd/yyyy}
#' @param to_date (character) Optional. Returns all
#' records with start date before specified date.
#' Format = \emph{mm/dd/yyyy}
#' @param query (character) Optional. By default, the function
#' searches for all awards with either "polar" or "arctic" in
#' the fundProgramName. Additional queries can be specified
#' as defined in the \href{https://www.research.gov/common/webapi/awardapisearch-v1.htm}{NSF API}.
#' Use '&' to join multiple queries (i.e., \emph{keyword=water&agency=NASA})
#' @param print_fields (character) Optional. By default, the
#' following fields will be returned: id, date,
#' startDate, expDate, fundProgramName, poName,
#' title, awardee, piFirstName, piLastName, piPhone, piEmail.
#' Additional field names can be found in the printFields description
#' of the \href{https://www.research.gov/common/webapi/awardapisearch-v1.htm}{NSF API}.
#'
#' @import XML
#' @import stringr
#' @import RCurl
#'
#' @export
#'
#' @author Irene Steves
#'
#' @examples
#' \dontrun{
#' all_awards <- get_awards()
#' new_awards <- get_awards(from_date = "01/01/2017")
#' }

get_awards <- function(from_date = NULL,
                       to_date = NULL,
                       query = NULL,
                       print_fields = NULL) {
    # TODO use additional_fields instead of print_fields so that user can specify additional fields
    # without having to write out all default fields
  
    # TODO split iteration part of function (repeat...) and arctic/polar customization to make 2 functions:
    # 1 - a generalized get_awards(query_URL)
    # 2 - a wrapper for get_polar_awards() for typical use-cases within ADC/NCEAS

    # basic argument checks
    stopifnot(is.character(from_date) | is.null(from_date))
    stopifnot(is.character(to_date) | is.null(to_date))
    stopifnot(is.character(query) | is.null(query))
    stopifnot(is.character(print_fields) | is.null(print_fields))

    base_url <- "https://api.nsf.gov/services/v1/awards.xml?fundProgramName=ARCTIC|fundProgramName=POLAR"
    if(!is.null(query)) {
        query <- paste0("&", query)
    }

    if(is.null(print_fields)) {
        print_fields <- "id,date,startDate,expDate,fundProgramName,poName,title,awardee,piFirstName,piLastName,piPhone,piEmail"
    }

    query_url <- paste0(base_url, query,
                        "&printFields=", print_fields)

    if(!is.null(from_date)) {
        if(!stringr::str_detect(from_date, "\\d\\d/\\d\\d/\\d\\d\\d\\d")) {
            stop("The from_date is not in the format 'mm/dd/yyyy'.")
        } else {
            query_url <- paste0(query_url, "&startDateStart=", from_date)
        }
    }

    if(!is.null(to_date)) {
        if(!stringr::str_detect(to_date, "\\d\\d/\\d\\d/\\d\\d\\d\\d")) {
            stop("The to_date is not in the format 'mm/dd/yyyy'.")
        } else {
            query_url <- paste0(query_url, "&startDateEnd=", to_date)
        }
    }

    xml1 <- RCurl::getURL(paste0(query_url, "&offset=", 1))
    if(stringr::str_detect(xml1, "ERROR")){
        stop("The query parameters are invalid.")
    }
    xml_df1 <- XML::xmlToDataFrame(xml1)

    # since we can only download 25 entries at a time, we need to loop through the query using different offsets
    n <- 1
    repeat {
        start <- 1 + 25 * n
        xml <- RCurl::getURL(paste0(query_url, "&offset=", start))
        xml_df <- XML::xmlToDataFrame(xml)
        if (length(xml_df) == 0) {break}

        #check column names, add in missing one
        missing <- colnames(xml_df1)[!colnames(xml_df1) %in% colnames(xml_df)]
        if(length(missing) > 0){
            xml_df[[missing]] <- NA
        }

        xml_df1 <- rbind(xml_df1, xml_df)
        n <- n + 1
    }

    return(xml_df1)
}
