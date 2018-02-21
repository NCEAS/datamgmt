#' Get NSF Arctic/Polar program award information
#'
#' Uses the [NSF API](https://www.research.gov/common/webapi/awardapisearch-v1.htm)
#' to get all records pertaining to the Arctic or Polar programs.
#'
#' @param from_date (character) Optional. Returns all
#' records with start date after specified date.
#' Format = "mm/dd/yyyy"
#' @param query (character) Optional. By default, the function
#' searches for all awards with either "polar" or "arctic" in
#' the fundProgramName. Additional queries can be specified
#' as defined in the NSF API.
#' Use '&' to join multiple queries (i.e., "keyword=water&agency=NASA")
#' @param print_fields (character) Optional. By default, the
#' following fields will be returned: id, awardeeName, date,
#' startDate, expDate, fundProgramName, poName,
#' primaryProgram, title, awardee, awardeeAddress,
#' perfAddress, piFirstName, piLastName, piPhone, piEmail
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
                       query = NULL,
                       print_fields = "id,awardeeName,date,startDate,expDate,fundProgramName,poName,primaryProgram,title,awardee,awardeeAddress,perfAddress,piFirstName,piLastName,piPhone,piEmail") {

    # package checks
    if (!requireNamespace("XML")) {
        stop(call. = FALSE,
             "The XML package is required for this function. Please install it and try again.")
    }
    if (!requireNamespace("stringr")) {
        stop(call. = FALSE,
             "The stringr package is required for this function. Please install it and try again.")
    }
    if (!requireNamespace("RCurl")) {
        stop(call. = FALSE,
             "The RCurl package is required for this function. Please install it and try again.")
    }

    # basic argument checks
    stopifnot(is.character(from_date) | is.null(from_date))
    stopifnot(is.character(query) | is.null(query))
    stopifnot(is.character(print_fields))

    base_url <- "https://api.nsf.gov/services/v1/awards.xml?fundProgramName=ARCTIC|fundProgramName=POLAR"
    if(!is.null(query)){
        query <- paste0("&", query)
    }
    query_url <- paste0(base_url, query,
                        "&printFields=", print_fields)

    if(!is.null(from_date)){
        if(!stringr::str_detect(from_date, "\\d\\d/\\d\\d/\\d\\d\\d\\d")) {
            stop("The from_date is not in the format 'mm/dd/yyyy'.")
        } else {
            query_url <- paste0(query_url, "&startDateStart=", from_date)
        }
    }

    xml1 <- RCurl::getURL(paste0(query_url, "&offset=", 1))
    if(stringr::str_detect(xml1, "ERROR")){
        stop("The query parameters are invalid.")
    }
    xml_df1 <- xmlToDataFrame(xml1)

    # since we can only download 25 entries at a time, we need to loop through the query using different offsets
    n <- 1
    repeat {
        start <- 1 + 25 * n
        xml <- getURL(paste0(query_url, "&offset=", start))
        xml_df <- xmlToDataFrame(xml)
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
