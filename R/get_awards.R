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
#' @importFrom RCurl getURL
#' @importFrom stringr str_detect
#' @importFrom XML xmlToDataFrame
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

    # Encode the pipe separators: the NSF API rejects raw pipes in request URLs.
    base_url <- "https://api.nsf.gov/services/v1/awards.xml?fundProgramName=ARCTIC%7CfundProgramName=POLAR%7CfundProgramName=AON"
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
            query_url <- paste0(query_url, "&dateStart=", from_date)
        }
    }

    if(!is.null(to_date)) {
        if(!stringr::str_detect(to_date, "\\d\\d/\\d\\d/\\d\\d\\d\\d")) {
            stop("The to_date is not in the format 'mm/dd/yyyy'.")
        } else {
            query_url <- paste0(query_url, "&dateEnd=", to_date)
        }
    }

    requested_fields <- strsplit(print_fields, ",", fixed = TRUE)[[1]]

    parse_page <- function(offset) {
        xml <- getURL(paste0(query_url, "&offset=", offset))
        if (stringr::str_detect(xml, "ERROR")) {
            stop("The query parameters are invalid.")
        }

        # The API occasionally includes a Windows-1252 byte in otherwise UTF-8
        # XML; RCurl represents these bytes as strings such as <9d>.
        xml <- gsub("<[[:xdigit:]]{2}>", "", xml)
        if (!stringr::str_detect(xml, "^\\s*<\\?xml") &&
            !stringr::str_detect(xml, "^\\s*<response(?:\\s|>)")) {
            stop("The NSF API returned a non-XML response. Please retry later.")
        }

        doc <- XML::xmlParse(xml, asText = TRUE)
        awards <- XML::getNodeSet(doc, "//award")
        rows <- lapply(awards, function(award) {
            values <- lapply(requested_fields, function(field) {
                nodes <- XML::getNodeSet(award, paste0("./", field))
                if (length(nodes)) XML::xmlValue(nodes[[1]]) else NA_character_
            })
            stats::setNames(as.data.frame(values, stringsAsFactors = FALSE), requested_fields)
        })
        data <- if (length(rows)) do.call(rbind, rows) else
            as.data.frame(stats::setNames(rep(list(character()), length(requested_fields)),
                                          requested_fields), stringsAsFactors = FALSE)

        total_node <- XML::getNodeSet(doc, "//metadata/totalCount")
        total <- if (length(total_node)) suppressWarnings(as.integer(XML::xmlValue(total_node[[1]]))) else NA_integer_
        list(data = data, total = total, n = length(awards))
    }

    page_size <- 25L
    offset <- 1L
    page <- parse_page(offset)
    result <- page$data

    repeat {
        next_offset <- offset + page_size
        if (page$n == 0L ||
            (!is.na(page$total) && next_offset > page$total) ||
            (is.na(page$total) && page$n < page_size)) {
            break
        }

        offset <- next_offset
        page <- parse_page(offset)
        result <- rbind(result, page$data)
    }

    return(result)
}
