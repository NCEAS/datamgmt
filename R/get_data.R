#' Log into RT
#'
#' Helper function to log into RT when downloading data. When run,
#' it prompts for the user's username and password.
#'
#' @param username RT username (enter in when prompted)
#' @param password RT password (enter in when prompted)
#'
#' @import dataone
#' @import rt
#' @import getPass
#'
#' @author Irene Steves
#'
#' @examples
#' \dontrun{
#' rt_login_interactive()
#' }
#'

rt_login_interactive <- function(username = readline("Enter username: ") ,
                                 password = getPass()) {
    rt_login(base = "https://support.nceas.ucsb.edu/rt/",
             user = username,
             pass = password)
}

#' Get an RT csv attachment
#'
#' This function allows you to read a csv attachment to an RT message
#' directly in R. It requires the RT ticket number and download URL
#' as arguments. The download URL can be accessed by right-clicking on
#' the link to download the attachment, and clicking "Copy Link Address."
#'
#' @param ticket (character) RT ticket number
#' @param download_url (character) RT download URL for the attachment
#'
#' @import dataone
#' @import stringr
#' @import rt
#' @import getPass
#' @import httr
#' @importFrom utils read.delim
#'
#' @author Irene Steves
#'
#' @examples
#' \dontrun{
#' data <- download_rt_csv(
#'     ticket = "13843",
#'     download_url = "https://support.nceas.ucsb.edu/rt/Ticket/Attachment/291896/209025/2016%20ITEX%20Barrow%20Atqasuk%20Climate%20v1.csv")
#' }
#'

get_rt_csv <- function(ticket, download_url) {
    #ticket = "16276"
    #download_url = "https://support.nceas.ucsb.edu/rt/Ticket/Attachment/355434/261485/Polaris%202017%20Soil.csv"

    attachment <- download_url %>%
        stringr::str_replace(".*Attachment/[\\d]{6}/", "") %>%
        stringr::str_replace("/.*", "")
    url <- paste0("https://support.nceas.ucsb.edu/rt/REST/1.0/ticket/",
                  ticket, "/attachments/", attachment, "/")
    raw <- httr::GET(url)
    data1 <- rawToChar(raw$content) %>%
        stringr::str_replace(".*Content\\: ", "") %>%
        stringr::str_replace("\n\n\n", "")

    if(stringr::str_detect(data1, "Credentials required")){
        #log into RT
        message("\nLogging into RT...\n")
        rt_login_interactive()
        raw <- httr::GET(url)
        data1 <- rawToChar(raw$content) %>%
            stringr::str_replace(".*Content\\: ", "") %>%
            stringr::str_replace("\n\n\n", "")
    }

    #roundabout way to account for commas inside cells
    #for example: 'cell1,cell2,"cell3,3",cell4' will split into 5 cells
    write(data1, "temp.txt")
    data2 <- utils::read.delim("temp.txt", sep = ",", quote = '\"',
                        check.names = FALSE)
    file.remove("temp.txt")
    return(data2)
}

#' Get a csv file from a DataOne node
#'
#' Get a csv file from a DataOne node; allows you to access the data in your
#' scripts without downloading the data onto your hard drive. Ensures that the original
#' data stays intact. This function is a wrapper for \link[dataone]{getObject}.
#'
#' @param mn A member node
#' @param pid The identifier of the data object
#'
#' @import dataone
#' @import stringr
#'
#' @export
#'
#' @author Irene Steves
#'
#' @examples
#' \dontrun{
#' cn <- CNode('PROD')
#' mn <- getMNode(cn,'urn:node:ARCTIC')
#'
#' data <- get_csv(mn, "urn:uuid:087bf762-ea3d-444f-ba59-1c0bc02fd415")
#' }
#'

get_csv <- function(mn, pid) {
    data_raw <- dataone::getObject(mn, pid)
    # same as: getDataObject(d1c, pid) %>% getData()
    data_char <- rawToChar(data_raw) %>%
        stringr::str_split("\r\n") %>%
        unlist() %>%
        stringr::str_split(",")
    data_combined <- do.call(rbind, data_char)
    data_clean <- data.frame(data_combined[-1,])
    colnames(data_clean) <- data_combined[1,]
    return(data_clean)
}

#' Get an EML file from a DataOne node
#'
#' Get an EML file from a DataOne node. This function is a wrapper for
#' \link[dataone]{getObject}.
#'
#' @param mn A member node
#' @param pid The identifier of the data object
#'
#' @import dataone
#' @import EML
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- CNode('PROD')
#' mn <- getMNode(cn,'urn:node:ARCTIC')
#'
#' eml <- get_eml(mn, "doi:10.18739/A2TP3N")
#' }
#'
#'

get_eml <- function(mn, pid) {
    dataone::getObject(mn, pid) %>%
        rawToChar() %>%
        EML::read_eml()
}
