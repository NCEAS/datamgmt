#to do fix ability to authenticateon server
#' Interim method of categorizing datasets
#'
#' Please ask Jeanette or Jasmine to grant you access to the google sheet
#'
#' @param doi (character) the doi # !!! needs to also take a list?
#' @param themes (character) themes of the dataset, can classify up to 5 - definition of the themes can be found here
#' @param coder (character) your name, this is to identify who coded these themes
#'
#' @return NULL the result wil be written to an external google sheet
#' @export
#'
#' @examples #new_category("doi:10.18739/A2QJ77Z09", c("biology", "oceanography"), "your name")
new_category <- function(doi, themes, coder) {
    doi <- "doi:10.18739/A2QJ77Z09"
    themes <- c("biology", "oceanography")
    coder <- "jasmine"

    stopifnot(length(themes) > 0)

    #test sheet
    ss <- "https://docs.google.com/spreadsheets/d/1GEj9THJdh22KCe1RywewbruUiiVkfZkFH8FO1ib9ysM/edit#gid=1479370118"
    #ss <-  "https://docs.google.com/spreadsheets/d/1S_7iW0UBZLZoJBrHXTW5fbHH-NOuOb6xLghraPA4Kf4/edit#gid=1479370118"
    #classify <- googlesheets4::read_sheet(ss)
    df_empty <- classify[FALSE,]

    cn <- dataone::CNode("PROD")
    adc <- dataone::getMNode(cn, "urn:node:ARCTIC")

    # Wrap the pid with special characters with escaped backslashes
    doi_escape <- paste0('id:', '\"', doi, '\"')

    latest <- dataone::query(adc, list(q=doi_escape,
                              fl="identifier, dateUploaded, abstract, keywords, title",
                              sort = "dateUploaded+desc",
                              rows="100"), as = "data.frame")
    df_query <- latest %>%
        dplyr::mutate(url = paste0("http://arcticdata.io/catalog/view/", identifier)) %>%
        dplyr::select("url", "identifier", "dateUploaded", "abstract", "keywords", "title") %>%
        dplyr::mutate(theme1 = themes[1],
                      theme2 = themes[2],
                      theme3 = themes[3],
                      theme4 = themes[4],
                      theme5 = themes[5],
                      coder = coder)

    #write to google sheet add
    googlesheets4::sheets_append(df_query, ss, sheet = 1)
}
