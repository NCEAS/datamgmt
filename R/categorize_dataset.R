# to do fix ability to authenticateon server
#' Interim method of categorizing datasets into one of several themes after a doi is issued
#'
#' Please ask Jeanette or Jasmine to grant you access to the [google sheet](https://docs.google.com/spreadsheets/d/1S_7iW0UBZLZoJBrHXTW5fbHH-NOuOb6xLghraPA4Kf4/edit#gid=1479370118)
#'
#' @param doi (character) the doi formatted as doi:10.#####/#########
#' @param themes (list) themes of the dataset, can classify up to 5 - definition of the [themes](https://docs.google.com/spreadsheets/d/1S_7iW0UBZLZoJBrHXTW5fbHH-NOuOb6xLghraPA4Kf4/edit#gid=1479370118)
#' @param coder (character) your name, this is to identify who coded these themes
#' @param test (logical) for using the test google sheet, defaults to FALSE
#' @param overwrite (logical) whether or not to update the themes
#'
#' @return NULL the result wil be written to an external [google sheet](https://docs.google.com/spreadsheets/d/1S_7iW0UBZLZoJBrHXTW5fbHH-NOuOb6xLghraPA4Kf4/edit#gid=1479370118)

#' @examples
#' \dontrun{
#' # categorize_dataset("doi:10.18739/A2QJ77Z09", c("biology", "oceanography"), "your name", test = T)
#' }
#' @author Jasmine Lai
#' @export
categorize_dataset <- function(doi, themes, coder, test = F, overwrite = F){

  stopifnot(length(themes) > 0 & length(themes) < 5)
  stopifnot(grepl("doi", doi))

  #for using google sheets on the server - prompts user to copy id into command prompt
  googlesheets4::gs4_auth(use_oob = T)

  #Select test sheet
  if (test) {
    ss <- "https://docs.google.com/spreadsheets/d/1GEj9THJdh22KCe1RywewbruUiiVkfZkFH8FO1ib9ysM/edit#gid=1479370118"
  } else {
    ss <- "https://docs.google.com/spreadsheets/d/1S_7iW0UBZLZoJBrHXTW5fbHH-NOuOb6xLghraPA4Kf4/edit#gid=1479370118" # offical
  }

  #checking for valid themes
  accepted_themes <- suppressMessages(googlesheets4::read_sheet(ss, sheet = "categories", col_names = F))
  check_themes <- themes %in% accepted_themes$...1
  problem_themes <- which(check_themes == F)
  stopifnot(all(themes %in% accepted_themes$...1) == T)

  #set node
  cn <- dataone::CNode("PROD")
  adc <- dataone::getMNode(cn, "urn:node:ARCTIC")

  #identifier or previous version already in the original heet
  original_sheet <- suppressMessages(googlesheets4::read_sheet(ss))
  all_versions <- arcticdatautils::get_all_versions(adc, doi) #previous versions
  #get the rows to look into
  sheet_index <- unlist(purrr::map(all_versions, ~which(.x == original_sheet$id)))
  #check themes if they are the same
  original_themes <- purrr::map(sheet_index, ~original_sheet[.x,7:11]) %>% unlist()

  if(overwrite){
    warning("overwriting themes")
    purrr::map(sheet_index, ~suppressMessages(googlesheets4::range_delete(ss, range = as.character(.x + 1), shift = "up")))
  } else if(any(all_versions[1:length(all_versions) - 1 ] %in% original_sheet$id)){  # update the identifier
    warning("identifiers or previous versions already in sheet, updating identifier")
    #suppressMessages()
    purrr::map(sheet_index, ~suppressMessages(googlesheets4::range_delete(ss, range = as.character(.x + 1), shift = "up")))
  } else {
    stop(paste("Dataset with identifier" ,doi, "is already categorized - identifier not added. Set overwrite to TRUE to update.")
  }

  #Wrap the pid with special characters with escaped backslashes
  doi_escape <- paste0("id:", '\"', all_versions[length(all_versions)], '\"')

  # search solr for the submission
  solr <- dataone::query(adc, list(
    q = doi_escape,
    fl = "identifier, dateUploaded, abstract, keywords, title",
    sort = "dateUploaded+desc",
    rows = "100"
  ), as = "data.frame")

  #identifier and previous versions not in sheet, add row
  df_query <- solr %>%
    dplyr::mutate(url = paste0("http://arcticdata.io/catalog/view/", solr$identifier)) %>%
    dplyr::select("url", "identifier", "dateUploaded", "abstract", "keywords", "title") %>%
    dplyr::mutate(
      theme1 = themes[1],
      theme2 = themes[2],
      theme3 = themes[3],
      theme4 = themes[4],
      theme5 = themes[5],
      coder = coder
    )

  #write to googlesheet
  suppressMessages(googlesheets4::sheet_append(ss, df_query, sheet = 1))
}
