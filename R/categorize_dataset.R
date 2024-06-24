#' Interim method of categorizing arctic data center datasets into one of several themes after a doi is issued
#'
#' Please ask Jeanette or Jasmine to grant you access to the [google sheet](https://docs.google.com/spreadsheets/d/1S_7iW0UBZLZoJBrHXTW5fbHH-NOuOb6xLghraPA4Kf4/edit#gid=1479370118)
#'
#' This function will account for older versions of the dataset has been already categorized. Please make sure you have a token from arcticdata.io.
#'
#' @param doi (character) the doi formatted as doi:10.#####/#########
#' @param themes (list) themes of the dataset and you can classify up to 5. The definitions of the [themes](https://docs.google.com/spreadsheets/d/1S_7iW0UBZLZoJBrHXTW5fbHH-NOuOb6xLghraPA4Kf4/edit#gid=1479370118)
#' @param coder (character) your name, this is to identify who coded these themes
#' @param test (logical) for using the test google sheet (mainly for testing purposes), defaults to FALSE
#' @param overwrite (logical) whether or not to update the entry (for example if you want to update the themes)
#'
#' @return NULL the result will be written to an external [google sheet](https://docs.google.com/spreadsheets/d/1S_7iW0UBZLZoJBrHXTW5fbHH-NOuOb6xLghraPA4Kf4/edit#gid=1479370118)

#' @examples
#' \dontrun{
#' # categorize_dataset("doi:10.18739/A2QJ77Z09", c("biology", "oceanography"), "your name")
#' }
#' @author Jasmine Lai
#' @export
categorize_dataset <- function(doi, themes, coder, test = F, overwrite = F){

  stopifnot(length(themes) > 0 & length(themes) < 5)

  #check if there is googlesheet token already
  if(!googlesheets4::gs4_has_token()){
    #for using googlesheets on the server - prompts user to copy id into command prompt
    googlesheets4::gs4_auth(use_oob = T)
  }

  #choose which sheet we want to work with (test or actual list)
  if (test) {
    ss <- "https://docs.google.com/spreadsheets/d/1GEj9THJdh22KCe1RywewbruUiiVkfZkFH8FO1ib9ysM/edit#gid=1479370118"
  } else {
    ss <- "https://docs.google.com/spreadsheets/d/1S_7iW0UBZLZoJBrHXTW5fbHH-NOuOb6xLghraPA4Kf4/edit#gid=1479370118" # offical
  }

  #checking for valid themes
  accepted_themes <- suppressMessages(googlesheets4::read_sheet(ss, sheet = "categories", col_names = F))
  standard_themes <- stringr::str_to_lower(themes)
  check_themes <- standard_themes %in% accepted_themes$...1
  problem_themes <- which(check_themes == F)
  stopifnot(all(standard_themes %in% accepted_themes$...1) == T)

  #set node
  cn <- dataone::CNode("PROD")
  adc <- dataone::getMNode(cn, "urn:node:ARCTIC")

  #identifier or previous version already in the original sheet
  original_sheet <- suppressMessages(googlesheets4::read_sheet(ss))
  all_versions <- arcticdatautils::get_all_versions(adc, doi) #previous versions
  #get the row(s) to look into
  sheet_index <- unlist(purrr::map(all_versions,
                                   ~which(.x == original_sheet$id)))
  #get the themes
  original_themes <- unlist(purrr::map(sheet_index,
                                       ~dplyr::select(original_sheet[1,],
                                                      `theme1`, `theme2`, `theme3`, `theme4`, `theme5`)))

  #check for previous mentions or if we want to overwrite all together
  if(overwrite & doi != all_versions[length(all_versions)]){
    warning("overwriting themes - identifiers or previous versions already in sheet, updating identifier")
    purrr::map(sheet_index, ~suppressMessages(googlesheets4::range_delete(ss, range = as.character(.x + 1), shift = "up")))
  } else if(overwrite){
    warning("overwriting themes")
    purrr::map(sheet_index, ~suppressMessages(googlesheets4::range_delete(ss, range = as.character(.x + 1), shift = "up")))
  } else if(any(all_versions[1:length(all_versions) - 1 ] %in% original_sheet$id)){  # update the identifier
    warning("identifiers or previous versions already in sheet, updating identifier")
    purrr::map(sheet_index, ~suppressMessages(googlesheets4::range_delete(ss, range = as.character(.x + 1), shift = "up")))
  } else if(doi %in% original_sheet$id){
    stop(paste("Dataset with identifier" ,doi, "is already categorized - identifier not added. Set overwrite to TRUE to update."))
  } else {
    message("categorizing dataset...")
  }

  #Wrap the pid with special characters with escaped backslashes
  doi_escape <- paste0("id:", '\"', all_versions[length(all_versions)], '\"')

  #search solr for the submission
  solr <- dataone::query(adc, list(
    q = doi_escape,
    fl = "identifier, dateUploaded, abstract, keywords, title",
    sort = "dateUploaded+desc",
    rows = "100"
  ), as = "data.frame")

  #do a solr query to retrieve information about the dataset
  df_query <- solr %>%
    dplyr::mutate(url = paste0("http://arcticdata.io/catalog/view/", solr$identifier))

  #make sure keywords is not a list
  if(is.list(df_query$keywords)){
    df_query$keywords[[1]] <- paste(df_query$keywords[[1]], collapse = ",")

    df_query$keywords <- as.character(df_query$keywords)
  }

  #check if all the columns needed were returned
  if(all(c("url", "identifier", "dateUploaded", "abstract", "keywords", "title") %in% names(df_query))){
    #if every column was returned and identifier and previous versions not in sheet
    df_row <- df_query %>%
      dplyr::select("url", "identifier", "dateUploaded", "abstract", "keywords", "title") %>%
      dplyr::mutate(
        theme1 = standard_themes[1],
        theme2 = standard_themes[2],
        theme3 = standard_themes[3],
        theme4 = standard_themes[4],
        theme5 = standard_themes[5],
        coder = coder
      )
  }else{
    #stop to let the user know what column is missing - dataset should be fixed before continuing
    col_missing <- setdiff(c("url", "identifier", "dateUploaded", "abstract", "keywords", "title"), names(df_query))
    stop(paste("the column(s):", col_missing, "is missing, please fix the dataset before continuing"))
  }

  #write to googlesheet and add a row
  suppressMessages(googlesheets4::sheet_append(ss, df_row, sheet = 1))
  message("dataset categorized")
}
