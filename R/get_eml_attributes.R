# Return the maximum depth (or levels) of a list
# From: https://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r
list_depth <- function(input_list) {
    ifelse(is.list(input_list),
           1L + max(sapply(input_list, list_depth)),
           0L)
}

#' Return attribute (column) metadata from a Dataone Metadata object.
#'
#' @description Return attribute metadata from an EML object. This is largely a
#' wrapper for the function \code{get_attributes} from the EML Package
#' \url{https://github.com/ropensci/EML}.
#'
#' @param metadata (S4) EML object.
#' @return (list) A list of all attribute metadata from the EML in data.frame objects
#'
#' @export
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' mn <- dataone::getMNode(cn, 'urn:node:ARCTIC')
#' eml <- EML::read_eml(rawToChar(dataone::getObject(mn, "doi:10.18739/A23W02")))
#' attributes <- datamgmt::get_eml_attributes(mn, eml)
#'
#' # switch nodes
#' cn <- dataone::CNode('PROD')
#' knb <- dataone::getMNode(cn,"urn:node:KNB")
#' attributes <- get_eml_attributes(knb, "doi:10.5063/F1639MWV")
#' }
get_eml_attributes <- function(eml) {
    # TODO - make sure it works for otherEntities

    stopifnot(methods::is(mn, "MNode"))
    stopifnot(isS4(eml))

    results <- EML::eml_get(eml, "attributeList")
    names(results) <- EML::eml_get(eml, "entityName")

    # Unlist results if depth (levels) > 2
    if (list_depth(results) > 2) {
        results <- unlist(results, recursive = FALSE)
    }

    return(results)
}

#' Download attribute (column) metadata from a Dataone Metadata object to csvs.
#'
#' @description Download attribute metadata from an EML object as csvs. The name
#' of each csv corresponds to the file name of the Data Object it describes.
#' This can be prepended with the package identifier by setting \code{prefix_file_names = TRUE} (recommended).
#'
#' @param metadata (S4) EML object.
#' @param download_directory (character) Directory to download attribute metadata csv's to.
#' @param prefix_file_names (logical) Optional.  Whether to prefix file names with the package metadata identifier.  This is useful when downloading files from multiple packages to one directory.
#'
#' @export
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#  cn <- dataone::CNode('PROD')
#' mn <- dataone::getMNode(cn, 'urn:node:ARCTIC')
#' eml <- EML::read_eml(rawToChar(dataone::getObject(mn, "doi:10.18739/A23W02")))
#' attributes <- datamgmt::download_eml_attributes(eml, download_directory = tempdir(),
#' prefix_file_names = TRUE)
#'}
download_eml_attributes <- function(eml,
                                    download_directory,
                                    prefix_file_names = FALSE) {
    # TODO - write to one excel workbook

    stopifnot(isS4(eml))
    stopifnot(file.exists(download_directory))
    stopifnot(is.logical(prefix_file_names))


    attributes <- get_eml_attributes(eml)

    prefix <- character(0)
    if (prefix_file_names == TRUE) {
        prefix <- eml_get(eml, "packageId") %>%
            as.character() %>%
            remove_special_characters() %>%
            paste0("_")
    }

    file_names <- paste0(prefix, names(attributes)) %>%
        gsub(pattern = "\\..*\\.", replacement = "_") %>%
        paste0(".csv")

    file_paths <- file.path(download_directory, file_names)

    for (i in seq_along(attributes)) {
        if (!is.null(attributes[[i]])) {
            write.csv(data.frame(attributes[[i]]), file = file_paths[i], row.names = FALSE)
        }
    }

    return(invisible())
}

#' Return attribute (column) metadata from a Dataone Package URL.
#'
#' @description Return attribute metadata from an EML object or Dataone Package URL.
#' This is largely a wrapper for the function \code{get_attributes} from the EML Package
#' \url{https://github.com/ropensci/EML}.
#'
#' @param mn (MNode/CNode) The Dataone Node that stores the Metadata object, from \url{https://cn.dataone.org/cn/v2/node}
#' @param metadata (S4 / character) Optional. Either the full eml S4 object document or the url of the Dataone Package.
#' The S4 object input is a more reliable method.
#' @param write_to_csv (logical) Optional. Option whether to download the attribute metadata to csv's.  Defaults to \code{FALSE}
#' @param prefix_file_names (logical) Optional.  Whether to prefix file names with the package metadata identifier.
#' This is useful when downloading files from multiple packages to one directory.
#' @param download_directory (character) Optional.  Directory to download attribute metadata csv's to.
#' Required if \code{write_to_csv} is \code{TRUE}
#' @return (list) A list of all attribute metadata from the EML in data.frame objects
#'
#' @export
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' attributes <- get_eml_attributes(mn,
#' "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02")
#'
#' # Download attribute metadata in csv format:
#' attributes <- get_eml_attributes(mn,
#' "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02",
#' write_to_csv = TRUE,
#' download_directory = tempdir())

#' # switch nodes
#' cn <- dataone::CNode('PROD')
#' knb <- dataone::getMNode(cn,"urn:node:KNB")
#' attributes <- get_eml_attributes(knb,
#' "https://knb.ecoinformatics.org/#view/doi:10.5063/F1639MWV")
#' }
get_eml_attributes_url <- function(mn,
                                   url_path,
                                   write_to_csv = FALSE,
                                   prefix_file_names = FALSE,
                                   download_directory = NULL) {
    stopifnot(methods::is(mn, "MNode"))
    stopifnot(is.character(url_path))
    stopifnot(is.logical(write_to_csv))
    if (!is.null(download_directory)){
        stopifnot(is.character(download_directory))
        stopifnot(file.exists(download_directory))
    }

    pid <- unlist(strsplit(url_link, "view/"))[[2]]
    eml <- EML::read_eml(rawToChar(dataone::getObject(mn, pid)))

    if (write_to_csv == TRUE) {
        download_eml_attributes(eml, download_directory, prefix_file_names)
    }

    results <- get_eml_attributes(eml)

    return(results)
}
