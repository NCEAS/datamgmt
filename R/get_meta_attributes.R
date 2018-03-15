#' Return attribute metadata from an EML object or Dataone Package URL.
#'
#' @description Return attribute metadata from an EML object or Dataone Package URL.
#' This is largely a wrapper for the function \code{get_attributes} from the EML Package
#' \url{https://github.com/ropensci/EML}.
#'
#' @param mn (MNode/CNode) The Dataone Node that stores the Metadata object, from \url{https://cn.dataone.org/cn/v2/node}
#' @param metadata (S4 / character) Optional. Either the full eml S4 object document or the url of the Dataone Package.
#' The S4 object input is a more reliable method.
#' @param write_to_csv (logical) Optional. Option whether to download the attribute metadata to csv's.  Defaults to \code{FALSE}
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
#  cn <- dataone::CNode('PROD')
#' mn <- dataone::getMNode(cn, 'urn:node:ARCTIC')
#' eml <- EML::read_eml(rawToChar(dataone::getObject(mn, "doi:10.18739/A23W02")))
#' attributes <- datamgmt::get_eml_attributes(mn, eml)
#'
#' attributes <- datamgmt::get_eml_attributes(mn, "https://arcticdata.io/catalog/#view/doi:10.18739/A23W02")
#'
#' # switch nodes
#' cn <- dataone::CNode('PROD')
#' knb <- dataone::getMNode(cn,"urn:node:KNB")
#' attributes <- get_eml_attributes(knb, "https://knb.ecoinformatics.org/#view/doi:10.5063/F1639MWV")
#' }
get_eml_attributes <- function(mn,
                                metadata,
                                write_to_csv = FALSE,
                                download_directory = NULL) {
    # TODO - make all TODO's individual functions
    # TODO - load RData objects
    # TODO - write to individual csvs
    # TODO - write to one excel workbook
    # TODO - make sure it works for otherEntities
    # TODO - switch nodes with "ADC", etc.
    stopifnot(methods::is(mn, "MNode"))
    stopifnot(any(isS4(metadata), is.character(metadata)))
    stopifnot(is.logical(write_to_csv))
    if (!is.null(download_directory)){
        stopifnot(is.character(download_directory))
        stopifnot(file.exists(download_directory))
    }

    # If url input is specified extract pid and download eml
    if (isS4(metadata)) {
        eml <- metadata
    } else {
        pid <- unlist(strsplit(metadata, "view/"))[[2]]
        eml <- EML::read_eml(rawToChar(dataone::getObject(mn, pid)))
    }

    n1 <- length(eml@dataset@dataTable)
    n2 <- length(eml@dataset@otherEntity)
    if (n1 == 0 & n2 == 0){
        warning("EML: ", metadata, "does not contain attribute metadata") #TODO make this work for S4 objects
    }

    results <- EML::eml_get(eml, "attributeList")
    names(results) <- EML::eml_get(eml, "entityName")

    # Convert nested list to one level
    results <- unlist(results, recursive = FALSE)

    if (write_to_csv == TRUE) {
        file_names <- gsub("\\.*\\.", "_", names(results))
        file_paths <- file.path(download_directory, paste0(file_names, ".csv"))

        for (i in seq_along(results)) {

            if (!is.null(results[[i]])) {
                data <- data.frame(results[[i]])
                file_path <- file_paths[i]
                write.csv(data, file = file_path, row.names = FALSE)
            }
        }
    }


    return(results)
}
