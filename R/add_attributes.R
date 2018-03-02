#' Add attribute metadata to a data.frame object
#'
#' Adds attribute metadata to a data.frame object.  Intended for use with a call
#' to 'EML:get_attributes' in order to extract metadata from an EML and attach
#' it a data.frame object as an attributes(data.frame) property.
#'
#' @param data (data.frame) Data object to add attributes information to.
#' @param attributes (data.frame) Data frame of attribute metadata to add.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame("depth" = c(1.2, 2.3), "temperature" = c(10.7, 9.5))
#' attributes <- data.frame("attributeName" = c("depth", "temperature"),
#'                          "attributeDefinition" = c("water depth in meters",
#'                                                     "temperature in celsius"),
#'                          "unit" = c("meter", "celsius"))
#' data <- add_attributes(data, attributes)
#'
#' # View all attribute metadata
#' attributes(data)
#' # View attribute metadata for one variable
#' attributes(data)$depth
#'
#' devtools::install_github("ropensci/EML")
#' library(EML) # for updated version of 'get_attributes'
#' eml <- EML::read_eml(rawToChar(dataone::getObject(mnReal, "doi:10.18739/A2F299")))
#' attributes <- EML::get_attributes(eml@@dataset@@dataTable[[3]]@@attributeList)[[1]]
#' data <- read.csv(
#' "https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A11986ca1-5560-48ac-b8f9-0469ce561946")
#' data <- add_attributes(data, attributes)
#' }
#'
#' @author Dominic Mullen \email{dmullen17@@gmail.com}
#'
#' @return (data.frame())
add_attributes <- function(data, attributes) {
    # TODO add $factors case from 'get_attributes'
    stopifnot(is.data.frame(data))
    stopifnot(is.data.frame(attributes))
    stopifnot(ncol(data) == nrow(attributes))

    n_attributes <- nrow(attributes)
    n_meta_col <- ncol(attributes)

    # Initialize attribute list
    attribute_list <- list()

    # Convert each row of 'attributes' to a list and store in 'attribute_list'
    for (i in seq_len(n_attributes)) {
        metadata_list <- list()

        for (j in seq_len(n_meta_col)) {
            temp_var <- assign("x", attributes[i, j])
            metadata_list <- c(metadata_list, list(x))
        }

        names(metadata_list) = colnames(attributes)
        attribute_list[[i]] <- metadata_list
    }

    names(attribute_list) <- colnames(data)
    attributes(data) <- c(attributes(data), attribute_list)

    return(data)
}

#
resource_map_pids = c("resource_map_urn:uuid:477cf55d-32f3-4515-87b7-15fce9e11e33",
                      "resource_map_doi:10.18739/A2F299")
import_package_data <- function(mn, pids) {
    stopifnot(methods::is(mn, "MNode"))
    stopifnot(length(pids) > 0)
    stopifnot(all(is.character(resource_map_pids)))

    # Check that data objects exist and return resource map pids
    query <- data_objects_exist(mn, pids)
    resouce_map_pids <- unlist(sapply(seq_along(query$resource_map), function(i) {
        if (query$data_objects_present[i] == "no") {
            message("No data objects present in ", query$identifier[i])
        } else {
            query$resource_map[i]
        }
    }))
    # import data objects
    # assign filename appended with metadata doi

    # if add_attributes = T then look in EML for attributes
    return(resource_map_pids)
}
import_package_data(mnReal, resource_map_pids)
