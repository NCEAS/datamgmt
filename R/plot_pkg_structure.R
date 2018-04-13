#' Get edges
#'
#' This is a helper function for \code{\link{plot_pkg_structure}}
#' that allows you to get edges for visualizing
#' the structure of nested packages.
#'
#' @param query_results Results of a Solr query. Requires at minimum
#' the obsoletedBy, resourceMap, and documents fields.
#'
#' @import dplyr
#' @import stringr
#' @importFrom tidyr unnest
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' mn <- dataone::getMNode(cn,'urn:node:ARCTIC')
#'
#' #query all packages with "Orcutt" as an author
#' result <- dataone::query(mn, list(q = 'origin:*Orcutt*',
#'                      fl = '*',
#'                      sort = 'dateUploaded+desc',
#'                      rows='10000'),
#'                      as = "data.frame")
#'
#' get_query_edges(result)
#' }

get_query_edges <- function(query_results) {
    # resource maps (from) and their children (to)

    query_results %>%
        # tibble::as_tibble() %>%
        dplyr::filter(is.na(obsoletedBy)) %>% #remove any obsoleted resource maps from the mix
        dplyr::select(resourceMap, documents) %>%
        dplyr::mutate(documents = stringr::str_split(documents, " ")) %>% #alternative to spread + gather
        tidyr::unnest(documents) %>%
        dplyr::filter(stringr::str_detect(documents, "resource")) %>% #grab only resource maps (ignores data and metadata objects)
        dplyr::rename(from = resourceMap,
               to = documents) %>%
        dplyr::mutate_all(funs(stringr::str_replace(., "resource_map_", ""))) #remove "resource_map_" to reduce identifier lengths when plotting
}

#' Plot package structure
#'
#' This function allows you to quickly visualize how data packages
#' are related to each other.
#'
#' @param query_results Results of a Solr query. Requires at minimum
#' the obsoletedBy, resourceMap, and documents fields.
#'
#' @export
#'
#' @importFrom visNetwork visIgraph
#' @importFrom igraph graph_from_data_frame
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' mn <- dataone::getMNode(cn,'urn:node:ARCTIC')
#'
#' #query all packages with "Orcutt" as an author
#' result <- dataone::query(mn, list(q = 'origin:*Orcutt*',
#'                      fl = '*',
#'                      sort = 'dateUploaded+desc',
#'                      rows='10000'),
#'                      as = "data.frame")
#'
#' plot_pkg_structure(result)
#'
#' #query all packages with "Tweedie" or "Vargas" or "Oberbauer" as an author
#' result <- dataone::query(mn, list(q = 'origin:(*Tweedie*+OR+*Vargas*+OR+*Oberbauer*)',
#'                      fl = '*',
#'                      sort = 'dateUploaded+desc',
#'                      rows='10000'),
#'                      as = "data.frame")
#'
#' plot_pkg_structure(result)
#' }

plot_pkg_structure <- function(query_results) {
    edges <- get_query_edges(query_results)

    rm_igraph <- igraph::graph_from_data_frame(d = edges, directed = TRUE)

    visNetwork::visIgraph(rm_igraph,
                          layout = "layout_as_tree",
                          flip.y = FALSE)
}
