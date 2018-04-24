#' Query tree
#'
#' This is a helper function for \code{\link{plot_pkg_structure}}
#' that allows you to start with a (grand)parent PID and recursively
#' run Solr queries. The function outputs a tibble with
#'
#' @param parent_pid The top-level PID to use for the query
#'
#' @import magrittr
#' @import stringr
#' @importFrom tidyr unnest
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' mn <- dataone::getMNode(cn,'urn:node:ARCTIC')
#'
#' parent_pid <- "urn:uuid:b3dc11f5-95e8-4f30-bef9-82464398bc5f"
#'
#' query_edges(parent_pid)
#' }

query_tree <- function(parent_rm_pid){
    # Initialize structure
    if(!exists("rm_all")){
        rm_all <- NULL
    }

    # Run query & clean results
    result <- dataone::query(mn, list(q = paste0('resourceMap:"', parent_rm_pid, '"+AND+formatType:RESOURCE'),
                                      fl = 'identifier, resourceMap',
                                      sort = 'dateUploaded+desc',
                                      rows='10000'),
                             as = "data.frame")

    # Run recursion
    if(nrow(result) > 0){
        rm_all <- dplyr::bind_rows(rm_all, result)

        for (rm_pid in result$identifier) {
            result2 <- query_tree(rm_pid)
            rm_all <- dplyr::bind_rows(rm_all, result2)
        }
    }
    return(rm_all)
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
