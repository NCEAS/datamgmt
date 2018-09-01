#' Query tree
#'
#' This is a helper function for \code{\link{plot_pkg_structure}}
#' that allows you to start with a (grand)parent PID and recursively
#' run Solr queries. The function outputs a tibble with
#'
#' @param mn The member node
#' @param parent_rm_pid (character) The top-level PID to use for the query
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
#' parent_rm_pid <- "urn:uuid:b3dc11f5-95e8-4f30-bef9-82464398bc5f"
#'
#' query_tree(mn, parent_rm_pid)
#' }

query_tree <- function(mn, parent_rm_pid){
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
            result2 <- query_tree(mn, rm_pid)
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
#' @param mn The member node
#' @param parent_rm_pid (character) The top-level PID in a data package family
#'
#' @export
#'
#' @import dplyr
#' @importFrom tidyr unnest
#'
#' @examples
#' \dontrun{
#' cn <- dataone::CNode('PROD')
#' mn <- dataone::getMNode(cn,'urn:node:ARCTIC')
#'
#' parent_rm_pid <- "resource_map_urn:uuid:2b5f44fc-810e-4c30-a63f-5bd2f3ff00a7"
#'
#' plot_pkg_structure(mn, parent_rm_pid)
#' }

plot_pkg_structure <- function(mn, parent_rm_pid) {

    if (!requireNamespace("visNetwork", quietly = TRUE) | !requireNamespace("igraph", quietly = TRUE) | !requireNamespace("purrr", quietly = TRUE)) {
        stop(call. = FALSE,
             "plot_pkg_structure requires the
         'visNetwork', 'igraph', and 'purrr' packages to be installed. Install these packages for this functionality.")

        #removed importFroms:

        # @importFrom visNetwork visIgraph
        # @importFrom igraph graph_from_data_frame
        # @importFrom purrr map
    }

    # Check that the pid is a resource map
    if(!str_detect(parent_rm_pid, "resource")){
        warning(cat("Is", parent_rm_pid, "a resource map PID? Please double check.\n"))
    }

    query_results <- query_tree(mn, parent_rm_pid)
    if(!nrow(query_results) > 0){
        stop("The query did not return any results. Check the member node and parent resource map.")
    }

    edges <- query_results %>%
        # as_tibble() %>%
        dplyr::mutate(resourceMap = str_split(resourceMap, " ")) %>%
        dplyr::mutate(resourceMap = purrr::map(resourceMap, function(vector){vector[length(vector)]})) %>% #get last element
        tidyr::unnest(resourceMap) %>%
        dplyr::rename(from = resourceMap,
                      to = identifier) %>%
        dplyr::mutate_all(funs(stringr::str_replace(., "resource_map_", ""))) %>%
        dplyr::distinct() %>%
        dplyr::select(from, to)

    rm_igraph <- igraph::graph_from_data_frame(d = edges, directed = TRUE)

    visNetwork::visIgraph(rm_igraph,
                          layout = "layout_as_tree",
                          flip.y = FALSE)
}
