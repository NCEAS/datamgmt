#' Query tree
#'
#' This is a helper function for [plot_pkg_structure()]
#' that starts with a (grand)parent PID and recursively
#' runs Solr queries to find all resource map PIDs in a
#' data package family.
#'
#' @param mn (MNode)The Member Node to query.
#' @param parent_rm_pid (character) The top-level PID in a data package family.
#'
#' @return (data.frame) A data frame of results.
#'
#' @import dataone
#' @importFrom dplyr bind_rows
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' cn <- CNode("PROD")
#' mn <- getMNode(cn,"urn:node:ARCTIC")
#'
#' parent_rm_pid <- "urn:uuid:..."
#'
#' query_tree(mn, parent_rm_pid)
#' }
query_tree <- function(mn, parent_rm_pid) {
    stopifnot(class(mn) %in% c("MNode", "CNode"))
    stopifnot(is.character(parent_rm_pid), nchar(parent_rm_pid) > 0)

    # Initialize structure
    if (!exists("rm_all")) {
        rm_all <- NULL
    }

    # Run query and clean results
    result <- dataone::query(mn,
                             list(q = paste0('resourceMap:"', parent_rm_pid, '"+AND+formatType:RESOURCE'),
                                  fl = 'identifier, resourceMap',
                                  sort = 'dateUploaded+desc',
                                  rows = '10000'),
                             as = "data.frame")

    # Run recursion
    if (nrow(result) > 0) {
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
#' This function visualizes how data packages in a data package family
#' are related to each other.
#'
#' @param mn (MNode) The Member Node to query.
#' @param parent_rm_pid (character) The top-level PID in a data package family.
#'
#' @return A visIgraph plot.
#'
#' @importFrom dplyr mutate rename mutate_all funs distinct select
#' @importFrom magrittr %>%
#' @importFrom stringr str_split str_replace
#' @importFrom tidyr unnest
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- CNode("PROD")
#' mn <- getMNode(cn,"urn:node:ARCTIC")
#'
#' parent_rm_pid <- "resource_map_urn:uuid:..."
#'
#' plot_pkg_structure(mn, parent_rm_pid)
#' }
plot_pkg_structure <- function(mn, parent_rm_pid) {
    stopifnot(class(mn) %in% c("MNode", "CNode"))
    stopifnot(is.character(parent_rm_pid), nchar(parent_rm_pid) > 0)

    # Include required packages in Suggests instead of Imports in DESCRIPTION file
    if (!requireNamespace("visNetwork", quietly = TRUE) || !requireNamespace("igraph", quietly = TRUE)) {
        stop(call. = FALSE,
             paste("plot_pkg_structure() requires the visNetwork and igraph packages to be installed.",
                   "Install these packages to use this function."))
    }

    query_results <- query_tree(mn, parent_rm_pid)

    if (is.null(query_results)) {
        stop("The query did not return any results. Check the Member Node and parent resource map PID.")
    }

    edges <- query_results %>%
        dplyr::mutate("resourceMap" = stringr::str_split(.$resourceMap, " ")) %>% # separate all resource map versions
        dplyr::mutate("resourceMap" = lapply(.$resourceMap, function(vector) vector[length(vector)])) %>% # get latest resource map
        tidyr::unnest() %>% # unnest resourceMap list column
        dplyr::rename("from" = "resourceMap", "to" = "identifier") %>%
        dplyr::mutate_all(dplyr::funs(stringr::str_replace(., "resource_map_", ""))) %>%
        dplyr::distinct() %>%
        dplyr::select("from", "to")

    rm_igraph <- igraph::graph_from_data_frame(d = edges, directed = TRUE)

    visNetwork::visIgraph(rm_igraph,
                          layout = "layout_as_tree",
                          flip.y = FALSE)
}


# Quiets concerns of R CMD check about no visible binding for global variable ‘.’
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
