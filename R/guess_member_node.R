#' Guess the Member Node
#'
#' @description Guess the member node that stores a Dataone object based on its
#' unique identifier (\code{pid}) and coordinating node (\code{cn}).  In most cases
#' the object is stored on the Production ("PROD") Node, however this function can
#' search across all coordinating nodes.  If only one member node is identified this
#' function returns the member node as a Dataone "MNode" object.  If multiple member
#' nodes are identified a vector of nodes is printed.
#'
#' @param cn (character) A character vector of coordinate nodes to search.  Defaults to "PROD".
#' Can be set to any combination of ("PROD", "STAGING", "STAGING2", "SANDBOX", "SANDBOX2", "DEV", "DEV2").
#' @param pid (character) The Datone unique object identifier.
#'
#' @export
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' The following two calls are equivalent:
#' mn <- guess_member_node("doi:10.18739/A2G287")
#' mn <- guess_member_node("PROD", "doi:10.18739/A2G287")
#'
#' Search all coordinating nodes:
#' cn = c("PROD", "STAGING", "STAGING2", "SANDBOX", "SANDBOX2", "DEV", "DEV2"))
#' mn <- guess_member_node(cn, "doi:10.18739/A2G287")
#' }
#'
guess_member_node <- function(cn = "PROD", pid) {
    stopifnot(is.character(cn))
    stopifnot(all(cn %in% c("PROD", "STAGING", "STAGING2", "SANDBOX", "SANDBOX2", "DEV", "DEV2")))
    stopifnot(is.character(pid))

    results <- unlist(sapply(cn, function(cn) {
        cn_object <- dataone::CNode(cn)
        q <- unlist(dataone::query(cn_object,
                                   paste0("q=identifier:\"",
                                          pid,
                                          "\"&fl=datasource")))
    }))

    if (length(results) == 0) {
        stop(paste0("Identifier not found in nodes: ", cn))
    }

    if (length(results) > 1) {
        message("Identifier found on multiple nodes:\n")
        print(results)
        return()
    }


    cn <- gsub("\\.datasource", "", names(results))
    cn <- dataone::CNode(cn)

    mn <- dataone::getMNode(cn, results)
    return(mn)
}
