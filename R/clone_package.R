#' Clones objects between DataONE Member Nodes.
#'
#' @description Clones objects between DataONE member nodes.
#' Note, the dateUploaded, obsoletes, and obsoletedBy fields in the sysmeta will be reset on the cloned object.
#'
#' @param pid (character) Object pid
#' @param from (D1Client) D1Client to clone objects from.
#' @param to (D1Client) D1Client to clone objects to.
#' @param add_access_to (character, vector) Will give read, write, and changePermission access to all strings in vector. If no additional access is desired, set to \code{NULL}. Note, setting this to \code{NULL} could lead to situations where it is not possible to read, write, or changePermissions on the cloned object.
#' @param public (logical) Optional. Will set public read access.  Defaults to \code{FALSE}.
#' @param new_pid (logical) Optional. Will give the clone a new pid.  Defaults to \code{TRUE}.
#' @param new_submitter (character) Optional. Will change the submitter in the system metadata to given string if present.  Defaults to \code{NULL}.
#' @param change_origin_node (logical) Optional. Will change the originMemberNode in the system metadata to the cloned member node if TRUE.  Defaults to \code{FALSE}.
#' @param change_auth_node (logical) Optional. Will change the authoritativeMemberNode in the system metadata to the cloned member node if TRUE.  Defaults to \code{FALSE}.
#'
#' @importFrom uuid UUIDgenerate
#'
#' @return (character) PID of cloned object. \code{NULL} if could not clone.
#'
#' @examples
#' \dontrun{
#' # First set up the member nodes we're cloning between (in this example they are the same but could be different)
#' to <- dataone::D1Client("STAGING", "urn:node:mnTestARCTIC")
#' from <- dataone::D1Client("STAGING", "urn:node:mnTestARCTIC")
#'
#' # Choose an object to clone (here a new one is created)
#' pid <- arcticdatautils::create_dummy_object(to@@mn)
#'
#' # Clone object
#' cloned_pid <- clone_object(pid = pid,
#'                           from = from,
#'                            to = to,
#'                            add_access_to = arcticdatautils:::get_token_subject(),
#'                            public = TRUE,
#'                            new_pid = TRUE)
#'}
#' @export
clone_object <- function(pid,
                         from,
                         to,
                         add_access_to,
                         public = FALSE,
                         new_pid = TRUE,
                         new_submitter = NULL,
                         change_origin_node = FALSE,
                         change_auth_node = FALSE) {

    if (!(is.character(pid) & nchar(pid) > 0)) {
        stop("pid must be a string with a non-zero number of characters")
    }

    if (!is(from, "D1Client")) {
        stop("from must be a D1Client (i.e. dataone::D1Client)")
    }

    if (!is(to, "D1Client")) {
        stop("to must be a D1Client (i.e. dataone::D1Client)")
    }

    if (!(is.null(add_access_to) || (is.character(add_access_to) & all(nchar(add_access_to)>0)))) {
        stop("new_submitter must be either NULL or a string with non-zero number of characters")
    }

    if (!is.logical(public)) {
        stop("public must be either TRUE or FALSE")
    }

    if (!is.logical(new_pid)) {
        stop("new_pid must be either TRUE or FALSE")
    }

    if (!(is.null(new_submitter) || (is.character(new_submitter) & nchar(new_submitter)>0))) {
        stop("new_submitter must be either NULL or a string with a non-zero number of characters")
    }

    if (!is.logical(change_origin_node)) {
        stop("change_origin_node must be either TRUE or FALSE")
    }

    if (!is.logical(change_auth_node)) {
        stop("change_auth_node must be either TRUE or FALSE")
    }

    # Get DataObject
    data_obj <- dataone::getDataObject(from, pid)

    # Adujst sysmeta
    if (new_pid) {
        data_obj@sysmeta@identifier <- paste0("urn:uuid:", uuid::UUIDgenerate())
    }

    if (!is.null(new_submitter)) {
        data_obj@sysmeta@submitter <- new_submitter
    }

    if (change_origin_node) {
        data_obj@sysmeta@originMemberNode <- to@mn@identifier
    }

    if (change_auth_node) {
        data_obj@sysmeta@authoritativeMemberNode <- to@mn@identifier
    }

    data_obj@sysmeta@dateUploaded <- as.character(NA)
    data_obj@sysmeta@obsoletes <- as.character(NA)
    data_obj@sysmeta@obsoletedBy <- as.character(NA)

    # Add rights and access to subjects
    if (!is.null(add_access_to)) {
        for (s in add_access_to) {
            data_obj@sysmeta <- datapack::addAccessRule(data_obj@sysmeta, s, "read")
            data_obj@sysmeta <- datapack::addAccessRule(data_obj@sysmeta, s, "write")
            data_obj@sysmeta <- datapack::addAccessRule(data_obj@sysmeta, s, "changePermission")
        }
    }

    if (public == TRUE) {
        data_obj@sysmeta <- datapack::addAccessRule(data_obj@sysmeta, "public", "read")
    }

    # Upload object
    upload_pid <- tryCatch({

        dataone::createObject(x = to@mn,
                              pid = data_obj@sysmeta@identifier,
                              sysmeta = data_obj@sysmeta,
                              dataobj = data_obj@data)

    }, error = function(e) {
        warning(e)
        NULL
    })

    message("Output: ", upload_pid)
    return(upload_pid)
}

#' Clones packages between Dataone Member Nodes.
#'
#' @description This function copies a Data Package from one DataOne member node to another.
#' Note, the dateUploaded, obsoletes, and obsoletedBy fields in the sysmeta will be reset on the cloned object.
#' This will not update the information in the metadata object.  This can also be used
#' to restore an older version of a Package to a member node, provided that the user subsequently
#' obsoletes the version of the package that they used to create the clone.
#'
#' @param resource_map_pid (character) Object pid
#' @param from (D1Client) D1Client to clone objects from.
#' @param to (D1Client) D1Client to clone objects to.
#' @param add_access_to (character, vector) Will give read, write, and changePermission access to all strings in vector. If no additional access is desired, set to \code{NULL}. Note, setting this to \code{NULL} could lead to situations where it is not possible to read, write, or changePermissions on the cloned object.
#' @param public (logical) Optional. Will set public read access.  Defaults to \code{FALSE}.
#' @param new_pid (logical) Optional. Will give the clone a new pid.  Defaults to \code{TRUE}.
#' @param new_submitter (character) Optional. Will change the submitter in the system metadata to given string if present.  Defaults to \code{NULL}.
#' @param clone_children (logical) Optional. Will clone all children recursively if TRUE. Defaults to \code{FALSE}.
#' @param change_origin_node (logical) Optional. Will change the originMemberNode in the system metadata to the cloned member node if TRUE.  Defaults to \code{FALSE}.
#' @param change_auth_node (logical) Optional. Will change the authoritativeMemberNode in the system metadata to the cloned member node if TRUE.  Defaults to \code{FALSE}.
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' # First set up the member nodes we're cloning between (in this example they are the same but could be different)
#' to <- dataone::D1Client("STAGING", "urn:node:mnTestARCTIC")
#' from <- dataone::D1Client("STAGING", "urn:node:mnTestARCTIC")
#'
#' # Choose a package to clone (here a new one is created)
#' package <- arcticdatautils::create_dummy_package(to@@mn)
#'
#' # Clone object
#' cloned_package <- clone_package(resource_map_pid = package$resource_map
#'                           from = from,
#'                            to = to,
#'                            add_access_to = arcticdatautils:::get_token_subject(),
#'                            public = TRUE,
#'                            new_pid = TRUE)
#' }
#' @export
clone_package <- function(resource_map_pid,
                          from,
                          to,
                          add_access_to,
                          public = FALSE,
                          clone_children = FALSE,
                          new_pid = TRUE,
                          new_submitter = NULL,
                          change_origin_node = FALSE,
                          change_auth_node = FALSE) {

    if (!(is.character(resource_map_pid) & nchar(resource_map_pid) > 0)) {
        stop("resource_map_pid must be a string with a non-zero number of characters")
    }

    if (!is(from, "D1Client")) {
        stop("from must be a D1Client (i.e. dataone::D1Client)")
    }

    if (!is(to, "D1Client")) {
        stop("to must be a D1Client (i.e. dataone::D1Client)")
    }

    if (!(is.null(add_access_to) || (is.character(add_access_to) & all(nchar(add_access_to)>0)))) {
        stop("new_submitter must be either NULL or a string with non-zero number of characters")
    }

    if (!is.logical(public)) {
        stop("public must be either TRUE or FALSE")
    }

    if (!is.logical(new_pid)) {
        stop("new_pid must be either TRUE or FALSE")
    }

    if (!(is.null(new_submitter) || (is.character(new_submitter) & nchar(new_submitter)>0))) {
        stop("new_submitter must be either NULL or a string with a non-zero number of characters")
    }

    if (!is.logical(change_origin_node)) {
        stop("change_origin_node must be either TRUE or FALSE")
    }

    if (!is.logical(change_auth_node)) {
        stop("change_auth_node must be either TRUE or FALSE")
    }

    # Initiallize the response object
    response <- list()

    # Get package
    # Try from member node first then coordinating node
    package <- suppressMessages(tryCatch({
        arcticdatautils::get_package(from@mn, resource_map_pid)
    }, warning = function(w) {
        arcticdatautils::get_package(from@cn, resource_map_pid)
    }, error = function(e) {
        stop("\nNo results were found when searching for a package with resource map:\n",
             resource_map_pid,
             "\nat the mn:\n",
             from@mn@identifier,
             "\nor the cn:\n",
             from@cn@identifier,
             "\nThis can be caused by a mis-typed PID, the resource map not existing, or by not having appropriate access to read the resource map.")
    }))

    # Clone metadata:
    message("\nCloning metadata: ", package$metadata)
    new_eml_pid <- clone_object(pid = package$metadata,
                                from = from,
                                to = to,
                                add_access_to = add_access_to,
                                public = public,
                                new_pid = new_pid,
                                new_submitter = new_submitter,
                                change_origin_node = change_origin_node,
                                change_auth_node = change_auth_node)

    if (is.null(new_eml_pid)) {
        stop("Metadata could not be cloned.")
    }
    response[["metadata"]] <- new_eml_pid

    # Clone data:
    new_data_pids <- unlist(lapply(package$data, function(x) {
        message("\nCloning data object: ", x)
        clone_object(pid = x,
                     from = from,
                     to = to,
                     add_access_to = add_access_to,
                     public = public,
                     new_pid = new_pid,
                     new_submitter = new_submitter,
                     change_origin_node = change_origin_node,
                     change_auth_node = change_auth_node)
    }))

    if (is.null(new_data_pids)) {
        new_data_pids = character(0)
    }
    response[["data"]] <- new_data_pids

    # Clone Children:
    new_child_pids = NULL
    if (clone_children) {
        new_child_pids <- unlist(lapply(package$child_packages, function(x) {
            message("\nCloning child package: ", x)
            cloned_child <- clone_package(resource_map_pid = x,
                                          from = from,
                                          to = to,
                                          add_access_to = add_access_to,
                                          public = public,
                                          clone_children = clone_children,
                                          new_pid = new_pid,
                                          new_submitter = new_submitter,
                                          change_origin_node = change_origin_node,
                                          change_auth_node = change_auth_node)
            cloned_child$resource_map
        }))
        if (is.null(new_child_pids)) {
            new_child_pids = character(0)
        }
        response[["child_packages"]] <- new_child_pids
    }

    # Create resource map
    if (new_pid) {
        message("\nCreating a resource map\n")
        new_resource_map_pid <- arcticdatautils::create_resource_map(mn = to@mn,
                                                                     metadata_pid = new_eml_pid,
                                                                     data_pids = new_data_pids,
                                                                     child_pids = new_child_pids)
    } else {
        message("\nCloning resource map: ", resource_map_pid)
        new_resource_map_pid <- clone_object(pid = resource_map_pid,
                                             from = from,
                                             to = to,
                                             add_access_to = add_access_to,
                                             public = public,
                                             new_pid = new_pid,
                                             new_submitter = new_submitter,
                                             change_origin_node = change_origin_node,
                                             change_auth_node = change_auth_node)
    }
    response[["resource_map"]] <- new_resource_map_pid

    return(response)
}