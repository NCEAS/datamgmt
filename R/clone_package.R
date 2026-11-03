is_blank_pid <- function(x) {
    length(x) != 1 || is.na(x) || !nzchar(x)
}

# Retry system-metadata lookup for an object just uploaded to a Member Node.
get_recent_system_metadata <- function(mn, pid, timeout = 30, interval = 1) {
    deadline <- Sys.time() + timeout
    last_error <- NULL

    repeat {
        sysmeta <- tryCatch(
            dataone::getSystemMetadata(mn, pid),
            warning = function(w) {
                last_error <<- conditionMessage(w)
                NULL
            },
            error = function(e) {
                last_error <<- conditionMessage(e)
                NULL
            }
        )

        if (!is.null(sysmeta)) {
            return(sysmeta)
        }

        if (Sys.time() >= deadline) {
            stop("System metadata for newly uploaded PID ", pid,
                 " was not available within ", timeout, " seconds",
                 if (!is.null(last_error)) paste0(": ", last_error))
        }

        Sys.sleep(interval)
    }
}

# Sets old_pid obsoletedBy new_pid and new_pid obsoletes old_pid, only if both exist on mn
# and neither link is already set. Never errors; warns if the link can't be made.
link_versions <- function(mn, old_pid, new_pid, recently_uploaded_pid) {
    tryCatch({
        if (identical(old_pid, recently_uploaded_pid)) {
            # A successor may not have been cloned yet. Leave the chain unlinked;
            # cloning that successor later will link it to this object.
            new_sysmeta <- suppressWarnings(tryCatch(
                dataone::getSystemMetadata(mn, new_pid),
                error = function(e) NULL
            ))
            if (is.null(new_sysmeta)) {
                return(invisible(FALSE))
            }
            old_sysmeta <- get_recent_system_metadata(mn, old_pid)
        } else {
            old_sysmeta <- dataone::getSystemMetadata(mn, old_pid)
            new_sysmeta <- get_recent_system_metadata(mn, new_pid)
        }

        if (!is_blank_pid(old_sysmeta@obsoletedBy) || !is_blank_pid(new_sysmeta@obsoletes)) {
            return(invisible(FALSE))
        }

        new_sysmeta@obsoletes <- old_pid
        dataone::updateSystemMetadata(mn, new_pid, new_sysmeta)
        old_sysmeta@obsoletedBy <- new_pid
        dataone::updateSystemMetadata(mn, old_pid, old_sysmeta)
        message("Linked version chain: ", old_pid, " -> ", new_pid)
        invisible(TRUE)
    }, error = function(e) {
        # Expected when the other version hasn't been cloned to this node yet.
        warning("Could not link ", old_pid, " -> ", new_pid, " at ",
                mn@identifier, ": ", conditionMessage(e), call. = FALSE)
        invisible(FALSE)
    })
}

#' Clone objects between DataONE Member Nodes
#'
#' Clones objects between DataONE Member Nodes.
#' Note:
#' - the dateUploaded field in the sysmeta will be reset on the cloned object.
#' - the obsoletes and obsoletedBy fields are carried over from the original object (preserving the version chain)
#'   unless \code{new_pid = TRUE}, in which case they are reset.
#' - if \code{new_pid = FALSE} and the object obsoletes a previous version, the previous version on the
#'   \code{to} node will have its obsoletedBy set to the cloned object (if it is currently empty and writable).
#' - the limit of the file size is set at a default of 1TB
#'
#' @param pid (character) Object PID.
#' @param from (D1Client) D1Client to clone objects from. (Objects must be public)
#' @param to (D1Client) D1Client to clone objects to. (Token must be set for this node)
#' @param add_access_to (character, vector) Will give read, write, and changePermission access to all strings in vector.
#'   If no additional access is desired, set to \code{NULL}. Note: setting this to \code{NULL} could lead to situations where
#'   it is not possible to read, write, or changePermission on the cloned object.
#' @param change_auth_node (logical) Will change the authoritativeMemberNode in the system metadata to the cloned member node
#'   if \code{TRUE}. Setting this to \code{TRUE} will allow you to edit the package after cloning.
#'   Setting this to \code{FALSE} syncs the system metadata with the package on the Authoritative Member Node,
#'   and will only allow DataONE admins with special privileges to edit the package system metadata.
#' @param public (logical) Optional. Will set public read access. Defaults to \code{FALSE}.
#' @param new_pid (logical) Optional. Will give the clone a new PID. Defaults to \code{TRUE}.
#'   Set to \code{FALSE} to keep the original PID and carry over obsoletes/obsoletedBy.
#'
#' @return (character) PID of cloned object. \code{NULL} if could not clone.
#'
#' @importFrom uuid UUIDgenerate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First set up the member nodes we're cloning between
#' # (in this example they are the same but could be different)
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
#'                            change_auth_node = TRUE,
#'                            public = TRUE,
#'                            new_pid = TRUE)
#'}
clone_object <- function(pid,
                         from,
                         to,
                         add_access_to,
                         change_auth_node,
                         public = FALSE,
                         new_pid = TRUE) {

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
        stop("add_access_to must be either NULL or a string with non-zero number of characters")
    }

    if (!is.logical(public)) {
        stop("public must be either TRUE or FALSE")
    }

    if (!is.logical(new_pid)) {
        stop("new_pid must be either TRUE or FALSE")
    }

    if (!is.logical(change_auth_node)) {
        stop("change_auth_node must be either TRUE or FALSE")
    }

    # Get DataObject
    data_obj <- dataone::getDataObject(from, pid, limit = "1TB")
    prev_pid <- data_obj@sysmeta@obsoletes
    next_pid <- data_obj@sysmeta@obsoletedBy

    # Adjust sysmeta
    if (new_pid) {
        data_obj@sysmeta@identifier <- paste0("urn:uuid:", uuid::UUIDgenerate())
    }

    if (change_auth_node) {
        data_obj@sysmeta@authoritativeMemberNode <- to@mn@identifier
    }

    data_obj@sysmeta@dateUploaded <- as.character(NA)

    # DataONE requires version links to be added after object creation.
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

    # Reuse an object with the same PID already on the destination. This is
    # important for packages that share data objects across versions.
    object_exists_on_target <- function() {
        isTRUE(arcticdatautils::object_exists(to@mn, data_obj@sysmeta@identifier))
    }

    if (!new_pid && object_exists_on_target()) {
        message("Reusing existing object: ", data_obj@sysmeta@identifier)
        upload_pid <- data_obj@sysmeta@identifier
    } else {
        # If another process uploads the same PID after the existence check,
        # treat the resulting create conflict as a reuse if the object is now present.
        upload_pid <- tryCatch({
            dataone::createObject(x = to@mn,
                                  pid = data_obj@sysmeta@identifier,
                                  sysmeta = data_obj@sysmeta,
                                  dataobj = data_obj@data)

        }, error = function(e) {
            if (!new_pid && object_exists_on_target()) {
                message("Reusing existing object: ", data_obj@sysmeta@identifier)
                return(data_obj@sysmeta@identifier)
            }
            warning(e)
            NULL
        })
    }

    # The object is created without version links (avoids validation errors when the
    # neighbouring version doesn't exist yet); links are added only to versions present on `to`.
    if (!new_pid && !is.null(upload_pid)) {
        if (!is_blank_pid(prev_pid)) {
            link_versions(to@mn, old_pid = prev_pid, new_pid = upload_pid,
                          recently_uploaded_pid = upload_pid)
        }

        if (!is_blank_pid(next_pid)) {
            link_versions(to@mn, old_pid = upload_pid, new_pid = next_pid,
                          recently_uploaded_pid = upload_pid)
        }
    }

    message("Output: ", upload_pid)
    return(upload_pid)
}


#' Clone packages between DataONE Member Nodes
#'
#' This function copies a data package from one DataONE Member Node to another.
#' Note: the dateUploaded field in the sysmeta will be reset on the cloned object. The obsoletes and obsoletedBy
#' fields are reset only when \code{new_pid = TRUE}; otherwise they are carried over (see [clone_object()]).
#' When \code{new_pid = FALSE}, objects already present on the destination Member Node are reused
#' rather than uploaded again. If a data object cannot be cloned or reused, the function stops
#' instead of creating a package with missing data objects.
#' This will not update the information in the metadata object. This can also be used
#' to restore an older version of a package to a Member Node, provided that the user subsequently
#' obsoletes the version of the package that they used to create the clone.
#'
#' @param resource_map_pid (character) PID for the package resource map.
#' @param from (D1Client) D1Client to clone package from. (Package must be public)
#' @param to (D1Client) D1Client to clone package to. (Token must be set for this node)
#' @param add_access_to (character) Will give read, write, and changePermission access to all strings in vector.
#'   If no additional access is desired, set to \code{NULL}. Note: setting this to \code{NULL} could lead to situations where
#'   it is not possible to read, write, or changePermissions on the cloned object.
#' @param change_auth_node (logical) Will change the authoritativeMemberNode in the system metadata to the cloned member node
#'   if \code{TRUE}. Setting this to \code{TRUE} will allow you to edit the package after cloning. Setting this to \code{FALSE}
#'   syncs the system metadata with the package on the Authoritative Member Node, and will only allow DataONE admins with
#'   special privileges to edit package system metadata.
#' @param public (logical) Optional. Will set public read access. Defaults to \code{FALSE}.
#' @param new_pid (logical) Optional. Will give the clone a new PID. Defaults to \code{TRUE}.
#' @param clone_children (logical) Optional. Will clone all children recursively if \code{TRUE}. Defaults to \code{FALSE}.
#'
#' @export
#'
#' @seealso [copy_package()]
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' # First set up the member nodes we're cloning between
#' # (in this example they are the same but could be different)
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
#'                            change_auth_node = TRUE,
#'                            public = TRUE,
#'                            new_pid = TRUE)
#' }
clone_package <- function(resource_map_pid,
                          from,
                          to,
                          add_access_to,
                          change_auth_node,
                          public = FALSE,
                          clone_children = FALSE,
                          new_pid = TRUE) {

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
        stop("add_access_to must be either NULL or a string with non-zero number of characters")
    }

    if (!is.logical(public)) {
        stop("public must be either TRUE or FALSE")
    }

    if (!is.logical(new_pid)) {
        stop("new_pid must be either TRUE or FALSE")
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
        # ideally the query to get pakcage pids would include datasource:from@mn@identifier but not sure it is worth rewriting the code.
        arcticdatautils::get_package(from@cn, resource_map_pid)
    }, error = function(e) {
        stop("\nNo results were found when searching for a package with resource map:\n",
             resource_map_pid,
             "\nat the mn:\n",
             from@mn@identifier,
             "\nor the cn:\n",
             from@cn@identifier,
             "\nThis can be caused by a mis-typed PID, the resource map not existing, the resource map not being synced with the mn or cn, or by not having appropriate access to read the resource map.  ")
    }))

    # Clone metadata:
    message("\nCloning metadata: ", package$metadata)
    new_eml_pid <- clone_object(pid = package$metadata,
                                from = from,
                                to = to,
                                add_access_to = add_access_to,
                                change_auth_node = change_auth_node,
                                public = public,
                                new_pid = new_pid)

    if (is.null(new_eml_pid)) {
        stop("Metadata could not be cloned.")
    }
    response[["metadata"]] <- new_eml_pid

    # Clone data:
    cloned_data <- lapply(package$data, function(x) {
        message("\nCloning data object: ", x)
        clone_object(pid = x,
                     from = from,
                     to = to,
                     add_access_to = add_access_to,
                     public = public,
                     new_pid = new_pid,
                     change_auth_node = change_auth_node)
    })

    failed_data <- package$data[vapply(cloned_data, is.null, logical(1))]
    if (length(failed_data) > 0) {
        stop("Could not clone or reuse data object(s): ",
             paste(failed_data, collapse = ", "))
    }

    new_data_pids <- unlist(cloned_data, use.names = FALSE)

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
                                             change_auth_node = change_auth_node)
    }

    arcticdatautils::set_access(to@mn,
                                pids = new_resource_map_pid,
                                subjects = add_access_to)

    response[["resource_map"]] <- new_resource_map_pid



    return(response)
}


#' Copy packages between DataONE Member Nodes with new identifiers
#'
#' This function is a convenience wrapper around [clone_package()] that
#' copies a package rather than cloning it. The distinction is that new PIDs will
#' always be generated, and the system metadata will reflect a stand-alone package
#' rather than a clone. This function copies a data package from one DataONE Member Node to another,
#' with new identifiers This can also be used to restore an older version of a package
#' to a Member Node, provided that the user subsequently obsoletes the version of
#' the package that they used to create the copy using [obsolete_package()].
#'
#' @param resource_map_pid (character) Object pid
#' @param from (D1Client) D1Client to clone package from. (Token must be set for this node)
#' @param to (D1Client) D1Client to clone package to. (Token must be set for this node)
#' @param public (logical) Optional. Will set public read access. Defaults to \code{FALSE}.
#' @param clone_children (logical) Optional. Will clone all children recursively if TRUE. Defaults to \code{FALSE}.
#'
#' @export
#'
#' @seealso [clone_package()] [obsolete_package()]
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' # First set up the member nodes we're copying between
#' # (in this example they are the same but could be different)
#' to <- dataone::D1Client("STAGING", "urn:node:mnTestARCTIC")
#' from <- dataone::D1Client("STAGING", "urn:node:mnTestARCTIC")
#'
#' # Choose a package to copy (here a new one is created)
#' package <- arcticdatautils::create_dummy_package(to@@mn)
#'
#' copied_package <- clone_package(resource_map_pid = package$resource_map,
#'                                 from = from,
#'                                 to = to)
#' }
copy_package <- function(resource_map_pid,
                         from,
                         to,
                         public = FALSE,
                         clone_children = FALSE) {
    if (!arcticdatautils::is_token_set(to@mn)) {
        stop("No token is set for member node: ", to@mn@identifier)
    }

    # Set hard-coded arguments (distinctions between copying and cloning)
    add_access_to <- arcticdatautils:::get_token_subject()
    change_auth_node <- TRUE
    new_pid <- TRUE

    response <- clone_package(resource_map_pid,
                              from,
                              to,
                              add_access_to,
                              change_auth_node,
                              public,
                              clone_children,
                              new_pid)

    return(response)
}
