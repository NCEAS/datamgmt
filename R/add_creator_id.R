#' Add an ORCID or reference ID to a creator
#'
#' This function allows you to add an ORCID or reference ID to a creator in EML.
#'
#' The function invisibly returns the full EML, which
#' can be saved to a variable. It also prints the changed creator
#' entry so that it is easy to check that the appropriate change was
#' made. In addition to the EML, at least one of either the ORCID or
#' reference id is required.
#'
#' Note: Updated creator information cannot be used as a
#' reference for associatedParties because the extra "role"
#' field is required. Also, the function does not (yet) account
#' for cases in which multiple creators have the same surname.
#'
#' @param eml EML script to modify.
#' @param orcid ORCID in the format 'https://orcid.org/WWWW-XXXX-YYYY-ZZZZ'.
#' @param id Reference ID. Character string to set reference ID for creators with additional roles (e.g., metadataProvider).
#' @param surname Creator surname. Defaults to first creator if not specified. Not case-sensitive.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eml_path <- file.path(system.file(package = "datamgmt"), "dummy_meta_full.xml")
#' eml <- EML::read_eml(eml_path_original)
#' add_creator_id(eml, orcid = "https://orcid.org/WWWW-XXXX-YYYY-ZZZZ")
#'
#' eml <- eml %>%
#'            add_creator_id(surname = "high-stakes",
#'                           orcid = "https://orcid.org/0000-1234-5678-4321",
#'                           id = "henrietta")
#'
#' #use references to add updated contact info to Henrietta's other roles
#' eml@dataset@contact[[1]] <- new('contact', reference = "henrietta")
#' eml@dataset@metadataProvider[[1]] <- new('metadataProvider', reference = "henrietta")
#' }
add_creator_id <- function(eml,
                           orcid = NULL,
                           id = NULL,
                           surname = NULL) { #not sensitive to capitalization

    if (!requireNamespace("crayon")) {
   stop(call. = FALSE,
       "The crayon package is required. Please install it and try again.")
 }

    #variable checks:
    for (args in c(orcid, id, surname)) {
        if (!(is.null(args) | is.character(args))) {
            stop(paste(args, "must be a character string."))
        }
    }
    if (is.null(orcid) & is.null(id)) {
        stop("Need either an orcid or id.")
    }

    #grab creators and save to new variable
    creatorList <- eml@dataset@creator

    #determine surname position to access correct creator
    #if none are specified, the first creator will be modified
    if (is.null(surname)) {
        cat(crayon::green("Since surname was not specified, the first creator entry will be modified. "))
        pos <- 1
    } else {
        #make vector of creator surnames
        surNames <- rep(NA, times = length(creatorList))
        for (i in 1:seq_along(creatorList)) {
            creator1 <- creatorList[[i]]
            surName1 <- creator1@individualName[[1]]@surName@.Data
            surNames[i] <- surName1
        }

        #if specified surname exists, then edit entry (caps-proof)
        surname_u <- toupper(surname)
        surNames_u <- toupper(surNames)
        if (surname_u %in% surNames_u) {
            pos <- which(surNames_u == surname_u)
        } else {
            stop(crayon::red("Surname not found. Check your spelling."))
        }
    }

    #add orcid if specified
    if (!is.null(orcid)) {
        creatorList[[pos]]@userId <- c(methods::new('userId',
                                           .Data = orcid,
                                           directory = "https://orcid.org"))
    }

    #add reference id if specified
    if (!is.null(id)) {
        creatorList[[pos]]@id[[1]] <- as.character(id)
    }

    #add updated creatorList back into eml
    eml@dataset@creator@.Data <- creatorList

    cat(crayon::green("The following entry has been changed:"))
    print(creatorList[[pos]]) #prints changed entry
    invisible(eml) #returns full eml
}
