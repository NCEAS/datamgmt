#' add_creator_id
#'
#' This function allows you to add an ORCID or reference ID to a creator in EML.
#' @param eml EML script to modify
#' @param orcid ORCID in the format 'https://orcid.org/WWWW-XXXX-YYYY-ZZZZ'
#' @param id reference ID. Character string to set reference ID for creators with additional roles (i.e. metadataProvider, etc.)
#' @param surname creator surname (last name), defaults to first creator if not specified. Not case-sensitive.
#' @keywords eml creator orcid id
#' @details All prameters other than the EML are optional, but since the point of the function is to modify either the orcid, ref id, or both, you better specify at least one. Requires the crayon package.
#' @export
#' @examples
#' add_creator_id(eml2, orcid = "https://orcid.org/WWWW-XXXX-YYYY-ZZZZ")
#'

#function for adding orcid or reference id to creator
add_creator_id <- function(eml,
                           orcid = NULL,
                           id = NULL,
                           surname = NULL) { #not sensitive to capitalization
    library(crayon)

    #grab creators and save to new variable
    creatorList <- eml@dataset@creator@.Data

    #determine surname position to access correct creator
    #if none are specified, the first creator will be modified
    if(is.null(surname == TRUE)){
        cat(green("Since surname was not specified, the first creator entry will be modified. "))
        pos <- 1
    } else {
        #make vector of creator surnames
        surNames <- rep(NA, times = length(creatorList))
        for(i in 1:length(creatorList)){
            creator1 <- creatorList[[i]]
            surName1 <- creator1@individualName@.Data[[1]]@surName@.Data
            surNames[i] <- surName1
        }

        #if specified surname exists, then edit entry (caps-proof)
        surname_u <- toupper(surname)
        surNames_u <- toupper(surNames)
        if(surname_u %in% surNames_u == TRUE){
            pos <- which(surNames_u == surname_u)
        } else {
            stop(red("Surname not found. Check your spelling."))
        }
    }

    #add orcid if specified
    if(is.null(orcid) == FALSE){
        creatorList[[pos]]@userId <- c(new('userId',
                                           .Data = orcid,
                                           directory = "https://orcid.org"))
    }

    #add reference id if specified
    if(is.null(id) == FALSE){
        creatorList[[pos]]@id[[1]] <- as.character(id)
    }

    #add updated creatorList back into eml
    eml@dataset@creator@.Data <- creatorList

    cat(green("The following entry has been changed:"))
    print(creatorList[[pos]]) #prints changed entry
    return(invisible(eml)) #returns full eml
}
