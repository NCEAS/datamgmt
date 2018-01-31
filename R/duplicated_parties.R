#' duplicated_parties
#'
#' This function will replace all duplicated ResponsibleParty occurrences in an EML.
#' The function searches through all creators, contacts, metadataProviders, associatedParties,and publishers.
#' All unique surName, givenName occurances are recorded.
#' If a surName, givenName occurance is duplicated, every occurance after the first is replaced with the reference id of the first occurance.
#'
#' @param eml EML script to modify
#'
#' @keywords EML ResponsibleParty creator contact metadataProvider associatedParty publisher
#
#' @return Modified EML
#' @example
#' eml <- duplicated_parties(eml)
duplicated_parties <- function(eml) {

    parties <- c("creator","contact","metadataProvider","associatedParty","publisher")
    namesdf <- data.frame()

    for (party in parties){
        switch(party,
               "creator" = {
                   ResponsibleParty = eml@dataset@creator
               },
               "contact" = {
                   ResponsibleParty = eml@dataset@contact
               },
               "metadataProvider" = {
                   ResponsibleParty = eml@dataset@metadataProvider
               },
               "associatedParty" = {
                   ResponsibleParty = eml@dataset@associatedParty
               },
               "publisher" = {
                   ResponsibleParty = eml@dataset@publisher
               })
        for(i in seq_along(ResponsibleParty)){
            id <- ResponsibleParty[[i]]@id@.Data
            individualNameList <- ResponsibleParty[[i]]@individualName
            for (j in seq_along(individualNameList)){
                surName <- individualNameList[[j]]@surName@.Data
                givenNameList <- individualNameList[[j]]@givenName
                for (k in seq_along(givenNameList)){
                    givenName <- givenNameList[[k]]@.Data
                    if(length(id)==0){
                        id <-  paste0(surName,"_",givenName)
                    }
                    new_party <- data.frame(surName,givenName,id)
                    match_row <- which(surName == namesdf$surName & givenName == namesdf$givenName)
                    if (length(match_row)==0){
                        namesdf <- rbind(namesdf,new_party)
                    }else
                    {
                        switch(party,
                               "creator" = {
                                   eml@dataset@creator[[i]] = new(party,reference=as.character(namesdf$id[match_row]))
                               },
                               "contact" = {
                                   eml@dataset@contact[[i]] = new(party,reference=as.character(namesdf$id[match_row]))

                               },
                               "metadataProvider" = {
                                   eml@dataset@metadataProvider[[i]] = new(party,reference=as.character(namesdf$id[match_row]))

                               },
                               "associatedParty" = {
                                   eml@dataset@associatedParty[[i]] = new(party,reference=as.character(namesdf$id[match_row]))
                               },
                               "publisher" = {
                                   eml@dataset@publisher[[i]] = new(party,reference=as.character(namesdf$id[match_row]))

                               })
                        cat("\nreference id",
                            as.character(namesdf$id[match_row]),
                            "set for",
                            party,
                            as.character(namesdf$givenName[match_row]),
                            as.character(namesdf$surName[match_row]),
                            "\n")
                    }
                }
            }
        }
    }
    invisible(eml) #returns full eml
}
