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
                    if(length(id_value)==0){
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

#' add_orcid_to_party
#'
#' This function allows you to add an ORCID to a ResponsibleParty in an EML
#' @param eml EML script to modify
#' @param surName surName of person to assign ORCID to
#' @param givenName givenName of person to assign ORCID to
#' @param orcid ORCID in the format 'https://orcid.org/WWWW-XXXX-YYYY-ZZZZ'
#' @param replace (TRUE/FALSE) If replace is set to FALSE, ORCIDs that already exist will not be overwritten
#'
#' @keywords EML ORCID
#' @return Modified EML

add_orcid_to_party <- function(eml,
                               surName,
                               givenName,
                               orcid,
                               replace= F) {

    parties <- c("creator","contact","metadataProvider","associatedParty","publisher")

    if (!grepl("https://orcid.org/[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}-[[:digit:]]{4}",orcid)){
        stop("orcid not of form 'https://orcid.org/WWWW-XXXX-YYYY-ZZZZ'")
    }

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
            individualNameList <- ResponsibleParty[[i]]@individualName
            for (j in seq_along(individualNameList)){
                if (individualNameList[[j]]@surName@.Data == surName){
                    givenNameList <- individualNameList[[j]]@givenName
                    for (k in seq_along(givenNameList)){
                        if (givenNameList[[k]] == givenName){
                            if(length(ResponsibleParty[[i]]@userId)==0 | replace==TRUE){
                                switch(party,
                                       "creator" = {
                                           eml@dataset@creator[[i]]@userId = c(new('userId',
                                                                                   .Data = orcid,
                                                                                   directory = "https://orcid.org"))
                                       },
                                       "contact" = {
                                           eml@dataset@contact[[i]]@userId = c(new('userId',
                                                                                   .Data = orcid,
                                                                                   directory = "https://orcid.org"))
                                       },
                                       "metadataProvider" = {
                                           eml@dataset@metadataProvider[[i]]@userId = c(new('userId',
                                                                                            .Data = orcid,
                                                                                            directory = "https://orcid.org"))
                                       },
                                       "associatedParty" = {
                                           eml@dataset@associatedParty[[i]]@userId = c(new('userId',
                                                                                           .Data = orcid,
                                                                                           directory = "https://orcid.org"))
                                       },
                                       "publisher" = {
                                           eml@dataset@publisher[[i]]@userId = c(new('userId',
                                                                                     .Data = orcid,
                                                                                     directory = "https://orcid.org"))
                                       })
                                cat('\norcid set for',party,'\n')}
                            else{
                                warning("Orcid already exists for ",party,". If would like to replace existing set replace to TRUE",call.=F)
                            }
                        }
                    }
                }
            }
        }
    }

    invisible(eml)
}

