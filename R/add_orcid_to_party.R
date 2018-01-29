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

