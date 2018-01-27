#' add_party_id
#'
#' This function allows you to add an ORCID or reference ID to a creator in EML.
#' @param eml EML script to modify
#' @param orcid ORCID in the format 'https://orcid.org/WWWW-XXXX-YYYY-ZZZZ'
#' @param id reference ID. Character string to set reference ID for creators with additional roles (i.e. metadataProvider, etc.)
#' @param surname creator surname (last name), defaults to first creator if not specified. Not case-sensitive.
#'
#' @keywords eml creator orcid id
#'
#' @details All prameters other than the EML are optional, but since the point of the function is to modify either the orcid, ref id, or both, you better specify at least one. Requires the crayon package.
#'
#' @export
#'
#' @examples
#' library(dataone)
#' library(arcticdatautils)
#' library(EML)
#'
#' cnTest <- dataone::CNode('STAGING')
#' mnTest <- dataone::getMNode(cnTest,'urn:node:mnTestARCTIC')
#' eml_pid <- arcticdatautils::create_dummy_metadata(mnTest)
#' eml1 <- EML::read_eml(rawToChar(getObject(mnTest, eml_pid)))
#' add_creator_id(eml1, orcid = "https://orcid.org/WWWW-XXXX-YYYY-ZZZZ")

add_party_id <- function(eml,
                         surName,
                         givenName,
                         orcid = NULL,
                         id = NULL,
                         id_keep = NULL,
                         replace= F) {

    if (is.null(orcid) & is.null(id)){
        stop("either orcid or id must be specified")
    }
    if (!is.null(orcid) & !is.null(id)){
        stop("only one of either orcid or id must be specified")
    }
    if (!is.null(id) & is.null(id_keep)){
        stop("id_keep can't be NULL, must specifiy which instance of party to keep")
    }
    parties <- c("creator","contact","metadataProvider","associatedParty","publisher")
    if (!is.null(id_keep)){
        if (!(id_keep %in% parties)){
            stop("id_keep must be of one of 'creator','contact','metadataProvider','associatedParty','publisher'")
        }
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
                            if(!is.null(orcid)){
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
                            if(!is.null(id)){

                                if(id_keep==party){
                                    cat("\nneed to make a test here to make sure id_keep == id\n")
                                }else{


                                    if(length(ResponsibleParty[[i]]@references@.Data)==0 | replace==TRUE){
                                        switch(party,
                                               "creator" = {
                                                   eml@dataset@creator[[i]] = new(party,reference=id)
                                               },
                                               "contact" = {
                                                   eml@dataset@contact[[i]] = new(party,reference=id)

                                               },
                                               "metadataProvider" = {
                                                   eml@dataset@metadataProvider[[i]] = new(party,reference=id)

                                               },
                                               "associatedParty" = {
                                                   eml@dataset@associatedParty[[i]] = new(party,reference=id)
                                               },
                                               "publisher" = {
                                                   eml@dataset@publisher[[i]] = new(party,reference=id)

                                               })
                                        cat('\nreference id set for',party,'\n')}
                                }
                            }

                        }
                    }
                }
            }
        }
    }

    return(invisible(eml)) #returns full eml
}
