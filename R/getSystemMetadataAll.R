#' Get System Metadata for all versions
#'
#' @description This function combines getSystemMetadata and solr query to produce
#' a data frame with any combination of getSystemMetadata and query fields for all
#' versions of the given PID. If no fields are specified, the resulting data frame
#' will contain all fields possible from getSystemMetadata and solr query.
#'
#' @param node (character) Specify the node where the object should be searched for.
#' @param object_pid (character) PID for the object that you want to return information about.
#' @param list_of_fields (list) List of fields that you want returned in the data frame.
#' "Id" is added automatically.
#'
#'
#' @author Sharis Ochs

getSystemMetadataAll <- function(node, object_pid, list_of_fields = "all") {

    ## Checks =========================
    # Check that object exist
    if (!(arcticdatautils::object_exists(node, object_pid))) {
        stop('Object does not exist on specified node ')
    }

    # get all solr fields
    adc_solr <- httr::GET("https://arcticdata.io/metacat/d1/mn/v2/query/solr")
    suppressMessages(suppressWarnings(adc_solr<- adc_solr %>%
        stringr::str_extract_all("name>.*<") %>%
        unlist() %>%
        stringr::str_replace_all("name>|<", "")))

    # fields in getSystemMetadata but not solr query
    gsm_fields <- c("serialVersion", 'accessPolicy', 'preferredNodes', 'blockedNodes', 'archived', 'dateSysMetadataModified', 'originMemberNode', 'authoritativeMemberNode')


    suppressWarnings(if (list_of_fields != "all"){
        ## Check that all specified fields are valid
        for (j in 1:length(list_of_fields)){
            if (!(list_of_fields[j] %in% gsm_fields | list_of_fields[j] %in% adc_solr)){
                stop('Please enter a valid field ')
            }
        }
    })

    ## Initialize ===========================
    # Check how many versions there are
    versions<- arcticdatautils::get_all_versions(node, object_pid)
    n<- length(versions)

    # Initialize data frame of fields only in getSystemMetadata
    sysmeta_table <- data.frame(id = rep("NA", n),
                                serialVersion = rep("NA", n),
                                accessPolicy = rep("NA", n),
                                preferredNodes = rep("NA", n),
                                blockedNodes = rep("NA", n),
                                archived = rep("NA", n),
                                dateSysMetadataModified = rep("NA", n),
                                originMemberNode = rep("NA", n),
                                authoritativeMemberNode = rep("NA", n),
                                stringsAsFactors = F)

    # Case 1 ========================
    # Case: if no fields are specified
    suppressWarnings(if (list_of_fields == "all"){

        # Query all fields
        inside_q <- paste(versions, collapse="\"+OR+\"")
        q <- paste0("documents:\"", inside_q, "\"")
        df_query <- dataone::query(node, list(q=q,
                                     fl= "*",
                                     rows="100"),
                                     as = "data.frame")

        # Then getSystemMetadata
        for (i in 1:n){
            sysmeta<- dataone::getSystemMetadata(node, versions[i])
            sysmeta_table$id[i] <- sysmeta@identifier
            sysmeta_table$serialVersion[i] <- sysmeta@serialVersion
            sysmeta_table$accessPolicy[i] <- I(list((sysmeta@accessPolicy)))
            if (length(sysmeta@preferredNodes)>0){
                sysmeta_table$preferredNodes[i] <- list(sysmeta@preferredNodes)
            }
            if (length(sysmeta@blockedNodes)>0){
                sysmeta_table$blockedNodes[i] <- list(sysmeta@blockedNodes)
            }
            sysmeta_table$archived[i] <- sysmeta@archived
            sysmeta_table$dateSysMetadataModified[i] <- sysmeta@dateSysMetadataModified
            sysmeta_table$originMemberNode[i] <- sysmeta@originMemberNode
            sysmeta_table$authoritativeMemberNode[i] <- sysmeta@authoritativeMemberNode
        }

        # Merge data frames
        final_df <- merge(df_query, sysmeta_table, by = "id")
    })


    # Case 2 ========================
    # Else: Fields are specified
    suppressWarnings(if (list_of_fields != "all") {
        # split fields into those in solr query and those in getSystemMetadata
        query_fields <- list_of_fields[which(list_of_fields %in% adc_solr)]
        gsm_fields <- list_of_fields[which(list_of_fields %in% gsm_fields)]

        # If query fields are specified, run query
        if (length(query_fields)>0){
            inside_q <- paste(versions, collapse="\"+OR+\"")
            q <- paste0("documents:\"", inside_q, "\"")
            fl <- paste(list_of_fields, collapse=", ")
            fl_add_id <- paste(c("id", list_of_fields), collapse=", ")
            df_query <- dataone::query(node, list(q=q,
                                       fl= fl_add_id,
                                       rows="100"),
                                       as = "data.frame")
        }

        # If getSystemMetadata fields are specified, getSystemMetadata
        if (length(gsm_fields)>0){
            for (i in 1:n){
                sysmeta<- dataone::getSystemMetadata(node, versions[i])
                sysmeta_table$id[i] <- sysmeta@identifier
                sysmeta_table$serialVersion[i] <- sysmeta@serialVersion
                sysmeta_table$accessPolicy[i] <- I(list((sysmeta@accessPolicy)))
                if (length(sysmeta@preferredNodes)>0){
                    sysmeta_table$preferredNodes[i] <- list(sysmeta@preferredNodes)
                }
                if (length(sysmeta@blockedNodes)>0){
                    sysmeta_table$blockedNodes[i] <- list(sysmeta@blockedNodes)
                }
                sysmeta_table$archived[i] <- sysmeta@archived
                sysmeta_table$dateSysMetadataModified[i] <- sysmeta@dateSysMetadataModified
                sysmeta_table$originMemberNode[i] <- sysmeta@originMemberNode
                sysmeta_table$authoritativeMemberNode[i] <- sysmeta@authoritativeMemberNode
            }
        }


        # Make new data frame with only the desired fields
        sysmeta_table_2 <- sysmeta_table[c("id", gsm_fields)]

        if (length(query_fields)>0 & length(gsm_fields)>0){
            # Merge data frames
            final_df <- merge(df_query, sysmeta_table_2, by = "id")
        }

        if (length(query_fields)>0 & length(gsm_fields)==0){
            final_df <- df_query
        }

        if (length(query_fields)==0 & length(gsm_fields)>0){
            final_df <- sysmeta_table_2
        }
    })

    return(final_df)
}

#example
df<- getSystemMetadataAll(mn, "doi:10.18739/A27D2Q670", c("origin", "submitter", "title", 'accessPolicy'))
