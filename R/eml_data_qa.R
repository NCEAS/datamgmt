# QA for metadata based on EML 2.1.1
# See EML specs at:
# https://knb.ecoinformatics.org/#external//emlparser/docs/eml-2.1.1/
# Also based on the MetaDIG checks found at:
# https://github.com/NCEAS/metadig-engine/tree/master/src/main/resources/checks


#' Check if title is present with sufficient length
#'
#' This function checks if a title is present with the sufficient length.
#' A title should be between 7 and 20 words.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @importFrom methods is
#' @importFrom stringr str_split
#'
#' @noRd
# TODO: check number of words in multiple titles (title[[i]])
qa_title <- function(input) {
    if (methods::is(input, "eml")) {
        title <- input@dataset@title
    } else if (any(c("ListOftitle", "character") %in% class(input))) {
        title <- input
    } else {
        stop("Input should be of class 'eml', 'ListOftitle', or 'character'.")
    }

    if (length(title) == 0) {
        return(list(status = "FAILURE",
                    output = "No title is present."))
    } else {
        # Required minimum word count
        req_min_count <- 5
        # Recommended minimum word count
        rec_min_count <- 7
        # Recommended max word count
        rec_max_count <- 20

        if (length(title) == 1) {
            if (class(input) == "character") { # For class 'eml', is.character(input) returns TRUE; use alternative method instead
                title2 <- title[[1]]
            } else {
                title2 <- title[[1]]@.Data
            }

            word_count <- length(stringr::str_split(title2, "\\s+", simplify = TRUE))

            if (word_count < req_min_count) {
                return(list(status = "FAILURE",
                            output = sprintf("One title is present with %d words. The minimum required word count is %d.",
                                             word_count, req_min_count)))
            } else if (word_count < rec_min_count) {
                return(list(status = "FAILURE",
                            output = sprintf("One title is present with %d words. The minimum recommended word count is %d.",
                                             word_count, rec_min_count)))
            } else if (word_count > rec_max_count) {
                return(list(status = "FAILURE",
                            output = sprintf("One title is present with %d words. The maximum recommended word count is %d.",
                                             word_count, rec_max_count)))
            } else {
                return(list(status = "SUCCESS",
                            output = sprintf("One title is present with %d words.", word_count)))
            }
        } else {
            return(list(status = "SUCCESS",
                        output = sprintf("%d titles are present.", length(title))))
        }
    }
}


#' Check if publication date is present with correct format
#'
#' This function checks if a publication date is present with the
#' correct format (YYYY or YYYY-MM-DD).
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @importFrom lubridate ymd
#' @importFrom methods is
#'
#' @noRd
qa_pubDate <- function(input) {
    if (methods::is(input, "eml")) {
        pubDate <- input@dataset@pubDate@.Data
    } else if (methods::is(input, "pubDate")) {
        pubDate <- input@.Data
    } else if (methods::is(input, "character")) {
        pubDate <- input
    } else {
        stop("Input should be of class 'eml', 'pubDate', or 'character'.")
    }

    # Publication dates should be in YYYY or YYYY-MM-DD format
    if (length(pubDate) == 0) {
        return(list(status = "FAILURE",
                    output = "No publication date is present."))
    } else {
        if (nchar(pubDate) == 4 && is.na(suppressWarnings(as.numeric(pubDate)))) {
            return(list(status = "FAILURE",
                        output = "A publication date is present but should be in YYYY or YYYY-MM-DD format."))
        } else if (nchar(pubDate) == 4 && !is.na(as.numeric(pubDate))) {
            return(list(status = "SUCCESS",
                        output = "A publication date is present with YYYY format."))
        } else if (nchar(pubDate) == 10 && is.na(suppressWarnings(lubridate::ymd(pubDate)))) {
            return(list(status = "FAILURE",
                        output = "A publication date is present but should be in YYYY or YYYY-MM-DD format."))
        } else if (nchar(pubDate) == 10 && !is.na(lubridate::ymd(pubDate))) {
            return(list(status = "SUCCESS",
                        output = "A publication date is present with YYYY-MM-DD format."))
        } else {
            return(list(status = "FAILURE",
                        output = "A publication date is present but should be in YYYY or YYYY-MM-DD format."))
        }
    }
}


#' Check if abstract is present with sufficient length
#'
#' This function checks if an abstract is present with sufficient length.
#' At least 100 words are required.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @importFrom methods is
#' @importFrom stringr str_split
#' @importFrom xml2 as_list
#'
#' @noRd
qa_abstract <- function(input) {
    if (methods::is(input, "eml")) {
        # Usually abstract has a 'para' slot but sometimes not
        if (length(input@dataset@abstract@para) == 0) {
            abstract <- input@dataset@abstract@.Data
            length_abstract <- length(abstract)
        } else {
            # There can be multiple 'para' but this code only extracts the first
            abstract <- unlist(xml2::as_list(input@dataset@abstract@para[[1]]@.Data[[1]]))
            length_abstract <- length(abstract)
        }
    } else if (methods::is(input, "abstract")) {
        if (length(input@para) == 0) {
            abstract <- input@.Data
            length_abstract <- length(abstract)
        } else {
            abstract <- unlist(xml2::as_list(input@para[[1]]@.Data[[1]]))
            length_abstract <- length(abstract)
        }
    } else if (methods::is(input, "character")) {
        abstract <- input
        length_abstract <- length(abstract)
    } else {
        stop("Input should be of class 'eml', 'abstract', or 'character'.")
    }

    if (length_abstract == 0) {
        return(list(status = "FAILURE",
                    output = "An abstract is not present."))
    } else if (length_abstract > 1) {
        return(list(status = "FAILURE",
                    output = "More than one abstract is present. Only one is allowed."))
    } else {
        # Split abstract into words and trim whitespace
        tokens <- trimws(stringr::str_split(abstract, "\\s+")[[1]], which = "both")
        # Remove any blank elements
        tokens <- tokens[tokens != ""]

        if (length(tokens) >= 100) {
            return(list(status = "SUCCESS",
                        output = sprintf("The abstract is present and %d words long.", length(tokens))))
        } else {
            return(list(status = "FAILURE",
                        output = sprintf("The abstract is present but only %d words long. 100 or more words are required.", length(tokens))))
        }
    }
}


#' Check if keywords are present
#'
#' This function checks if keywords are present. At least one
#' keyword is recommended.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @importFrom methods is
#'
#' @noRd
qa_keywordSet <- function(input) {
    if (methods::is(input, "eml")) {
        key <- input@dataset@keywordSet
    } else if (any(c("ListOfkeywordSet", "character") %in% class(input))) {
        key <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfkeywordSet', or 'character'.")
    }

    if (length(key) == 0) {
        return(list(status = "FAILURE",
                    output = "No keywords are present. At least one keyword is recommended."))
    }

    # Count number of keywords if present
    if (class(input) == "character") { # For class 'eml', is.character(input) returns TRUE; use alternative method instead
        length_key <- length(key)
    } else {
        length_key <- length(key[[1]]@keyword)
    }

    if (length_key == 1) {
        return(list(status = "SUCCESS",
                    output = "One keyword is present."))
    } else {
        return(list(status = "SUCCESS",
                    output = sprintf("%d keywords are present.", length_key)))
    }
}


#' Check if data usage rights are present
#'
#' This function checks if data usage rights are present and are either CC-BY or CC-0.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @importFrom methods is
#' @importFrom stringr str_detect
#' @importFrom xml2 as_list
#'
#' @noRd
qa_intellectualRights <- function(input) {
    if (methods::is(input, "eml")) {
        # Usually intellectualRights has a 'para' slot but sometimes not
        if (length(input@dataset@intellectualRights@para) == 0) {
            rights <- input@dataset@intellectualRights@.Data
            length_rights <- length(rights)
        } else {
            # There can be multiple 'para' but this code only extracts the first
            rights <- unlist(xml2::as_list(input@dataset@intellectualRights@para[[1]]@.Data[[1]]))
            length_rights <- length(rights)
        }
    } else if (methods::is(input, "intellectualRights")) {
        if (length(input@para) == 0) {
            rights <- input@.Data
            length_rights <- length(rights)
        } else {
            rights <- unlist(xml2::as_list(input@para[[1]]@.Data[[1]]))
            length_rights <- length(rights)
        }
    } else if (methods::is(input, "character")) {
        rights <- input
        length_rights <- length(rights)
    } else {
        stop("Input should be of class 'eml', 'intellectualRights', or 'character'.")
    }

    # Most data packages should be CC-BY or CC-0
    phrases <- c("http[s]*://creativecommons.org/licenses/by/4.0", "http[s]*://creativecommons.org/publicdomain/zero/1.0")

    if (length_rights == 0) {
        return(list(status = "FAILURE",
                    output = "Intellectual rights are not present."))
    } else if (length_rights > 1) {
        return(list(status = "FAILURE",
                    output = "Multiple intellectual rights are present. Only one is allowed."))
    } else {
        if (stringr::str_detect(rights, phrases[[1]])) {
            return(list(status = "SUCCESS",
                        output = "Intellectual rights are present as a CC-BY license."))
        } else if (stringr::str_detect(rights, phrases[[2]])) {
            return(list(status = "SUCCESS",
                        output = "Intellectual rights are present as a CC-0 license."))
        } else {
            return(list(status = "FAILURE",
                        output = "Intellectual rights are present but do not indicate a CC-BY or CC-0 license."))
        }
    }
}


#' Check if creator is present
#'
#' This function checks if a creator is present.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @importFrom methods is
#'
#' @noRd
qa_creator <- function(input) {
    if (methods::is(input, "eml")) {
        creators <- input@dataset@creator
    } else if (any(c("ListOfcreator", "character") %in% class(input))) {
        creators <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfcreator', or 'character'.")
    }

    if (length(creators) == 0) {
        return(list(status = "FAILURE",
                    output = "No creators are present."))
    } else {
        if (length(creators) == 1) {
            return(list(status = "SUCCESS",
                        output = "One creator is present."))
        } else {
            return(list(status = "SUCCESS",
                        output = sprintf("%d creators are present.", length(creators))))
        }
    }
}


#' Check if creator information is present
#'
#' This function checks if creator information is present, including an ID, email, and address.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom methods is
#'
#' @noRd
qa_creator_info <- function(input) {
    if (methods::is(input, "eml")) {
        creators <- input@dataset@creator
    } else if (methods::is(input, "ListOfcreator")) {
        creators <- input
    } else {
        stop("Input should be of class 'eml' or 'ListOfcreator'.")
    }

    if (length(creators) == 0) {
        return(list(status = "FAILURE",
                    output = "A creator entry is not present. Unable to check for an ORCID, email, or address."))
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Store output messages in a vector
    messages <- c()

    # There could be multiple creators, but just one creator with an ID will satisfy this check
    userId <- lapply(c(1:length(creators)), function(i) length(creators[[i]]@userId@.Data))
    if (all(userId == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "A user identifier for any creator is not present. Unable to check for an ORCID."
    } else {
        creator_ORCIDs <- unlist(EML::eml_get(creators, "userId"))
        has_ORCID <-  grepl("http[s]?:\\/\\/orcid.org\\/[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}", creator_ORCIDs)
        if (suppressWarnings(any(has_ORCID))) {
            messages[[length(messages) + 1]] <- sprintf("%d of %d creators have an ORCID.", length(creator_ORCIDs), length(creators))
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "The user identifier for any creator is not an ORCID."
        }
    }

    # There could be multiple creators, but just one creator with an email will satisfy this check
    email <- lapply(c(1:length(creators)), function(i) length(creators[[i]]@electronicMailAddress@.Data))
    if (all(email == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An email address for any creator is not present."
    } else {
        creator_emails <- unlist(EML::eml_get(creators, "electronicMailAddress"))
        has_email <-  grepl("@", creator_emails)
        if (suppressWarnings(any(has_email))) {
            messages[[length(messages) + 1]] <- sprintf("%d of %d creators have email addresses.", length(creator_emails), length(creators))
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "An email address for any creator is not present."
        }
    }

    # There could be multiple creators, but just one creator with an address will satisfy this check
    address <- lapply(c(1:length(creators)), function(i) length(creators[[i]]@address@.Data))
    if (all(address == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An address for any creator is not present."
    } else {
        creator_addresses <- unlist(EML::eml_get(creators, "deliveryPoint"))
        messages[[length(messages) + 1]] <- sprintf("%d of %d creators have addresses.", length(creator_addresses), length(creators))
    }

    return(list(status = status,
                output = messages))
}


#' Check if contact is present
#'
#' This function checks if a contact is present.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @importFrom methods is
#'
#' @noRd
qa_contact <- function(input) {
    if (methods::is(input, "eml")) {
        contacts <- input@dataset@contact
    } else if (any(c("ListOfcontact", "character") %in% class(input))) {
        contacts <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfcontact', or 'character'.")
    }

    if (length(contacts) == 0) {
        return(list(status = "FAILURE",
                    output = "No contacts are present."))
    } else {
        if (length(contacts) == 1) {
            return(list(status = "SUCCESS",
                        output = "One contact is present."))
        } else {
            return(list(status = "SUCCESS",
                        output = sprintf("%d contacts are present.", length(contacts))))
        }
    }
}


#' Check if contact information is present
#'
#' This function checks if contact information is present, including an ID, email, and address.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom methods is
#'
#' @noRd
qa_contact_info <- function(input) {
    if (methods::is(input, "eml")) {
        contacts <- input@dataset@contact
    } else if (methods::is(input, "ListOfcontact")) {
        contacts <- input
    } else {
        stop("Input should be of class 'eml' or 'ListOfcontact'.")
    }

    if (length(contacts) <= 0) {
        return(list(status = "FAILURE",
                    output = "A contact entry is not present. Unable to check for an ORCID, email, or address."))
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Store output messages in a vector
    messages <- c()

    # There could be multiple contacts, but just one contact with an ID will satisfy this check
    userId <- lapply(c(1:length(contacts)), function(i) length(contacts[[i]]@userId@.Data))
    if (all(userId == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "A user identifier for any contact is not present. Unable to check for an ORCID."
    } else {
        contact_ORCIDs <- unlist(EML::eml_get(contacts, "userId"))
        has_ORCID <-  grepl("http[s]?:\\/\\/orcid.org\\/[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}", contact_ORCIDs)
        if (suppressWarnings(any(has_ORCID))) {
            messages[[length(messages) + 1]] <- sprintf("%d of %d contacts have an ORCID.", length(contact_ORCIDs), length(contacts))
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "The user identifier for any contact is not an ORCID."
        }
    }

    # There could be multiple contacts, but just one contact with an email will satisfy this check
    email <- lapply(c(1:length(contacts)), function(i) length(contacts[[i]]@electronicMailAddress@.Data))
    if (all(email == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An email address for any contact is not present."
    } else {
        contact_emails <- unlist(EML::eml_get(contacts, "electronicMailAddress"))
        has_email <-  grepl("@", contact_emails)
        if (suppressWarnings(any(has_email))) {
            messages[[length(messages) + 1]] <- sprintf("%d of %d contacts have an email.", length(contact_emails), length(contacts))
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "An email address for any contact is not present."
        }
    }

    # There could be multiple contacts, but just one contact with an address will satisfy this check
    address <- lapply(c(1:length(contacts)), function(i) length(contacts[[i]]@address@.Data))
    if (all(address == 0)) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "An address for any contact is not present."
    } else {
        contact_addresses <- unlist(EML::eml_get(contacts, "deliveryPoint"))
        messages[[length(messages) + 1]] <- sprintf("%d of %d contacts have an address.", length(contact_addresses), length(contacts))
    }

    return(list(status = status,
                output = messages))
}


#' Check if geographic coverage is present with description
#'
#' This function checks if geographic coverage is present with a description.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom methods is
#'
#' @noRd
qa_geographic <- function(input) {
    if (methods::is(input, "eml")) {
        geo <- input@dataset@coverage@geographicCoverage
    } else if (any(c("ListOfgeographicCoverage", "character") %in% class(input))) {
        geo <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfgeographicCoverage', or 'character'.")
    }

    if (length(geo) == 0) {
        return(list(status = "FAILURE",
                    output = "Geographic coverage is not present."))
    }

    if (class(input) == "character") { # For class 'eml', is.character(input) returns TRUE; use alternative method instead
        desc <- input
    } else {
        desc <- EML::eml_get(input, "geographicDescription")
    }

    if (length(desc) != length(geo)) {
        return(list(status = "FAILURE",
                    output = sprintf("A textual description is present for %d of %d geographic coverages.",
                                     length(desc), length(geo))))
    } else {
        return(list(status = "SUCCESS",
                    output = sprintf("A textual description is present for %d of %d geographic coverages.",
                                     length(desc), length(geo))))
    }
}


#' Check if geographic coverage bounding coordinates are present
#'
#' This function checks if geographic coverage bounding coordinates are present.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom methods is
#'
#' @noRd
qa_geographic_coord <- function(input) {
    if (methods::is(input, "eml")) {
        geo <- input@dataset@coverage@geographicCoverage
    } else if (any(c("ListOfgeographicCoverage", "character") %in% class(input))) {
        geo <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfgeographicCoverage', or 'character'.")
    }

    if (length(geo) == 0) {
        return(list(status = "FAILURE",
                    output = "Geographic coverage is not present. Unable to check for bounding coordinates."))
    }

    # Both bounding boxes and single points should have four coordinates
    # Single points have duplicates for north/south and east/west
    if (class(input) == "character") { # For class 'eml', is.character(input) returns TRUE; use alternative method instead
        four_coords <- geo
        if (length(input) == 4) {logicals <- TRUE} else {logicals <- FALSE}
        true_sum <- 1
    } else {
        four_coords <- function(x) {
            coord <- EML::eml_get(x, "westBoundingCoordinate")
            coord <- append(coord, EML::eml_get(x, "eastBoundingCoordinate"))
            coord <- append(coord, EML::eml_get(x, "northBoundingCoordinate"))
            coord <- append(coord, EML::eml_get(x, "southBoundingCoordinate"))
            length(coord) == 4 # TRUE/FALSE
        }

        logicals <- lapply(geo, four_coords)
        true_sum <- sum(unlist(logicals))
    }

    if (any(logicals == FALSE)) {
        return(list(status = "FAILURE",
                    output = sprintf("A complete set of bounding coordinates is not present for %d of %d geographic coverages.",
                                     true_sum, length(geo))))
    } else {
        return(list(status = "SUCCESS",
                    output = sprintf("A complete set of bounding coordinates is present for %d of %d geographic coverages.",
                                     true_sum, length(geo))))
    }
}


#' Check if geographic coverage intersects with Arctic
#'
#' This function checks if geographic coverage intersects with the Arctic.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom methods is
#'
#' @noRd
qa_geographic_arctic <- function(input) {
    if (methods::is(input, "eml")) {
        geo <- input@dataset@coverage@geographicCoverage
    } else if (any(c("ListOfgeographicCoverage", "numeric", "character") %in% class(input))) {
        geo <- input
    } else {
        stop("Input should be of class 'eml', 'ListOfgeographicCoverage', 'numeric', or 'character'.")
    }

    if (length(geo) == 0) {
        return(list(status = "FAILURE",
                    output = "Geographic coverage is not present. Unable to check for bounding coordinates."))
    }

    if (any(c("numeric", "character") %in% class(input))) {
        ncoord <- input
    } else {
        ncoord <- EML::eml_get(input, "northBoundingCoordinate")
    }

    if (any(is.na(suppressWarnings(as.numeric(ncoord))))) {
        return(list(status = "FAILURE",
                    output = "A northern bounding coordinate is not numeric."))
    } else if (any(as.numeric(ncoord) > 90)) {
        return(list(status = "FAILURE",
                    output = "A northern bounding coordinate is out of range. The valid range is 0 to +90."))
    } else {
        if (any(as.numeric(ncoord) >= 45)) {
            return(list(status = "SUCCESS",
                        output = "Geographic coverage is in the Arctic."))
        } else {
            return(list(status = "FAILURE",
                        output = "Geographic coverage is not in the Arctic."))
        }
    }
}


#' Check if temporal coverage is present.
#'
#' This function checks if temporal coverage is present.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom methods is
#'
#' @noRd
# TODO: check for correct formatting of dates (YYYY or YYYY-MM-DD)
qa_temporal <- function(input) {
    if (methods::is(input, "eml")) {
        temp <- input@dataset@coverage@temporalCoverage
    } else if (any(c("ListOftemporalCoverage", "character") %in% class(input))) {
        temp <- input
    } else {
        stop("Input should be of class 'eml', 'ListOftemporalCoverage', or 'character'.")
    }

    if (length(temp) == 0) {
        return(list(status = "FAILURE",
                    output = "Temporal coverage is not present."))
    } else {
        dates <- EML::eml_get(temp, "calendarDate")
        if (length(dates) == 0) { # no dates
            return(list(status = "FAILURE",
                        output = "Temporal coverage is not present."))
        } else if (length(dates) == 1) { # single date
            return(list(status = "SUCCESS",
                        output = "Temporal coverage is present with one date."))
        } else { # date range, multiple single dates, multiple date ranges, etc.
            return(list(status = "SUCCESS",
                        output = "Temporal coverage is present with multiple dates."))
        }
    }
}


#' Check if taxonomic coverage is present
#'
#' This function checks if taxonomic coverage is present.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom methods is
#'
#' @noRd
qa_taxonomic <- function(input) {
    if (methods::is(input, "eml")) {
        taxo <- input@dataset@coverage@taxonomicCoverage
    } else if (any(c("ListOftaxonomicCoverage", "character") %in% class(input))) {
        taxo <- input
    } else {
        stop("Input should be of class 'eml', 'ListOftaxonomicCoverage', or 'character'.")
    }

    if (length(taxo) == 0) {
        return(list(status = "FAILURE",
                    output = "Taxonomic coverage is not present."))
    } else {
        class <- EML::eml_get(taxo, "taxonomicClassification")
        if (length(class) == 0) { # no taxonomic classifications
            return(list(status = "FAILURE",
                        output = "Taxonomic coverage is not present."))
        } else { # one or more taxonomic classifications
            return(list(status = "SUCCESS",
                        output = sprintf("Taxonomic coverage is present with %d classifications.", length(class))))
        }
    }
}


#' Check if methods are present and complete
#'
#' This function checks if methods are present and complete by checking
#' for method steps and sampling descriptions.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @importFrom methods is
#'
#' @noRd
# TODO: account for more than one sampling step (sampling[[i]])
qa_methods <- function(input) {
    if (methods::is(input, "eml")) {
        methods <- input@dataset@methods
    } else if (methods::is(input, "methods")) {
        methods <- input
    } else {
        stop("Input should be of class 'eml' or 'methods'.")
    }

    # No methods present
    if (length(methods@methodStep) == 0 && length(methods@sampling) == 0) {
        return(list(status = "FAILURE",
                    output = "Methods are not present."))
    }

    # Methods present but not complete
    if ((length(methods@methodStep) == 0 && length(methods@sampling) > 0) ||
        (length(methods@methodStep) > 0 && length(methods@sampling) == 0) ||
        ((length(methods@methodStep) > 0 && length(methods@sampling) > 0) &&
         length(methods@sampling[[1]]@studyExtent@description) == 0 ||
         (length(methods@sampling[[1]]@samplingDescription) == 0 &&
          length(methods@sampling[[1]]@samplingDescription@para) == 0))) {
        status <- "FAILURE"
        # Store output messages in a vector
        messages <- c()

        if (length(methods@methodStep) == 0) {
            messages[[length(messages) + 1]] <- "Methods are present but do not include method steps."
        }

        if (length(methods@sampling) == 0) {
            messages[[length(messages) + 1]] <- "Methods are present but do not include a sampling description."
        } else {
            if (length(methods@sampling[[1]]@studyExtent@description) == 0) {
                messages[[length(messages) + 1]] <- "Methods are present but do not include the sampling area and frequency (studyExtent)."
            }
            # Usually samplingDescription has a 'para' slot but sometimes not
            if (length(methods@sampling[[1]]@samplingDescription) == 0 &&
                length(methods@sampling[[1]]@samplingDescription@para) == 0) {
                messages[[length(messages) + 1]] <- "Methods are present but do not include a description of the sampling procedures (samplingDescription)."
            }
        }

        return(list(status = status,
                    output = messages))
    }

    # All methods present and complete
    if (length(methods@methodStep) > 0 && length(methods@sampling) > 0 &&
        length(methods@sampling[[1]]@studyExtent@description) > 0 &&
        (length(methods@sampling[[1]]@samplingDescription) > 0 ||
         length(methods@sampling[[1]]@samplingDescription@para) > 0)) {
        return(list(status = "SUCCESS",
                    output = "Methods are present, including method steps and a complete sampling description."))
    }
}


#' Check if project information is present and complete
#'
#' This function checks if project information is present and complete.
#' The minimum recommended fields are title, personnel, abstract, and funding.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @importFrom methods is
#'
#' @noRd
qa_project <- function(input) {
    if (methods::is(input, "eml")) {
        project <- input@dataset@project
    } else if (methods::is(input, "project")) {
        project <- input
    } else {
        stop("Input should be of class 'eml' or 'project'.")
    }

    # Check if title exists to determine if project information is present
    if (length(project@title) == 0) {
        return(list(status = "FAILURE",
                    output = "Project information is not present."))
    } else {
        # Minimum recommended fields
        title <- length(project@title)
        personnel <- length(project@personnel)
        abstract <- length(project@abstract@para) # usually abstract has a 'para' slot but sometimes not
        funding <- length(project@funding@para) # usually funding has a 'para' slot but sometimes not

        missing <- append(c(), c(if (title == 0) "title" else {},
                                 if (personnel == 0) "personnel" else {},
                                 if (abstract == 0) "abstract" else {},
                                 if (funding == 0) "funding" else {}))

        if (title > 0 && personnel > 0 && abstract > 0 && funding > 0) {
            return(list(status = "SUCCESS",
                        output = "Project information is present and complete."))
        } else {
            return(list(status = "FAILURE",
                        output = paste0("Project information is present but not complete. ",
                                        "The following are missing: ", paste(missing, collapse = ", "), ".")))
        }
    }
}


#' Check if name and description are present for data entities
#'
#' This function checks if a name and description are present for each data entity.
#' Additionally, entity names should be 100 characters or less.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @importFrom methods is
#'
#' @noRd
qa_entity <- function(input) {
    if (methods::is(input, "eml")) {
        entity <- list(input@dataset@dataTable,
                       input@dataset@otherEntity,
                       input@dataset@spatialVector)
    } else if (any(c("ListOfdataTable", "ListOfotherEntity", "ListOfspatialVector") %in% class(input))) {
        entity <- list(input)
    } else if (any(c("dataTable", "otherEntity", "spatialVector") %in% class(input))) {
        entity <- list(list(input))
    } else {
        stop("Input should be of class 'eml', 'ListOfdataTable', 'ListOfotherEntity',
             'ListOfspatialVector', 'dataTable', 'otherEntity', or 'spatialVector'.")
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Store output messages in a vector
    messages <- c()

    for (i in seq_along(entity)) { # each data type
        for (j in seq_along(entity[[i]])) { # each entity of a data type
            if (length(entity[[i]]) == 0) {
                next
            } else {
                object <-
                    if (!is.null(EML::eml_get(entity[[i]][[j]], "objectName"))) {
                        EML::eml_get(entity[[i]][[j]], "objectName")
                    } else {
                        EML::eml_get(entity[[i]][[j]], "entityName")
                    }
                if (length(entity[[i]][[j]]@entityName@.Data) == 0) {
                    status <- "FAILURE"
                    messages[[length(messages) + 1]] <- paste("The entity name for", object, "is missing.")
                } else if (nchar(entity[[i]][[j]]@entityName@.Data) > 100) {
                    status <- "FAILURE"
                    messages[[length(messages) + 1]] <- paste("The entity name for", object, "is present but greater than 100 characters.",
                                                              "Shorter names are recommended.")
                } else if (length(entity[[i]][[j]]@entityDescription@.Data) == 0) {
                    status <- "FAILURE"
                    messages[[length(messages) + 1]] <- paste("The entity description for", object, "is missing.")
                } else {
                    messages[[length(messages) + 1]] <- paste0("The entity name and description are present for ", object, ".")
                }
            }
        }
    }

    return(list(status = status,
                output = messages))
}


#' Check for duplicated data entities
#'
#' This function checks if data entities have been duplicated
#' based on matching PIDs.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @importFrom methods is
#'
#' @noRd
qa_entity_dup <- function(input) {
    if (methods::is(input, "eml")) {
        entity <- list(input@dataset@dataTable,
                       input@dataset@otherEntity,
                       input@dataset@spatialVector)
    } else if (any(c("ListOfdataTable", "ListOfotherEntity", "ListOfspatialVector") %in% class(input))) {
        entity <- list(input)
    } else if (any(c("dataTable", "otherEntity", "spatialVector") %in% class(input))) {
        entity <- list(list(input))
    } else {
        stop("Input should be of class 'eml', 'ListOfdataTable', 'ListOfotherEntity', 'ListOfspatialVector', 'dataTable', 'otherEntity', or 'spatialVector'.")
    }

    pids <- character()
    for (i in seq_along(entity)) { # each data type
        for (j in seq_along(entity[[i]])) { # each entity of a data type
            if (length(entity[[i]]) == 0 || length(entity[[i]][[j]]@id@.Data) == 0) { # id is often missing
                next
            } else {
                pids[[length(pids) + 1]] <- entity[[i]][[j]]@id@.Data
            }
        }
    }

    if (length(pids) != length(unique(pids))) {
        return(list(status = "FAILURE",
                    output = paste0("Data entities have been duplicated based on matching PIDs: ",
                                    paste0(pids[which(duplicated(pids))], collapse = " + "))))
    } else {
        return(list(status = "SUCCESS",
                    output = "No data entities are duplicated based on matching PIDs."))
    }
}


#' Check if physical of data entity is present and complete
#'
#' This function checks the physical of a data entity for presence, completeness, and mismatches.
#'
#' @param input (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom methods is
#' @importFrom stringr str_detect str_split
#'
#' @noRd
qa_physical <- function(input) {
    if (methods::is(input, "eml")) {
        entity <- list(input@dataset@dataTable,
                       input@dataset@otherEntity,
                       input@dataset@spatialVector)
    } else if (any(c("ListOfdataTable", "ListOfotherEntity", "ListOfspatialVector") %in% class(input))) {
        entity <- list(input)
    } else if (any(c("dataTable", "otherEntity", "spatialVector") %in% class(input))) {
        entity <- list(list(input))
    } else {
        stop("Input should be of class 'eml', 'ListOfdataTable', 'ListOfotherEntity', 'ListOfspatialVector', 'dataTable', 'otherEntity', or 'spatialVector'.")
    }

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Store output messages in a vector
    messages <- c()

    for (i in seq_along(entity)) { # each data type
        for (j in seq_along(entity[[i]])) { # each entity of a data type
            if (length(entity[[i]]) == 0) {
                next
            } else {
                object <-
                    if (!is.null(EML::eml_get(entity[[i]][[j]], "objectName"))) {
                        EML::eml_get(entity[[i]][[j]], "objectName")
                    } else {
                        EML::eml_get(entity[[i]][[j]], "entityName")
                    }
                if (length(entity[[i]][[j]]@physical@.Data) == 0) {
                    status <- "FAILURE"
                    messages[[length(messages) + 1]] <- paste("The physical for", object, "is missing.")
                } else {
                    # Check for presence and completeness of elements in physical
                    elements <- c("objectName", "size", "authentication", "formatName", "url")
                    missing <- character()
                    for (x in elements) {
                        if (length(EML::eml_get(entity[[i]][[j]], x)) == 0) missing[[length(missing) + 1]] <- x else {}
                    }
                    if (length(missing > 0)) {
                        status <- "FAILURE"
                        messages[[length(messages) + 1]] <- paste0("The physical for ", object, " is present but not complete. ",
                                                                   "The following are missing: ", paste0(missing, collapse = ", "), ".")
                    } else {
                        messages[[length(messages) + 1]] <- paste("The physical for", object, "is present and complete.")
                    }

                    # Check if entityName matches objectName
                    if (entity[[i]][[j]]@entityName@.Data != entity[[i]][[j]]@physical[[1]]@objectName@.Data) {
                        status <- "FAILURE"
                        messages[[length(messages) + 1]] <- paste("The entity and object names for", object, "do not match.")
                    } else {
                        messages[[length(messages) + 1]] <- paste("The entity and object names for", object, "match.")
                    }

                    # Check if entity PID matches PID in URL in physical
                    if (!stringr::str_detect(entity[[i]][[j]]@physical[[1]]@distribution[[1]]@online@url@.Data, "urn") ||
                        length(entity[[i]][[j]]@id@.Data) == 0) { # id is often missing
                        # skip
                    } else {
                        url_pid <- stringr::str_split(entity[[i]][[j]]@physical[[1]]@distribution[[1]]@online@url@.Data, "(?=urn.)", simplify = TRUE)[[2]]
                        if (entity[[i]][[j]]@id@.Data != url_pid) {
                            status <- "FAILURE"
                            messages[[length(messages) + 1]] <- paste("The entity PID and URL PID for", object, "do not match.")
                        } else {
                            messages[[length(messages) + 1]] <- paste("The entity PID and URL PID for", object, "match.")
                        }
                    }
                }
            }
        }
    }

    return(list(status = status,
                output = messages))
}


#' Quality assurance for EML metadata
#'
#' This function checks the quality of EML metadata, such as the presence and completeness
#' of EML fields and correct formatting.
#'
#' Not all FAILURE messages necessarily indicate something wrong with the metadata. For example, missing project
#' information will return a FAILURE status, but project information is not necessary if there is
#' no affiliated project.
#'
#' @param input (eml) An EML object.
#' @param all_results (logical) Return all results. If `FALSE`, only returns results with FAILURE or ERROR status.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom methods is
#' @importFrom stringr str_detect
#'
#' @export
#'
#' @seealso [qa_package()]
#'
#' @examples
#' \dontrun{
#' # Results with FAILURE or ERROR status
#' qa_eml(eml)
#'
#' # All results
#' qa_eml(eml, all_results = TRUE)
#' }
qa_eml <- function(input, all_results = FALSE) {
    if (!methods::is(input, "eml")) {
        stop("Input should be of class 'eml'.")
    }
    stopifnot(is.logical(all_results))

    val <- EML::eml_validate(input)
    val_results <- list(status = if (length(attr(val, "errors")) == 0) "SUCCESS" else "FAILURE",
                        output = attr(val, "errors"))

    # Use tryCatch to return ERROR status if error is encountered
    err <- function(e) list(status = "ERROR")

    results <- list("Title" = tryCatch(qa_title(input), error = err),
                    "Publication_Date" = tryCatch(qa_pubDate(input), error = err),
                    "Abstract" = tryCatch(qa_abstract(input), error = err),
                    "Keywords" = tryCatch(qa_keywordSet(input), error = err),
                    "Intellectual_Rights" = tryCatch(qa_intellectualRights(input), error = err),
                    "Creator" = tryCatch(qa_creator(input), error = err),
                    "Creator_Info" = tryCatch(qa_creator_info(input), error = err),
                    "Contact" = tryCatch(qa_contact(input), error = err),
                    "Contact_Info" = tryCatch(qa_contact_info(input), error = err),
                    "Geographic_Coverage" = tryCatch(qa_geographic(input), error = err),
                    "Geographic_Coordinates" = tryCatch(qa_geographic_coord(input), error = err),
                    "Arctic_Coverage" = tryCatch(qa_geographic_arctic(input), error = err),
                    "Temporal_Coverage" = tryCatch(qa_temporal(input), error = err),
                    "Taxonomic_Coverage" = tryCatch(qa_taxonomic(input), error = err),
                    "Methods" = tryCatch(qa_methods(input), error = err),
                    "Project" = tryCatch(qa_project(input), error = err),
                    "Entity" = tryCatch(qa_entity(input), error = err),
                    "Entity_Duplication" = tryCatch(qa_entity_dup(input), error = err),
                    "Physical" = tryCatch(qa_physical(input), error = err),
                    "EML_Validation" = val_results)

    # Default to only return results with FAILURE or ERROR status
    if (all_results) {
        return(results)
    } else {
        results <- Filter(function(x) stringr::str_detect(x$status, "FAILURE|ERROR"), results)
        if (length(results) > 0) {
            return(results)
        } else {
            return(list("EML" = list(status = "SUCCESS",
                                     output = "All QA checks were successful.")))
        }
    }
}


#' Check congruence of data and metadata attributes for all tabular data objects
#'
#' This function checks the congruence of data and metadata attributes for all
#' tabular data objects in a data package. Supported objects include dataTable,
#' otherEntity, and spatialVector entities. This function also checks if all
#' tabular data objects can be successfully downloaded from a DataONE Member Node.
#' The data is downloaded based on the distribution URL found in the physical section
#' of the EML for supported data objects.
#'
#' This function can be used on its own or as an option with [qa_package()].
#'
#' This function only checks one data package at a time. For parent packages, it does not automatically check
#' child packages. These should be checked individually. However, it is possible to iterate through
#' child packages using:
#' ```
#' pkg <- arcticdatautils::get_package(mn, parent_rm_pid, file_names = TRUE)
#' results <- lapply(pkg$child_packages, function(x) qa_congruence(mn, x))
#' ```
#'
#' @param mn (MNode) The Member Node to query.
#' @param resource_map_pid (character) The PID for a resource map.
#' @param read_all_data (logical) Read all data from remote. If `FALSE`, only read first 10 rows.
#' @param all_results (logical) Return all results. If `FALSE`, only returns results with FAILURE or ERROR status.
#'
#' @return (list) A list of results.
#'
#' @import arcticdatautils
#' @import dataone
#' @import EML
#' @importFrom datapack hasAccessRule
#' @importFrom methods is
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 nc_open
#' @importFrom readxl read_excel
#' @importFrom sf read_sf
#' @importFrom sf st_set_geometry
#' @importFrom utils read.csv
#' @importFrom utils read.delim
#' @importFrom utils read.table
#' @importFrom utils download.file
#'
#' @export
#'
#' @seealso [qa_attributes()] [qa_package()]
#'
#' @examples
#' \dontrun{
#' # Read all data
#' qa_congruence(adc_prod, "resource_map_urn:uuid:...")
#'
#' # For large or many data objects, read first 10 rows
#' qa_congruence(adc_prod, "resource_map_urn:uuid:...", read_all_data = FALSE)
#'
#' # All results
#' qa_congruence(adc_prod, "resource_map_urn:uuid:...", all_results = TRUE)
#' }
qa_congruence <- function(mn, resource_map_pid, read_all_data = TRUE, all_results = FALSE) {
    stopifnot(class(mn) %in% c("MNode", "CNode"))
    stopifnot(is.character(resource_map_pid), nchar(resource_map_pid) > 0)
    stopifnot(is.logical(read_all_data))
    stopifnot(is.logical(all_results))

    package <- tryCatch(suppressWarnings(arcticdatautils::get_package(mn, resource_map_pid, file_names = TRUE)),
                        error = function(e) stop("Failed to get package. Is the Member Node correct? Is your DataONE token set?"))

    eml <- EML::read_eml(dataone::getObject(mn, package$metadata))
    eml_objects <- c(EML::eml_get(eml, "dataTable"),
                     EML::eml_get(eml, "otherEntity"),
                     EML::eml_get(eml, "spatialVector"))
    if (length(eml_objects) == 0) {
        return(list(status = "FAILURE",
                    output = "No data objects of a supported format were found in the EML."))
    }
    # Preserve order of getting data objects based on data type for correct name assignment
    # Entity names may not match data object names, so use objectName to ensure matches with data names
    names(eml_objects) <- unlist(c(EML::eml_get(eml@dataset@dataTable, "objectName"),
                                   EML::eml_get(eml@dataset@otherEntity, "objectName"),
                                   EML::eml_get(eml@dataset@spatialVector, "objectName")))
    # If object names are missing, use entity names instead
    if (is.null(names(eml_objects)) || any(is.na(names(eml_objects)))) {
        names(eml_objects) <- unlist(c(EML::eml_get(eml@dataset@dataTable, "entityName"),
                                       EML::eml_get(eml@dataset@otherEntity, "entityName"),
                                       EML::eml_get(eml@dataset@spatialVector, "entityName")))
    }

    data_objects <- dl_and_read_all_data(mn, package, eml, read_all_data)

    # If missing fileName, assign name to data objects
    for (i in seq_along(data_objects)) {
        if (is.na(names(data_objects)[[i]])) {
            id <- package$data[[i]]
            j <- which(stringr::str_detect(EML::eml_get(eml_objects, "url"), id))
            names(data_objects)[[i]] <-
                if (!is.na(EML::eml_get(eml_objects[[j]], "objectName"))) {
                    EML::eml_get(eml_objects[[j]], "objectName")
                } else {
                    EML::eml_get(eml_objects[[j]], "entityName")
                }
        }
    }

    # If dl_and_read_all_data() has FAILURE status, stop here
    if (length(data_objects$status) == 1 && data_objects$status == "FAILURE") { # missing or incongruent URLs
        return(data_objects)
    } else if (any(unlist(lapply(data_objects, function(x) suppressWarnings(length(x$status) == 1) && x$status == "FAILURE")))) { # some data objects failed to read
        data_objects <- Filter(function(x) length(x$status) == 1 && x$status == "FAILURE", data_objects)
        return(list(status = "FAILURE",
                    output = data_objects))
    }

    if (length(eml_objects) != length(data_objects)) {
        return(list(status = "FAILURE",
                    output = "The number of downloaded data objects does not match the number of EML data objects."))
    }

    # Filter out data objects that have SKIP status
    data_objects <- Filter(function(x) suppressWarnings(length(x$status) == 0) || suppressWarnings(x$status != "SKIP"), data_objects)
    eml_objects <- Filter(function(x) EML::eml_get(x, "objectName") %in% names(data_objects) ||
                                        EML::eml_get(x, "entityName") %in% names(data_objects), eml_objects)

    # Index objects in parallel based on names (in ascending order) for correct processing in iterations
    eml_objects <- eml_objects[order(names(eml_objects))]
    data_objects <- data_objects[order(names(data_objects))]

    att_checks <- mapply(qa_attributes, eml_objects, data_objects, SIMPLIFY = FALSE) # do not simplify to ensure a list is returned

    if (any(unlist(lapply(att_checks, function(x) x$status == "FAILURE")))) {
        status <- "FAILURE"
    } else {
        status <- "SUCCESS"
    }

    # Default to only return results with FAILURE or ERROR status
    if (all_results) {
        return(list(status = status,
                    output = att_checks))
    } else {
        results <- Filter(function(x) stringr::str_detect(x$status, "FAILURE"), att_checks)
        if (length(results) > 0) {
            return(list(status = status,
                        output = results))
        } else {
            return(list("Congruence" = list(status = "SUCCESS",
                                            output = "All QA checks were successful.")))
        }
    }
}


# Helper function for downloading and reading all data objects in a data package
dl_and_read_all_data <- function(mn, package, eml, read_all_data) {
    stopifnot(class(mn) %in% c("MNode", "CNode"))
    stopifnot(is.list(package), length(package) > 0)
    stopifnot(methods::is(eml, "eml"))
    stopifnot(is.logical(read_all_data))

    urls <- unique(unlist(EML::eml_get(eml@dataset, "url"), recursive = TRUE))

    # Store output messages in a vector
    messages <- c()
    # Check that each data object has a matching URL in the EML
    wrong_URL <- FALSE
    for (datapid in package$data) {
        n <- which(grepl(paste0(datapid, "$"), urls))
        if (length(n) != 1) {
            messages[[length(messages) + 1]] <- paste("The distribution URL for", datapid, "is missing or incongruent.")
            wrong_URL <- TRUE
        }
    }

    # If needed, stop here to ensure proper ordering in the following iterations
    if (length(urls) != length(package$data) || wrong_URL) {
        return(list(status = "FAILURE",
                    output = c("All distribution URLs for data objects must match the data PIDs to continue.",
                               messages)))
    }

    if (read_all_data) {
        rows_to_read <- -1
    } else {
        rows_to_read <- 10
    }

    objects <- lapply(package$data, dl_and_read_data, eml, mn, rows_to_read)

    return(objects)
}


# Helper function for downloading and reading a data object
dl_and_read_data <- function(objectpid, eml, mn, rows_to_read) {
    supported_file_formats <- c("text/csv",
                                "text/tsv",
                                "text/plain",
                                "application/vnd.ms-excel",
                                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                "application/zip",
                                "netCDF-4",
                                "netCDF-3",
                                "CF-1.4", "CF-1.3", "CF-1.2", "CF-1.1", "CF-1.0")

    urls_dataTable <- unique(unlist(EML::eml_get(eml@dataset@dataTable, "url"), recursive = TRUE))
    urls_otherEntity <- unique(unlist(EML::eml_get(eml@dataset@otherEntity, "url"), recursive = TRUE))
    urls_spatialVector <- unique(unlist(EML::eml_get(eml@dataset@spatialVector, "url"), recursive = TRUE))

    n_dT <- which(grepl(paste0(objectpid, "$"), urls_dataTable))
    n_oE <- which(grepl(paste0(objectpid, "$"), urls_otherEntity))
    n_sV <- which(grepl(paste0(objectpid, "$"), urls_spatialVector))

    if (length(n_dT) == 1) {
        entity <- eml@dataset@dataTable[[n_dT]]
        urls <- urls_dataTable
        i <- n_dT
    } else if (length(n_oE) == 1) {
        entity <- eml@dataset@otherEntity[[n_oE]]
        urls <- urls_otherEntity
        i <- n_oE
    } else if (length(n_sV) == 1) {
        entity <- eml@dataset@spatialVector[[n_sV]]
        urls <- urls_spatialVector
        i <- n_sV
    } else {
        return(list(status = "SKIP",
                    output = paste("Data object", objectpid, "is not a supported format. Skipped.")))
    }

    # If object is not tabular data, skip to next object
    format <- EML::eml_get(entity, "formatName")
    if (length(format) == 0) {
        return(list(status = "FAILURE",
                    output = paste("Data object", objectpid, "has no given format ID.",
                                   "Unable to check if object is a supported format.")))
    } else if (!format %in% supported_file_formats) {
        return(list(status = "SKIP",
                    output = paste("Data object", objectpid, "is not tabular or not a supported format. Skipped.")))
    }

    # If package is public, read directly from the file; otherwise, use DataONE API
    sysmeta <- dataone::getSystemMetadata(mn, objectpid)
    isPublic <- datapack::hasAccessRule(sysmeta, "public", "read")

    tryCatch({
        if (isPublic) {
            if (format == "text/csv") {
                data <- utils::read.csv(urls[i], nrows = rows_to_read, check.names = FALSE, stringsAsFactors = FALSE)
            } else if (format == "text/tsv") {
                data <- utils::read.delim(urls[i], nrows = rows_to_read)
            } else if (format == "text/plain") {
                data <- utils::read.table(urls[i], nrows = rows_to_read)
            } else if (format == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" || format == "application/vnd.ms-excel") {
                tmp <- tempfile()
                utils::download.file(url = urls[i], destfile = tmp, mode = "wb", quiet = TRUE)
                data <- readxl::read_excel(tmp, n_max = if (rows_to_read == -1) {Inf} else {rows_to_read})
                unlink(tmp)
                data
            } else if (format == "application/zip") {
                # Many formats can exist within a .zip file; skip if not spatial data
                tmp <- tempfile()
                utils::download.file(url = urls[i], destfile = tmp, quiet = TRUE)
                tmp2 <- tempfile()
                utils::unzip(tmp, exdir = tmp2)
                t <- list.files(tmp2, full.names = TRUE, recursive = TRUE)
                if (any(grep("*\\.shp", t))) {
                    data <- suppressWarnings(sf::read_sf(t[grep("*\\.shp", t)]) %>% sf::st_set_geometry(., value = NULL))
                } else if (any(grep("*\\.gdb", t))) {
                    data <- suppressWarnings(sf::read_sf(list.dirs(tmp2)[2]) %>% sf::st_set_geometry(., value = NULL))
                } else {
                    return(list(status = "SKIP",
                                output = "Spatial data not present within .zip file. Skipped."))
                }
                unlink(c(tmp, tmp2), recursive = TRUE)
                data
            } else if (format == "netCDF-4" || format == "netCDF-3" || format == "CF-1.4" || format == "CF-1.3" ||
                       format == "CF-1.2" || format == "CF-1.1" || format == "CF-1.0") {
                tmp <- tempfile()
                utils::download.file(url = urls[i], destfile = tmp, mode = "wb", quiet = TRUE)
                nc <- ncdf4::nc_open(tmp)
                data <- netcdf_to_dataframe(nc)
                unlink(tmp)
                rm(nc) # clean up now because many netCDF files are large
                data
            }
        } else {
            if (format == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" || format == "application/vnd.ms-excel") {
                return(list(status = "SKIP",
                            output = paste("This function uses the DataONE API to read private data objects and currently cannot read .xls or .xlsx files.",
                                           "Skipped. Check attributes manually.")))
            } else {
                data <- utils::read.csv(textConnection(rawToChar(dataone::getObject(mn, objectpid))),
                                        nrows = rows_to_read, check.names = FALSE, stringsAsFactors = FALSE)
            }
        }

        return(data)
    },
    error = function(e) {
        return(list(status = "FAILURE",
                    output = paste("Failed to read file at", urls[i])))
    })
}


# Helper function for converting 2D data from a netCDF to a data.frame
netcdf_to_dataframe <- function(nc) {
    att_names <- names(nc$var)
    dims <- nc$dim
    dim_names <- c()
    for (i in seq_along(dims)) {
        dim_names[i] <- dims[[i]]$name
    }

    var_names <- c(att_names, dim_names)
    var_names <- var_names[-which(duplicated(tolower(var_names)))]

    data <- lapply(var_names, function(x) ncdf4::ncvar_get(nc, x))
    max_length <- max(unlist(lapply(data, function(x) length(x))))

    results <- data.frame(matrix(ncol = length(data), nrow = max_length))
    names(results) <- var_names
    for (i in seq_along(results)) {
        results[ , i] <- rep_len(data[[i]], length.out = max_length)
    }

    return(results)
}


#' Check congruence of data and metadata attributes for a tabular data object
#'
#' This function checks the congruence of data and metadata attributes
#' for a tabular data object. Supported objects include dataTable, otherEntity,
#' and spatialVector entities. It can be used on its own but is also
#' called by [qa_congruence()] to check all tabular data objects in a data package.
#'
#' This function checks the following:
#' * Names: Check that column names in attributes match column names in data frame. Possible conditions to check for:
#'     * attributeList does not exist for data frame
#'     * Some of the attributes that exist in the data do not exist in the attributeList
#'     * Some of the attributes that exist in the attributeList do not exist in the data
#'     * Typos in attribute or column names resulting in nonmatches
#' * Domains: Check that attribute types in EML match attribute types in data frame. Possible conditions to check for:
#'     * nominal, ordinal, integer, ratio, dateTime
#'     * If domain is enumerated domain, enumerated values in the data are accounted for in the enumerated definition
#'     * If domain is enumerated domain, enumerated values in the enumerated definition are all represented in the data
#'     * Type of data does not match attribute type
#' * Values: Check that values in data are reasonable. Possible conditions to check for:
#'     * Accidental characters in the data (e.g., one character in a column of integers)
#'     * If missing values are present, missing value codes are also present
#'
#' @param entity (eml) An EML entity associated with a data object.
#' @param data (data.frame) A data frame of the data object.
#'
#' @return (list) A list of results.
#'
#' @import arcticdatautils
#' @import EML
#' @importFrom lubridate parse_date_time
#' @importFrom stats na.omit
#' @importFrom utils capture.output
#' @importFrom utils head
#'
#' @export
#'
#' @seealso [qa_congruence()]
#'
#' @examples
#' \dontrun{
#' # Checking a .csv file
#' dataTable <- eml@dataset@dataTable[[1]]
#' data <- readr::read_csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:...")
#'
#' qa_attributes(dataTable, data)
#' }
qa_attributes <- function(entity, data) {
    stopifnot(any(c("dataTable", "otherEntity", "spatialVector") %in% class(entity)))
    stopifnot(is.data.frame(data))

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Store output messages in a vector
    messages <- c()

    attributeTable <- EML::get_attributes(entity@attributeList)
    attributeNames <- attributeTable$attributes$attributeName

    # Check if attributes are present
    if (is.null(attributeNames)) {
        return(list(status = "FAILURE",
                    output = paste("Empty attributes table for data object at", entity@physical[[1]]@distribution[[1]]@online@url)))
    }

    # Check for duplicated attributes based on names
    if (any(duplicated(attributeNames))) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "There are duplicated attribute names in the EML."
    }

    header <- as.numeric(entity@physical[[1]]@dataFormat@textFormat@numHeaderLines)

    if (length(header) > 0 && !is.na(header) && header > 1) {
        names(data) <- NULL
        names(data) <- data[(header - 1), ]
    }

    data_cols <- colnames(data)

    # Check for attribute correctness according to the EML schema
    att_output <- utils::capture.output(arcticdatautils::eml_validate_attributes(entity@attributeList))
    att_errors <- which(grepl("FALSE", utils::head(att_output, length(attributeNames))))

    if (length(att_errors) > 0) {
        status <- "FAILURE"
        for (i in seq_along(att_errors)) {
            messages[[length(messages) + 1]] <- att_output[att_errors[i]]
        }
    }

    # Check that attribute names match column names
    allequal <- isTRUE(all.equal(data_cols, attributeNames))

    if (!allequal) {
        intersection <- intersect(attributeNames, data_cols)
        nonmatcheml <- attributeNames[!attributeNames %in% intersection]
        nonmatchdata <- data_cols[!data_cols %in% intersection]

        # EML has values that data does not have
        if (length(nonmatcheml) > 0) {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- paste0("The EML includes attributes '", toString(nonmatcheml, sep = ", "), "' that are not present in the data.")
        }

        # Data has values that EML does not have
        if (length(nonmatchdata) > 0) {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- paste0("The data includes attributes '", toString(nonmatchdata, sep = ", "), "' that are not present in the EML.")
        }

        # Values match but are not ordered correctly
        if (length(nonmatcheml) == 0 && length(nonmatchdata) == 0 && allequal == FALSE) {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- "Attribute names match column names but are incorrectly ordered."
        }

        data <- data[ , which(colnames(data) %in% intersection)]
        attributeTable$attributes <- attributeTable$attributes[which(attributeTable$attributes$attributeName %in% intersection), ]
        attributeTable$attributes <- attributeTable$attributes[order(match(attributeTable$attributes$attributeName, colnames(data))), ]
    }

    # Check that type of column matches type of data based on acceptable DataONE formats
    for (i in seq_along(data)) {
        matchingAtt <- attributeTable$attributes[i, ]
        attClass <- class(data[ , i])
        # If matchingAtt has a dateTime domain, coerce the column based on the date/time format
        if (matchingAtt$measurementScale == "dateTime") {
            attClass <- class(suppressWarnings(lubridate::parse_date_time(data[ , i], orders = c("ymd", "HMS", "ymd HMS", "y", "m", "d", "ym", "md", "m/d/y",
                                                                                                 "d/m/y", "ymd HM", "yq",  "j", "H", "M", "S", "MS", "HM", "I",
                                                                                                 "a", "A", "U", "w", "W"))))
        }

        if (attClass == "numeric" || attClass == "integer" || attClass == "double") {
            if (matchingAtt$measurementScale != "ratio" && matchingAtt$measurementScale != "interval" && matchingAtt$measurementScale != "dateTime") {
                status <- "FAILURE"
                messages[[length(messages) + 1]] <- paste0("Mismatch in attribute type for the attribute '", matchingAtt$attributeName,
                                                           "'. Type of data is ", attClass, " which should probably have interval or ratio measurementScale in EML, not ",
                                                           matchingAtt$measurementScale, ".")
            }
        } else if (attClass == "character" || attClass == "logical") {
            if (matchingAtt$measurementScale != "nominal" && matchingAtt$measurementScale != "ordinal") {
                status <- "FAILURE"
                messages[[length(messages) + 1]] <- paste0("Mismatch in attribute type for the attribute '", matchingAtt$attributeName,
                                                           "'. Type of data is ", attClass, " which should probably have nominal or ordinal measurementScale in EML, not ",
                                                           matchingAtt$measurementScale, ".")
            }
        } else if (any(attClass %in% c("POSIXct", "POSIXt", "Date", "Period"))) {
            if (matchingAtt$measurementScale != "dateTime") {
                status <- "FAILURE"
                messages[[length(messages) + 1]] <- paste0("Mismatch in attribute type for the attribute '", matchingAtt$attributeName,
                                                           "'. Type of data is ", attClass, " which should probably have dateTime measurementScale in EML, not ",
                                                           matchingAtt$measurementScale, ".")
            }
        }
    }

    # Check that enumerated domains match values in data
    if (length(attributeTable$factors) > 0) {
        for (i in seq_along(unique(attributeTable$factors$attributeName))) {
            emlAttName <- unique(attributeTable$factors$attributeName)[i]
            emlUniqueValues <- attributeTable$factors[attributeTable$factors$attributeName == emlAttName, "code"]

            dataUniqueValues <- unique(stats::na.omit(data[[which(colnames(data) == emlAttName)]])) # omit NAs in unique values

            intersection <- intersect(dataUniqueValues, emlUniqueValues)
            nonmatcheml <- emlUniqueValues[!emlUniqueValues %in% intersection]
            nonmatchdata <- dataUniqueValues[!dataUniqueValues %in% intersection]

            if (length(nonmatcheml) > 0) {
                status <- "FAILURE"
                messages[[length(messages) + 1]] <- paste0("The EML contains the following enumerated domain values for the attribute '",
                                                           as.character(emlAttName), "' that do not appear in the data: ", toString(nonmatcheml, sep = ", "))
            }

            if (length(nonmatchdata) > 0) {
                status <- "FAILURE"
                messages[[length(messages) + 1]] <- paste0("The data contains the following enumerated domain values for the attribute '",
                                                           as.character(emlAttName), "' that do not appear in the EML: ", toString(nonmatchdata, sep = ", "))
            }
        }
    }

    # If there are any missing values in the data, check that there is an associated missing value code in the EML
    for (i in which(colSums(is.na(data)) > 0)) { # only checks for NA values but others like -99 or -999 could be present
        attribute <- attributeTable$attributes[i, ]

        if (is.na(attribute$missingValueCode)) {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- paste0("The attribute '", attribute$attributeName, "' contains missing values but does not have a missing value code.")
        }
    }

    return(list(status = status,
                output = messages))
}


#' Check rights and access for creators in system metadata
#'
#' This function checks if the creators of a data package have
#' rights and access set in the system metadata.
#'
#' @param sysmeta (SystemMetadata) A system metadata object.
#' @param eml (eml) An EML object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom datapack hasAccessRule
#' @importFrom methods is
#' @importFrom stringr str_extract_all
#'
#' @noRd
qa_rights_access <- function(sysmeta, eml) {
    stopifnot(methods::is(sysmeta, "SystemMetadata"))
    stopifnot(methods::is(eml, "eml"))

    # Assume that the check will succeed, until proven otherwise
    status <- "SUCCESS"
    # Store output messages in a vector
    messages <- c()

    # Check if the rightsHolder is a creator
    rightsHolder <- sysmeta@rightsHolder
    userIds <- unlist(EML::eml_get(eml@dataset@creator, "userId"))
    orcids <- unlist(stringr::str_extract_all(userIds, "http[s]?:\\/\\/orcid.org\\/[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}"))
    orcids <- sub("^https://", "http://", orcids)

    if (length(orcids) == 0) {
        status <- "FAILURE"
        messages[[length(messages) + 1]] <- "No creators have an ORCID. Unable to check if rights holder is one of the creators."
    } else {
        if (rightsHolder %in% orcids) {
            messages[[length(messages) + 1]] <- sprintf("The rights holder, %s, is one of the creators.", rightsHolder)
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- sprintf("The rights holder, %s, is not one of the creators.", rightsHolder)
        }
    }

    # Check if creators have full access
    for (creator in orcids) {
        creator_read <- datapack::hasAccessRule(sysmeta, creator, "read")
        creator_write <- datapack::hasAccessRule(sysmeta, creator, "write")
        creator_changePermission <- datapack::hasAccessRule(sysmeta, creator, "changePermission")
        access <- c(creator_read, creator_write, creator_changePermission)

        if (all(access)) {
            messages[[length(messages) + 1]] <- sprintf("Full access is set for creator: %s", creator)
        } else {
            status <- "FAILURE"
            messages[[length(messages) + 1]] <- sprintf("Full access is not set for creator: %s", creator)
        }
    }

    return(list(status = status,
                output = messages))
}


#' Check if format IDs match in EML and system metadata
#'
#' This function checks if the format IDs for data objects match in the EML and system metadata.
#'
#' @param sysmeta (SystemMetadata) A system metadata object.
#' @param entity (eml) An EML entity element associated with a data object.
#'
#' @return (list) A list of results.
#'
#' @import EML
#' @importFrom methods is
#'
#' @noRd
qa_formatId <- function(sysmeta, entity) {
    stopifnot(methods::is(sysmeta, "SystemMetadata"))
    stopifnot(any(c("dataTable", "otherEntity", "spatialVector", "spatialRaster", "storedProcedure", "view") %in% class(entity)))

    sysmeta_format <- sysmeta@formatId
    eml_format <- EML::eml_get(entity, "formatName")

    if (identical(sysmeta_format, eml_format)) {
        return(list(status = "SUCCESS",
                    output = "Format IDs in EML and system metadata are identical."))
    } else {
        return(list(status = "FAILURE",
                    output = "Format IDs in EML and system metadata are not identical."))
    }
}


#' Quality assurance for system metadata
#'
#' This function checks the quality of system metadata for all elements of a data package,
#' including the resource map, metadata, and all data objects.
#'
#' @param mn (MNode) The Member Node to query.
#' @param resource_map_pid (character) The PID for a resource map.
#' @param all_results (logical) Return all results. If `FALSE`, only returns results with FAILURE or ERROR status.
#'
#' @return (list) A list of results.
#'
#' @import arcticdatautils
#' @import dataone
#' @import EML
#' @importFrom stringr str_detect
#'
#' @export
#'
#' @seealso [qa_package()]
#'
#' @examples
#' \dontrun{
#' # Results with FAILURE or ERROR status
#' qa_sysmeta(knb_prod, "resource_map_urn:uuid:...")
#'
#' # All results
#' qa_sysmeta(knb_prod, "resource_map_urn:uuid:...", all_results = TRUE)
#' }
qa_sysmeta <- function(mn, resource_map_pid, all_results = FALSE) {
    stopifnot(class(mn) %in% c("MNode", "CNode"))
    stopifnot(is.character(resource_map_pid), nchar(resource_map_pid) > 0)
    stopifnot(is.logical(all_results))

    package <- tryCatch(suppressWarnings(arcticdatautils::get_package(mn, resource_map_pid, file_names = TRUE)),
                        error = function(e) stop("Failed to get package. Is the Member Node correct? Is your DataONE token set?"))

    eml <- EML::read_eml(dataone::getObject(mn, package$metadata))
    eml_objects <- c(EML::eml_get(eml, "dataTable"),
                     EML::eml_get(eml, "otherEntity"),
                     EML::eml_get(eml, "spatialVector"))
    if (length(eml_objects) == 0) {
        return(list("qa_sysmeta" = list(status = "FAILURE",
                                        output = "No data objects of a supported format were found in the EML.")))
    }
    # Preserve order of getting data objects based on data type for correct name assignment
    # Entity names may not match object names, so use objectName to ensure matches with sysmeta names
    names(eml_objects) <- unlist(c(EML::eml_get(eml@dataset@dataTable, "objectName"),
                                   EML::eml_get(eml@dataset@otherEntity, "objectName"),
                                   EML::eml_get(eml@dataset@spatialVector, "objectName")))
    # If object names are missing, use entity names instead
    if (is.null(names(eml_objects)) || any(is.na(names(eml_objects)))) {
        names(eml_objects) <- unlist(c(EML::eml_get(eml@dataset@dataTable, "entityName"),
                                       EML::eml_get(eml@dataset@otherEntity, "entityName"),
                                       EML::eml_get(eml@dataset@spatialVector, "entityName")))
    }

    sysmeta_rm <- dataone::getSystemMetadata(mn, package$resource_map)
    sysmeta_md <- dataone::getSystemMetadata(mn, package$metadata)
    sysmeta_data <- lapply(package$data, function(x) dataone::getSystemMetadata(mn, x))

    # If missing fileName, assign name to sysmeta for data objects
    for (i in seq_along(sysmeta_data)) {
        if (is.na(names(sysmeta_data)[[i]])) {
            id <- sysmeta_data[[i]]@identifier
            j <- which(stringr::str_detect(EML::eml_get(eml_objects, "url"), id))
            names(sysmeta_data)[[i]] <-
                if (!is.na(EML::eml_get(eml_objects[[j]], "objectName"))) {
                    EML::eml_get(eml_objects[[j]], "objectName")
                } else {
                    EML::eml_get(eml_objects[[j]], "entityName")
                }
        }
    }

    sysmeta_all <- c(sysmeta_rm, sysmeta_md, sysmeta_data)
    names(sysmeta_all)[[1]] <- "Resource_Map"
    names(sysmeta_all)[[2]] <- "Metadata"

    # Index objects in parallel based on names (in ascending order) for correct processing in iterations
    eml_objects <- eml_objects[order(names(eml_objects))]
    sysmeta_data <- sysmeta_data[order(names(sysmeta_data))]

    # Use tryCatch to return ERROR status if error is encountered
    err <- function(e) list(status = "ERROR")

    rights_access <- tryCatch(lapply(sysmeta_all, qa_rights_access, eml), error = err)
    format <- tryCatch(mapply(qa_formatId, sysmeta_data, eml_objects,
                              SIMPLIFY = FALSE), error = err) # do not simplify to ensure a list is returned

    results <- c(list("Rights_and_Access" = rights_access),
                 list("Format_ID" = format))

    # Default to only return results with FAILURE or ERROR status
    if (all_results) {
        return(results)
    } else {
        results <- lapply(results, function(x) Filter(function(x) stringr::str_detect(x$status, "FAILURE|ERROR"), x))
        # If all statuses are SUCCESS for certain checks, returns empty named list, so remove before returning results
        if (length(results[[2]]) == 0) results[[2]] <- NULL # Format_ID
        if (length(results[[1]]) == 0) results[[1]] <- NULL # Rights_and_Access
        if (length(results[[1]]) > 0) {
            return(results)
        } else {
            return(list("System_Metadata" = list(status = "SUCCESS",
                                                 output = "All QA checks were successful.")))
        }
    }
}


#' Quality assurance for data package metadata
#'
#' For a single data package, this function checks the quality of the EML metadata
#' as well as the system metadata for all package elements, including
#' the resource map, metadata, and all data objects.
#'
#' This function is a wrapper around [qa_eml()], [qa_congruence()], and [qa_sysmeta()].
#'
#' @param mn (MNode) The Member Node to query.
#' @param resource_map_pid (character) The PID for a resource map.
#' @param congruence (logical) Check congruence between data and and metadata attributes.
#' @param read_all_data (logical) Required if `congruence = TRUE`. Read all data from remote. If `FALSE`, only read first 10 rows.
#' @param all_results (logical) Return all results. If `FALSE`, only returns results with FAILURE or ERROR status.
#'
#' @return (list) A list of results.
#'
#' @import arcticdatautils
#' @import dataone
#' @import EML
#'
#' @export
#'
#' @seealso [qa_eml()] [qa_congruence()] [qa_sysmeta()]
#'
#' @examples
#' \dontrun{
#' # Results with FAILURE or ERROR status without checking congruence
#' qa_package(knb_prod, "resource_map_urn:uuid:...")
#'
#' # All results without checking congruence
#' qa_package(knb_prod, "resource_map_urn:uuid:...", all_results = TRUE)
#'
#' # Results with FAILURE or ERROR status and including congruence checks
#' qa_package(knb_prod, "resource_map_urn:uuid:...", congruence = TRUE)
#' }
qa_package <- function(mn, resource_map_pid, congruence = FALSE, read_all_data = TRUE, all_results = FALSE) {
    stopifnot(class(mn) %in% c("MNode", "CNode"))
    stopifnot(is.character(resource_map_pid), nchar(resource_map_pid) > 0)
    stopifnot(is.logical(congruence))
    stopifnot(is.logical(read_all_data))
    stopifnot(is.logical(all_results))

    package <- tryCatch(suppressWarnings(arcticdatautils::get_package(mn, resource_map_pid, file_names = TRUE)),
                        error = function(e) stop("Failed to get package. Is the Member Node correct? Is your DataONE token set?"))

    eml <- EML::read_eml(dataone::getObject(mn, package$metadata))

    results_eml <- qa_eml(eml, all_results)
    results_congruence <- if (congruence) list("Congruence" = qa_congruence(mn, resource_map_pid, read_all_data, all_results)) else {}
    results_sysmeta <- qa_sysmeta(mn, resource_map_pid, all_results)
    results <- c(results_eml, results_congruence, results_sysmeta)

    return(results)
}


#' View QA results in viewer
#'
#' This is a convenience function for pretty output of QA results,
#' using [listviewer::jsonedit()].
#'
#' @param input (list) A list of QA results.
#'
#' @importFrom listviewer jsonedit
#'
#' @export
#'
#' @seealso [qa_package()] [qa_eml()] [qa_sysmeta()]
#'
#' @examples
#' \dontrun{
#' qa_view(qa_package(knb_prod, "resource_map_urn:uuid:..."))
#' }
qa_view <- function(input) {
    stopifnot(is.list(input))

    listviewer::jsonedit(input, mode = "view")
}
