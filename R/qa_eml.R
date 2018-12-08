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
        if (length(input@dataset@abstract@section) > 0) {
            # There can be multiple 'section' but this code only extracts the first
            text <- xml2::as_list(input@dataset@abstract@section[[1]]@.Data[[1]])
            abstract <- paste(text, collapse = " ")
            length_abstract <- length(abstract)
        } else if (length(input@dataset@abstract@para) > 0) {
            # There can be multiple 'para' but this code only extracts the first
            text <- xml2::as_list(input@dataset@abstract@para[[1]]@.Data[[1]])
            abstract <- paste(text, collapse = " ")
            length_abstract <- length(abstract)
        } else {
            abstract <- input@dataset@abstract@.Data
            length_abstract <- length(abstract)
        }
    } else if (methods::is(input, "abstract")) {
        if (length(input@section) > 0) {
            # There can be multiple 'section' but this code only extracts the first
            text <- xml2::as_list(input@section[[1]]@.Data[[1]])
            abstract <- paste(text, collapse = " ")
            length_abstract <- length(abstract)
        } else if (length(input@para) > 0) {
            # There can be multiple 'para' but this code only extracts the first
            text <- xml2::as_list(input@para[[1]]@.Data[[1]])
            abstract <- paste(text, collapse = " ")
            length_abstract <- length(abstract)
        } else {
            abstract <- input@.Data
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


#' Check if temporal coverage is present
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
#' @seealso [qa_sysmeta()]
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
#' @seealso [qa_eml()] [qa_sysmeta()]
#'
#' @examples
#' \dontrun{
#' qa_view(qa_eml(eml))
#'
#' results <- qa_eml(eml)
#' qa_view(results)
#' }
qa_view <- function(input) {
    stopifnot(is.list(input))

    listviewer::jsonedit(input, mode = "view")
}


qa_award_number_present <- function(input) {
    if (methods::is(input, "eml")) {
        awards <- EML::eml_get(input, "funding") %>%
            utils::capture.output() %>%
            stringr::str_extract(">.*<") %>%  # extract all text between <para> </para> tags
            stringr::str_replace_all("<|>", "") %>%
            stats::na.omit() %>%
            as.character()
    } else {
        awards <- input
    }

    if (is.null(awards)) {
        status <- "FAILURE"
        output <- "No award numbers were found."
        mdq_result <- list(status = status,
                           output = list(list(value = output)))
    } else if (length(awards) < 1) {
        status <- "FAILURE"
        output <- paste0("No award numbers were found when one or more were expected.")
        mdq_result <- list(status = status,
                           output = list(list(value = output)))
    } else if (all(nchar(awards) <= 0)) {
        status <- "FAILURE"
        output <- "Of the award numbers found, none were non-zero in length."
        mdq_result <- list(status = status,
                           output = list(list(value = output)))
    } else if (all(sapply(awards, is_whitespace))) {
        status <- "FAILURE"
        output <- "Of the awards numbers found, none were non-whitespace."
        mdq_result <- list(status = status,
                           output = list(list(value = output)))
    } else {
        status <- "SUCCESS"
        output <- "At least one award number was found."
        mdq_result <- list(status = status,
                           output = list(list(value = output)))
    }
    return(mdq_result)
}


# test both cases rangeOfDates beginDate and singleDateTime calendarDate
qa_temporal_start_year <- function(input) {
    if (methods::is(input, "eml")) {
        date <- EML::eml_get(input, "calendarDate") %>%
            lapply(function(x){x}) %>%
            simplify2array()
    } else {
        date <- input
    }
}


# helper function that checks if the input is whitespace
is_whitespace <- function(input) {
    input <- trimws(input, which = "both")
    if (input == "") {
        return(TRUE)
    }
    return(FALSE)
}
