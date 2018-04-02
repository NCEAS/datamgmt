#' Copies udunits2 xml files to datamgmt/UDUNITS
#' Updates uduntis2-accepted.xml with units from EML-units.xml
.onLoad <- function(libname, pkgname) {

    # Get directory for udunits files
    pkg_dir <- system.file(package = "datamgmt")
    ud_dir <- paste0(pkg_dir, "/UDUNITS")
    if (!dir.exists(ud_dir)) {
        dir.create(ud_dir)
    }
    stopifnot(dir.exists(ud_dir))

    # Get udunits2 files
    udunits2_dir <- system.file("share/", package = "udunits2")
    udunits_xmls <- dir(udunits2_dir, full.names = FALSE, recursive = TRUE)
    udunits_xmls_names <- sub("^.*\\/", "", udunits_xmls)

    # Read-in xml files
    n_accepted <- which(udunits_xmls_names == "udunits2-accepted.xml")
    accepted <- xml2::read_xml(paste0(udunits2_dir, "/", udunits_xmls[n_accepted]))

    # Load custom udunits.xml
    datamgmt_dir <- system.file("share/", package = "datamgmt")
    custom_xml <- dir(datamgmt_dir, full.names = FALSE)
    n_custom <- which(custom_xml == "EML-units.xml")
    custom <- xml2::read_xml(paste0(datamgmt_dir, "/", custom_xml[n_custom]))
    custom_units <- xml2::xml_children(custom)

    # Add custom units to udunits2-accepted.xml
    for (x in custom_units) {
        xml2::xml_add_child(accepted, x)
    }

    # Write files to ud_dir
    xml2::write_html(accepted, paste0(ud_dir, "/", "udunits2-accepted.xml"),
                     encoding = "US-ASCII")
    copied <- file.copy(paste0(udunits2_dir, "/", udunits_xmls[-n_accepted]),
                        ud_dir, overwrite = T)
}

#' Will return TRUE if properly loaded or FALSE if not
#' @return (logical)
#' @import gsubfn
#' @import udunits2
#' @import xml2
#' @import units
#' @importFrom stringi stri_reverse
#' @importFrom compare compare
#' @importFrom memoise memoise
#' @importFrom utils setTxtProgressBar txtProgressBar
set_custom_UDUNITS <- function() {
    pkg_dir <- system.file(package = "datamgmt")
    ud_dir <- paste0(pkg_dir, "/UDUNITS")

    # Load custom udunits2.xml
    p0 <- paste0(ud_dir, "/", "udunits2.xml")
    Sys.setenv(UDUNITS2_XML_PATH = p0)
    udunits2:::.onLoad(dirname(system.file(package = "udunits2")), "udunits2") #This is known to fail on Windows https://github.com/pacificclimate/Rudunits2/issues/21
    Sys.getenv("UDUNITS2_XML_PATH") == p0
}

#' Loads udunits_units
#' @return (data.frame)
load_udunits <- function() {

    # Get unit systems
    prefixes <- suppressMessages(units::valid_udunits_prefixes())
    udunits_unit_system <- suppressMessages(units::valid_udunits())

    # Get all single and plural units and symbols
    udunits_units <- lapply(seq_len(nrow(udunits_unit_system)), function(i) {
        # Single
        name_singular <- paste0(udunits_unit_system[i, ]$name_singular,
                                ", ", udunits_unit_system[i, ]$name_singular_aliases)
        name_singular <- unlist(strsplit(name_singular, ", "))

        # Plural
        name_plural_test <- paste0(udunits_unit_system[i, ]$name_plural,
                                   ", ", udunits_unit_system[i, ]$name_plural_aliases)
        name_plural_test <- unlist(strsplit(name_plural_test, ", "))

        # See if there is a plural version for each singular version
        if (length(name_plural_test) == length(name_singular)) {
            name_plural <- name_plural_test
        } else {
            name_plural <- rep(udunits_unit_system[i, ]$name_plural, length(name_singular))
        }

        # Symbols
        symbol <- rep(paste0(udunits_unit_system[i, ]$symbol), length(name_singular))

        #Dimensionless
        dimensionless <- rep(paste0(udunits_unit_system[i, ]$dimensionless), length(name_singular))

        # Data frame
        data.frame(name_singular, name_plural, symbol, dimensionless, stringsAsFactors = F)
    })

    udunits_units <- do.call(rbind, udunits_units)
    udunits_units <- udunits_units[udunits_units$name_singular != "", ]

    out <- list(udunits_units = udunits_units, prefixes = prefixes)

    return(out)
}

#' Gets plural form of unit
#' Based on ut_form_plural function in xml.c of udunits2
#' @param unit (character) unit
#' @param udunits_units (data.frame) load_udunits()
#' @return (character) plural form of unit
get_plural <- function(unit, udunits_units = load_udunits()$udunits_units) {

    if (is.na(unit)) {
        unit <- ""
    }

    stopifnot(is.character(unit))

    singular <- tolower(unit)

    n_singular <- which(singular == tolower(udunits_units$name_singular))

    if (length(n_singular) > 0) {
        plural <- udunits_units$name_plural[n_singular]
    } else {
        # No unit found in udunits_units
        plural <- ""
        unit <- ""
    }

    # Known exceptions
    plural <- ifelse(unit == "siemens", "siemens", plural)
    plural <- ifelse(unit == "hertz", "hertz", plural)
    plural <- ifelse(unit == "dimensionless", "dimensionless", plural)
    plural <- ifelse(unit == "number", "number", plural)

    # If plural is not defined, define it
    if (plural == "" && unit != "") {
        unit_length <- nchar(singular)
        last_char <- substr(singular, unit_length, unit_length)
        penultimate_char = substr(singular, (unit_length - 1), (unit_length - 1))

        if (last_char == "y") {

            if (penultimate_char == "a" || penultimate_char == "e" || penultimate_char == "i" ||
                penultimate_char == "o" || penultimate_char == "u") {
                plural <- paste0(unit, "s")

            } else {
                plural <- paste0(substr(unit, 1, (unit_length - 1)), "ies")
            }

        } else {
            if (last_char == "s" || last_char == "x" || last_char == "z" ||
                paste0(penultimate_char, last_char) == "ch" ||
                paste0(penultimate_char, last_char) == "sh") {
                plural <- paste0(unit, "es")
            } else {
                plural <- ifelse(unit == "", NA, paste0(unit, "s"))
            }
        }
    }
    return(plural)
}

#' Load all forms of units in udunit list
#' @return (data.frame)
load_all_units <- function() {

    udunits_units_out <- load_udunits()
    udunits_units <- udunits_units_out$udunits_units
    prefixes <- udunits_units_out$prefixes

    # Add a blank character to prefixes to allow for no prefix
    prefixes_symbol <- c(paste0(prefixes$symbol), "")
    prefixes_name <- c(paste0(prefixes$name), "")

    # remove allready prefixed names, i.e. kilogram
    udunits_units <- udunits_units[!grepl(paste0("^", prefixes$name, collapse = "|"),
                                          udunits_units$name_singular), ]

    # Get all plurals
    udunits_units$name_plural <- unlist(lapply(seq_len(nrow(udunits_units)),
                                               function(i) {
                                                   if (udunits_units[i, ]$name_plural == "") {
                                                       name_plural <- get_plural(udunits_units[i, ]$name_singular,
                                                                                 udunits_units)
                                                   } else {
                                                       name_plural <- udunits_units[i, ]$name_plural
                                                   }
                                                   return(name_plural)
                                               }))

    # Add prefixes to singles, plurals, and symbols
    unit_single <- unlist(lapply(seq_along(udunits_units$name_singular), function(i) {
        x <- udunits_units$name_singular[i]
        if (udunits_units$dimensionless[i]) {
            paste0(x)
        } else {
            paste0(prefixes_name, x)}
    }))

    unit_plural <- unlist(lapply(seq_along(udunits_units$name_singular), function(i) {
        x <- udunits_units$name_plural[i]
        if (udunits_units$dimensionless[i]) {
            paste0(x)
        } else {
            paste0(prefixes_name, x)}
    }))

    symbol <- unlist(lapply(seq_along(udunits_units$name_singular), function(i) {
        x <- udunits_units$symbol[i]
        if (udunits_units$dimensionless[i]) {
            paste0(x)
        } else if (grepl("[[:alpha:]]", x)) {
            paste0(prefixes_symbol, x)
        } else {
            out <- rep("", length(prefixes_symbol) - 1)
            c(out, x)
        }
    }))

    all_units <- data.frame(unit_single, unit_plural, symbol, stringsAsFactors = F)
}

#' Will attempt to deparse unit with  units::deparse_unit(units::as.units(unit))
#' @param unit (character)
#' @param exponents (character)
#' @param exponents_numeric (character)
#' @param all_units (data.frame)
#' @return (character) unit. Fails if cannot deparse.
try_units_deparse <- function(unit, exponents, exponents_numeric, all_units = load_all_units()) {

    stopifnot(length(unlist(gregexpr("\\(", unit))) == length(unlist(gregexpr("\\)", unit))))
    stopifnot(!grepl("\\([^\\)]*\\(", unit)) # stop if nested parenthesis
    stopifnot(!grepl("\\([^\\)]*\\/[^\\)]*\\)", unit)) # stop if fractions in parenthesis

    # Preformat unit
    unit <- gsub("(\\^)(-{0,1}[[:digit:]]+)", "\\2", unit)  # remove ^ in front of digits
    unit <- gsub("(^|[[:blank:]]+)[p|P]er[[:blank:]]+"," / ", unit) # remove "per"

    # Deal with parenthesis
    unit <- stringi::stri_reverse(gsub("([[:blank:]]|\\*)(?=[^\\)]+\\({1}[[:blank:]]*\\/{1})", "/", stringi::stri_reverse(unit), perl = TRUE))
    unit <- gsub("\\(|\\)", " ", unit) # remove parenthesis
    unit <- gsub("\\*", " ", unit) # remove "*"

    unit <- gsub("([[:blank:]]*\\/{1}[[:blank:]]*)([[:alpha:]_]+)(-{0,1}[[:digit:]]+|[[:blank:]]*)",
                 " \\2-\\3 ", unit)  # remove / and add - to exponent
    unit <- gsub("(-{2})([[:digit:]])", "\\2", unit)  # change -- to -
    unit <- gsub("-{1}[[:blank:]]{1}", "-1 ", unit)  # change -[[:blank:]] to -1

    # Remove leading 1 from instances like "1/m"
    unit <- gsub("(^|[[:blank:]])1", "", unit)

    unit <- gsub("[[:blank:]]+", " ", unit)  # remove multple spaces
    unit <- gsub("^[[:blank:]]|[[:blank:]]$", "", unit)  # remove leading/trailing spaces

    # Attempt to deparse unit
    if (grepl("^[[:blank:]]*\\/", unit)) {
        unit <- units::deparse_unit(units::as.units(sub("^[[:blank:]]*\\/","",unit)))
        unit <- paste0("per ", unit)
    } else {
        unit <- units::deparse_unit(units::as.units(unit))
    }

    # Change exponent form
    unit <- gsub("([^[:blank:]-]+)(-{0,1}[[:digit:]]+)([[:blank:]]|$)", " \\2 \\1 ", unit)
    unit <- gsub("(-)([[:digit:]]+)", "per \\2", unit)
    for (i in seq_along(exponents_numeric)) {
        unit <- gsub(paste0("[[:blank:]]", exponents_numeric[i], "[[:blank:]]"),
                     paste0(" ", exponents[i], " "), unit)
    }

    # Split unit
    unit_split <- unlist(strsplit(unit, "[[:blank:]]+"))

    # Substitute unit symbol for unit_single
    unit_split <- unlist(lapply(unit_split, function(x) {
        if (!(x %in% c("", " ", "per", exponents)) && x %in% all_units$symbol &&
            !(x %in% all_units$unit_single) && !(x %in% all_units$unit_plural)) {
            x <- all_units$unit_single[which(all_units$symbol == x)]
        }
        x[1]
    }))

    unit <- paste0(unit_split, collapse = " ")

    # Don't return non alphanumeric, _, or " " results
    stopifnot(!grepl("[^[:alnum:]+_+ ]", unit))
    return(unit)
}

#' Performs formating on unit and splits into components
#' @param unit (character) unit
#' @param all_units (data.frame)
#' @return (character) unit_split
get_unit_split <- function(unit, all_units = mem_load_all_units()) {

    # Try to run gsubfn, if can't set gsubfn.engine
    tryCatch({
        invisible(gsubfn::strapply("", ""))
    }, error = function(e) {
        options(gsubfn.engine = "R")
    })

    if (is.na(unit)) {
        unit <- ""
    }

    # Initiallize exponents
    exponents <- c("square", "cubic")
    exponents_numeric <- c("2", "3")
    exponents_bad <- c("squared", "cubed")

    # if symbolic, use units package to try to deparse
    unit <- suppressWarnings(tryCatch({
        out <- try_units_deparse(unit, exponents, exponents_numeric, all_units)
        stopifnot(out != "")
        out},
        error = function(e) {
            unit
        }))

    # Replace '/' with ' Per '
    unit <- gsub("\\/", " Per ", unit)

    # Remove units with symbols other than '_'
    is_bad <- grepl("[^[:alnum:]+_+ ]", unit)
    if (is_bad) {
        unit <- ""
    }

    # Remove blanks
    has_blank <- grepl("[[:blank:]]", unit)
    if (has_blank) {
        unit_split <- unlist(strsplit(unit, "[[:blank:]]+"))
        if (length(unit_split) > 1) {
            unit_split[-1] <- unlist(lapply(unit_split[-1], function(x) {
                x <- sub(paste0("^", substr(x, 1, 1)), toupper(substr(x,
                                                                      1, 1)), x)
            }))
        }
        unit <- paste0(unit_split, collapse = "")
    }

    # Split string at capitals
    exclusions <- c("[[:lower:]]+",
                    "[[:upper:]][[:lower:]]+",
                    "[[:lower:]]+_[[:upper:]][[:lower:]]+",
                    "[[:lower:]]+_[[:lower:]]+",
                    "[[:upper:]][[:lower:]]+_[[:lower:]]+",
                    "[[:upper:]][[:lower:]]+_[[:upper:]][[:lower:]]+")
    unit_split <- unlist(gsubfn::strapply(unit, paste(exclusions, collapse = "|")))

    if (is.null(unit_split)) {
        unit_split <- ""
    }

    # Get proper form of unit and split_type
    split_type <- rep("", length(unit_split))
    for (i in seq_along(unit_split)) {

        # Find which unit in all_units
        n_single <- which(tolower(unit_split[i]) == tolower(all_units$unit_single))
        n_plural <- which(tolower(unit_split[i]) == tolower(all_units$unit_plural))

        # Determine if is an exponent
        is_exponent <- tolower(unit_split[i]) %in% exponents
        is_exponent_bad <- tolower(unit_split[i]) %in% exponents_bad

        # Determine if is "per"
        is_per <- tolower(unit_split[i]) == "per"

        # Set unit_split (with correct capitalization form) and split_type
        if (length(n_single) > 0) {
            unit_split[i] <- all_units$unit_single[n_single]
            split_type[i] <- "unit_single"
        } else if (length(n_plural) > 0) {
            unit_split[i] <- all_units$unit_plural[n_plural]
            split_type[i] <- "unit_plural"
        } else if (is_exponent) {
            split_type[i] <- "exponent"
        } else if (is_exponent_bad) {
            split_type[i] <- "exponent_bad"
        } else if (is_per) {
            split_type[i] <- "per"
        } else {
            split_type[i] <- "unknown"
        }
    }

    # Fix exponents
    n_exp <- which(grepl("^exponent", split_type))
    for (i in n_exp) {

        # Determine if there is a unit lagging the exponent
        lag_unit <- ((i + 1) <= length(unit_split) && (grepl("^unit", split_type[i + 1])))

        # Determine if there is a unit leading the exponent
        lead_unit <- ((i - 1) > 0 && (grepl("^unit", split_type[i - 1])))

        # Determine if there is an exponent leading the unit leading the exponent
        lead2_exp <- ((i - 2) > 0 && (grepl("^exponent", split_type[i - 2])))

        # Set exponent
        n_bad <- which(tolower(unit_split[i]) == exponents_bad)
        if (length(n_bad) > 0) {
            exponent <- exponents[n_bad]
        } else {
            exponent <- unit_split[i]
        }

        # If the exponent is bad, or there is not a lagging unit
        if (length(n_bad) != 0 | !lag_unit) {

            # If there is a leading unit and not a lagging unit and not an exponent leading the leading unit
            # e.g. meters seconds squared
            if (lead_unit & !lag_unit & !lead2_exp) {
                unit_split[i] <- unit_split[i - 1]
                split_type[i] <- split_type[i - 1]
                unit_split[i - 1] <- exponent
                split_type[i - 1] <- "exponent"

                # If there is a lagging unit and not a leading unit
                # e.g. square meter
            } else if (lag_unit & !lead_unit) {
                unit_split[i] <- exponent
                split_type[i] <- "exponent"

                # Otherwise, keep bad exponent to filter at end
            } else {
                split_type[i] <- "exponent_bad"
            }
        }
    }

    # Fix plurals
    n_per <- which(split_type == "per")

    if (length(n_per) == 0) {
        n_per <- 0
    } else {
        n_per <- min(n_per)
    }

    # Go backwards along unit split
    # All units should be single unless immediately preceeding a "per"
    for (i in rev(seq_along(unit_split))) {

        if (i != (n_per - 1) && split_type[i] == "unit_plural") {
            unit_split[i] <- all_units$unit_single[which(all_units$unit_plural == unit_split[i])]
            split_type[i] <- "unit_single"
        }

        if (i == (n_per - 1) && split_type[i] == "unit_single") {
            unit_split[i] <- all_units$unit_plural[which(all_units$unit_single == unit_split[i])]
            split_type[i] <- "unit_plural"
        }
    }

    # Make sure each unit in the denominator is preceeded by "per"
    if (n_per > 0) {
        n_single <- which(split_type == "unit_single")
        n_single_d <- n_single[n_single > n_per]

        # If there are units following "per"
        if (length(n_single_d) > 1) {

            # For each unit following "per" after the first following unit
            for (i in n_single_d[-1]) {

                # Place a "per" before unit
                if (split_type[i - 1] != "per") {
                    j <- ifelse(split_type[i - 1] == "exponent", i - 2, i - 1) # check if there is an exponent
                    unit_split <- append(unit_split, "Per", after = (j))
                    split_type <- append(split_type, "per", after = (j))
                }
            }
        }
    }

    # Set camelCase
    if (length(unit_split) > 1) {
        unit_split[-1] <- unlist(lapply(unit_split[-1], function(x) {
            x <- sub(paste0("^", substr(x, 1, 1)), toupper(substr(x, 1, 1)), x)
        }))
    }
    unit_split[1] <- ifelse(is.na(unit_split[1]),
                            NA,
                            sub(paste0("^", substr(unit_split[1], 1, 1)), tolower(substr(unit_split[1], 1, 1)), unit_split[1]))

    # Check unit
    if (any(split_type == "exponent_bad")) {
        unit_split <- NA
        warning("Exponents are unclear. ", "Ensure proper notation. ",
                "Use 'square' and 'cubic'.")
    }

    if (any(split_type == "unknown") || all(split_type == "per")) {
        unit_split <- NA
    }

    return(unit_split)
}

#' Formats unit_split
#' @param unit_split (character) result of function get_unit_split
#' @param form ('id', 'symbol', 'udunit', 'description'). 'id' is an EML id form. 'symbol' is an EML abbreviation form. 'udunit' is a udunits2 form.
#' @param all_units (data.frame)
#' @return (character) formated unit_split
format_unit_split <- function(unit_split, form = "id", all_units = mem_load_all_units()) {

    if (!is.na(unit_split[1])) {
        exponents <- c("square", "cubic")
        n_per <- which(tolower(unit_split) == "per")

        if (length(n_per) == 0) {
            n_per <- 0
        } else {
            n_per <- min(n_per)
        }

        if (form == "symbol") {
            exponent_symbols <- c("\u00B2", "\u00B3")
        }

        if (form == "udunit") {
            exponent_symbols <- c("2", "3")
        }

        if (form == "id") {
            unit_split <- paste(unit_split, collapse = "")
        } else if (form == "description") {
            unit_split <- paste(tolower(unit_split), collapse = " ")
            unit_split <- gsub("celsius", "Celsius", unit_split)
        } else {

            for (i in rev(seq_along(unit_split))) {
                n_exp <- which(exponents == tolower(unit_split[i]))

                if (form == "symbol" && i <= length(unit_split)) {
                    n <- which(tolower(all_units$unit_single) == tolower(unit_split[i]))

                    if (length(n) == 0) {
                        n <- which(tolower(all_units$unit_plural) == tolower(unit_split[i]))
                    }

                    if (length(n) == 1) {
                        unit_split[i] <- all_units$symbol[n]

                        if (unit_split[i] == "") {
                            unit_split[i] <- all_units$unit_single[n]
                        }
                    }
                }

                if (length(n_exp) > 0) {

                    unit_split[i] <- unit_split[i + 1]
                    unit_split[i + 1] <- exponent_symbols[n_exp]

                    unit_split[i + 1] <- paste0(unit_split[i], unit_split[i + 1], collapse = "")
                    unit_split <- unit_split[-i]

                } else if (form == "symbol" && tolower(unit_split[i]) == "per" && i <= length(unit_split)) {
                    unit_split[i] <- "/"

                } else if (form == "symbol" && (i < n_per || n_per == 0) && i > 1 && !(unit_split[i - 1] %in% exponents)) {
                    unit_split[i] <- paste0("*", unit_split[i])
                }
            }

            if (form == "udunit") {
                unit_split <- paste(unit_split, collapse = " ")
            }

            if (form == "symbol")
                if (any(unit_split == "")) {
                    unit_split <- NA
                } else {
                    unit_split <- paste(unit_split, collapse = "")
                }
        }
    }
    return(unit_split)
}

#' Loads EML unit library from EML::get_unitList()
#' @param all_units (data.frame)
#' @return (list) EML_units
load_EML_units <- function(all_units = mem_load_all_units()) {

    # Get EML Standard units
    unitList <- EML::get_unitList()
    units <- unitList$units
    unitTypes <- unitList$unitTypes

    # Get EML Non Standard units
    unitList_nS <- EML::get_unitList(x = EML::read_eml(system.file("share/EML-nS-unitDictionary.xml",
                                                                   package = "datamgmt")))
    units_nS <- unitList_nS$units
    unitTypes_nS <- unitList_nS$unitTypes

    units <- rbind(units, units_nS)
    unitTypes <- rbind(unitTypes, unitTypes_nS)

    # Set NA to 1 in unitTypes
    unitTypes$power[is.na(unitTypes$power)] <- 1

    # Get EML SI units
    SI_units <- units[grepl("^SI unit", units$description) | units$id == "dimensionless", ]
    EML_SI_units <- units[(!is.na(units$multiplierToSI) & units$multiplierToSI == 1), ]
    EML_SI_units <- rbind(EML_SI_units, units[units$id == "dimensionless",])

    # Exceptions
    EML_SI_units$name[EML_SI_units$name == "waveNumber"] <- "radianPerMeter" # This is a cheat to get udunits to recognize unit as dimensionless / meter
    EML_SI_units$name[EML_SI_units$name == "siemen"] <- "siemens"
    EML_SI_units$name[EML_SI_units$name == "milliGramsPerMilliLiter"] <- "milligramsPerMilliliter"
    EML_SI_units$name[EML_SI_units$name == "molality"] <- "molesPerKilogram"

    # Get udunits for of EML units
    EML_SI_units$ud <- unlist(lapply(EML_SI_units$name, function(x) {
        unit_split <- suppressWarnings(get_unit_split(x, all_units))
        udunit <- format_unit_split(unit_split, form = "udunit")
    }))

    EML_units = list(units = units,
                     unitTypes = unitTypes,
                     SI_units = SI_units,
                     EML_SI_units = EML_SI_units)
    return(EML_units)
}

#' Get the unitType of unit_split
#' @param udunit (character) result of format_unit_split(unit_split, form = 'udunit')
#' @param all_units (data.frame)
#' @param EML_units (list)
#' @return (data.frame) parentSI_df
get_parentSI_df <- function(udunit, all_units = mem_load_all_units(), EML_units = mem_load_EML_units()) {

    has_denominator <- grepl(" *[p|P]er .*", udunit)

    if (has_denominator) {
        numerator <- gsub(" *[p|P]er .*", "", udunit)
        num_dimensionless <- udunits2::ud.are.convertible(numerator, "radian")
    } else {
        num_dimensionless <- FALSE
    }

    # Easy check. Checks to see if unit is convertible with only one EML unit
    n_EML <- unique(unlist(lapply(EML_units$EML_SI_units$ud, function(x) {
        # Careful with reciprocals here, e.g. "seconds-1" and "seconds" return TRUE here
        # but "meter seconds-1" and " meter seconds" return FALSE
        if (num_dimensionless) {
            if (grepl("^per ", udunit)) {
                udunit <-  paste0("radian ", udunit)
            }
            is_ud <- udunits2::ud.are.convertible(udunit, ifelse(grepl(" *[p|P]er .*", x), x, ""))
        } else {
            is_ud <- udunits2::ud.are.convertible(udunit, x)}

        if (is_ud) {
            which(EML_units$EML_SI_units$ud == x)
        } else {
            NULL
        }

    })))

    # Harder check, multiple easy checks were TRUE
    if (length(n_EML) > 1) {

        unit_split <- unlist(strsplit(udunit, " "))
        n_per <- which(tolower(unit_split) == "per")

        if (length(n_per) == 0) {
            n_per <- length(unit_split) + 1
        } else {
            n_per <- min(n_per)
        }

        unitType_l <- rep(list(c(dimension = "", power = "")), length(unit_split))

        # Find dimension and power of all units in unit_split
        for (i in rev(seq_along(unit_split))) {

            unitType_l[[i]] <- unlist(lapply(EML_units$SI_units$id, function(x) {
                out <- NULL
                is_unit <- udunits2::ud.are.convertible(x, gsub("[[:digit:]]$", "", unit_split[i]))

                if (is_unit) {
                    dimension <- EML_units$SI_units$unitType[which(EML_units$SI_units$id == x)]
                    power <- regmatches(unit_split[i], regexpr("[[:digit:]]+$", unit_split[i]))

                    if (length(power) > 0) {
                        power <- as.numeric(power) * (-1)^(i > n_per)
                    } else {
                        power <- (-1)^(i > n_per)
                    }
                    out <- list(c(dimension = dimension, power = power))
                }
                return(out)
            }))
        }
        unitType_df <- as.data.frame(do.call(rbind, unitType_l), stringsAsFactors = F)

        p_unitTypes <- EML_units$EML_SI_units$unitType[n_EML]

        # Compare dimension and power of unitTypes in EML
        n_unitType <- unlist(lapply(p_unitTypes, function(p) {
            test <- compare::compare(EML_units$unitTypes[EML_units$unitTypes$id %in% p,
                                     c("dimension", "power")],
                                     unitType_df,
                                     allowAll = T)$result
            return(test)
        }))

        if (any(n_unitType)) {
            n_EML <- n_EML[n_unitType]
        } else if ("dimensionless" %in% EML_units$EML_SI_units$id[n_EML]) {
            n_EML <- which(EML_units$EML_SI_units$id == "dimensionless")
        }

        if (length(n_EML) > 1) {
            n_EML = numeric(0)
        }
    }

    if (length(n_EML) > 0) {
        unitType <- EML_units$EML_SI_units$unitType[n_EML]
        parentSI <- EML_units$EML_SI_units$id[n_EML]
        parent_ud <- EML_units$EML_SI_units$ud[n_EML]

        if (grepl("^per ", udunit)) {
            udunit <-  paste0("radian ", udunit)
        }

        multiplierToSI <- ifelse(parentSI == "dimensionless", NA, udunits2::ud.convert(1, udunit, parent_ud))

    } else {
        unitType <- NA
        parentSI <- NA
        multiplierToSI <- NA
    }

    parentSI_df <- data.frame(unitType, parentSI, multiplierToSI, stringsAsFactors = F)
    return(parentSI_df)
}

#' Unset custom units
#' @return (logical)
unset_custom_UDUNITS <- function() {
    Sys.unsetenv("UDUNITS2_XML_PATH")
    Sys.getenv("UDUNITS2_XML_PATH") == ""
    udunits2:::.onLoad(dirname(system.file(package = "udunits2")), "udunits2") #This is known to fail on Windows https://github.com/pacificclimate/Rudunits2/issues/21
}

mem_load_all_units <- memoise::memoise(load_all_units)
mem_load_EML_units <- memoise::memoise(load_EML_units)

#' Get custom unit data frame
#' @param units (character) unit or vector of units
#' @param quiet (logical) if true will quiet console text
#' @return (data.frame) custom unit data frame (will return a row of NAs if a unit cannot be formated in an EML form)
#' @description Uses the udunits2 unit library to format inputted unit into an EML unit form.
#' @examples
#' \dontrun{
#' #The following all return the same data frame.
#' get_custom_units('kilometersPerSquareSecond') #preferred input form
#' get_custom_units('Kilometers per seconds squared')
#' get_custom_units('km/s^2')
#' get_custom_units('km s-2')
#' get_custom_units('s-2 /     kilometers-1') #works but is not advised
#' }
#' @export
get_custom_units <- function(units, quiet = FALSE) {

    # Load custom .xml files
    loaded <- suppressPackageStartupMessages(set_custom_UDUNITS())
    if (!loaded) {
        warning("There was an error loading custom udunits files. ", "Not all custom units will be available.")
    }

    stopifnot(is.character(units))

    # Load units
    all_units = mem_load_all_units()
    EML_units = mem_load_EML_units(all_units)
    columns <- c("id", "unitType", "parentSI", "multiplierToSI", "abbreviation", "description")

    # Initillize progress bar
    if (quiet == FALSE) {
        progressBar <- utils::txtProgressBar(min = 0, max = length(units), style = 3)}

    # Get custom units
    custom_units <- lapply(seq_along(units), function(i) {

        if (quiet == FALSE) {
            utils::setTxtProgressBar(progressBar, i)}

        unit_split <- get_unit_split(units[i], all_units)
        id <- format_unit_split(unit_split, form = "id", all_units)

        # First check if unit is standard
        n_id <- which(EML_units$units$id == units[i])

        if (length(n_id) == 0) {
            n_id <- which(EML_units$units$abbreviation == units[i])
        }

        if (length(n_id) == 1) {
            custom_unit <- EML_units$units[n_id, columns]

        } else if (!is.na(id)) {
            udunit <- format_unit_split(unit_split, form = "udunit", all_units)
            abbreviation <- format_unit_split(unit_split, form = "symbol", all_units)
            description <-  format_unit_split(unit_split, form = "description", all_units)
            parentSI_df <- get_parentSI_df(udunit, all_units, EML_units)

            custom_unit <- data.frame(id, parentSI_df, abbreviation, description, stringsAsFactors = F)
        } else {
            warning("Unknown unit ", units[i])
            custom_unit <- data.frame(matrix(ncol = length(columns), nrow = 1), stringsAsFactors = F)
            colnames(custom_unit) <- columns
            custom_unit$id <- units[i]
        }
        rownames(custom_unit) <- c()
        custom_unit
    })

    custom_unit_df <- do.call(rbind, custom_units)
    return(custom_unit_df)
    unset_custom_UDUNITS()
}

