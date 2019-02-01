#' Edit a single attribute
#'
#' This function edits the slots of a single attribute in an existing list of attributes for a data object.
#'
#' This function can only be used on attributes entirely defined within the 'attributes' slot of attributeList;
#' it cannot be used to edit the factors table of an enumeratedDomain.
#'
#' In cases with very large attribute lists, use [which_in_eml()] first to locate
#' the attribute index number in the attributeList.
#'
#' @param attribute (attribute) The attribute in the the attributeList of a data object.
#' @param attributeName (character) The new name to give to the attribute.
#' @param attributeLabel (character) The new label to give to the attribute.
#' @param attributeDefinition (character) The new attributeDefinition to give to the attribute.
#' @param domain (character) The new domain to give to the attribute.
#' @param measurementScale (character) The new measurementScale to give to the attribute.
#' @param unit (character) The new unit (for numericDomain) to give to the attribute.
#' @param numberType (character) The new numberType (for numericDomain) to give to the attribute.
#' @param definition (character) The new definition (for textDomain) to give to the attribute.
#' @param formatString (character) The new formatString (for dateTime) to give to the attribute.
#' @param missingValueCode (character) The new missing value code to give to the attribute.
#' @param missingValueCodeExplanation (character) The new missing value code explanation to give to the attribute.
#'
#' @return (attribute) The modified attribute.
#'
#' @import EML
#' @importFrom methods new
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Change an attribute's name
#' new_attribute <- edit_attribute(eml@dataset@dataTable[[1]]@attributeList@attribute[[8]],
#' attributeName = "new name")
#' eml@dataset@dataTable[[1]]@attributeList@attribute[[8]] <- new_attribute
#'
#' # Change an attribute's measurementScale from nominal to ratio
#' # (also requires changing domain, unit, and numberType as well
#' # as setting definition to NA)
#' new_attribute <- edit_attribute(eml@dataset@otherEntity[[2]]@attributeList@attribute[[1]],
#' domain = "numericDomain", measurementScale = "ratio", unit = "dimensionless",
#' numberType = "whole", definition = NA)
#' eml@dataset@otherEntity[[2]]@attributeList@attribute[[1]] <- new_attribute
#'
#' # Add the same missing value codes to all attributes for a data object
#' new_attributes <- lapply(eml@dataset@dataTable[[1]]@attributeList@attribute, edit_attribute,
#' missingValueCode = "NA", missingValueCodeExplanation = "data unavailable")
#' eml@dataset@dataTable[[1]]@attributeList@attribute <- new_attributes
#' }
edit_attribute <- function(attribute, attributeName = NULL, attributeLabel = NULL, attributeDefinition = NULL, domain = NULL,
                           measurementScale = NULL, unit = NULL, numberType = NULL, definition = NULL, formatString = NULL,
                           missingValueCode = NULL, missingValueCodeExplanation = NULL) {
    stopifnot(methods::is(attribute, "attribute"))
    if (!is.null(attributeName)) stopifnot(is.character(attributeName) && nchar(attributeName) > 0)
    if (!is.null(attributeLabel) && !is.na(attributeLabel)) stopifnot(is.character(attributeLabel) && nchar(attributeLabel) > 0)
    if (!is.null(attributeDefinition)) stopifnot(is.character(attributeDefinition) && nchar(attributeDefinition) > 0)
    if (!is.null(domain)) stopifnot(is.character(domain) && any(c("textDomain", "enumeratedDomain", "numericDomain", "dateTime") %in% domain))
    if (!is.null(measurementScale)) stopifnot(is.character(measurementScale) && any(c("nominal", "ordinal", "dateTime", "ratio", "interval") %in% measurementScale))
    if (!is.null(unit) && !is.na(unit)) stopifnot(is.character(unit) && nchar(unit) > 0)
    if (!is.null(numberType) && !is.na(numberType)) stopifnot(is.character(numberType) && any(c("real", "natural", "whole", "integer") %in% numberType))
    if (!is.null(definition) && !is.na(definition)) stopifnot(is.character(definition) && nchar(definition) > 0)
    if (!is.null(formatString) && !is.na(formatString)) stopifnot(is.character(formatString) && nchar(formatString) > 0)
    if (!is.null(missingValueCode) && !is.na(missingValueCode)) stopifnot(is.character(missingValueCode) && nchar(missingValueCode) > 0)
    if (!is.null(missingValueCodeExplanation) && !is.na(missingValueCodeExplanation)) stopifnot(is.character(missingValueCodeExplanation) && nchar(missingValueCodeExplanation) > 0)
    if (length(c(missingValueCode, missingValueCodeExplanation)) == 1) stop("Need both missingValueCode and missingValueCodeExplanation.")

    # Assign attribute to attributeList in order to convert attribute slots to data.frame
    attList <- methods::new("attributeList")
    attList@attribute[[1]] <- attribute
    data <- EML::get_attributes(attList)

    # enumeratedDomain does not contain the following fields
    if (!is.null(data$factors)) {
        fields <-c(unit, numberType, definition, formatString)
        if (!is.null(fields)) {
            stop('enumeratedDomain attributes cannot contain "unit", "numberType", "definition", or "formatString"')
        }
    }

    attribute_edits <- cbind(attributeName, attributeLabel, attributeDefinition, domain,
                             measurementScale, unit, numberType, definition, formatString,
                             missingValueCode, missingValueCodeExplanation)

    for (i in colnames(attribute_edits)) {
        data[ , i] <- attribute_edits[ , i]
    }

    # Set edits to attributeList in order to convert data.frame to new attribute
    new_attribute <- EML::set_attributes(data$attributes, data$factors)@attribute[[1]]
    new_attribute
}
