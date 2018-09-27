#' Search through EMLs
#'
#' This function returns indices within an EML list that contain an instance where `test == TRUE`.
#' See examples for more information.
#'
#' @param eml_list (S4/List) An EML list object.
#' @param element (character) Element to evaluate.
#' @param test (function/character) A function to evaluate.
#'   If character, will evaluate if `element == test` (see example 1).
#'
#' @import EML
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Question: Which creators have a surName "Smith"?
#' n <- which_in_eml(eml@@dataset@@creator, "surName", "Smith")
#' # Answer: eml@@dataset@@creator[n]
#'
#' # Question: Which dataTables have an entityName that begins with "2016"
#' n <- which_in_eml(eml@@dataset@@dataTable, "entityName", function(x) {grepl("^2016", x)})
#' # Answer: eml@@dataset@@dataTable[n]
#'
#' # Question: Which attributes in dataTable[[1]] have a numberType "natural"?
#' n <- which_in_eml(eml@@dataset@@dataTable[[1]]@@attributeList@@attribute, "numberType", "natural")
#' # Answer: eml@@dataset@@dataTable[[1]]@@attributeList@@attribute[n]
#'
#' #' # Question: Which dataTables have at least one attribute with a numberType "natural"?
#' n <- which_in_eml(eml@@dataset@@dataTable, "numberType", function(x) {"natural" %in% x})
#' # Answer: eml@@dataset@@dataTable[n]
#' }
which_in_eml <- function(eml_list, element, test) {

    stopifnot(isS4(eml_list))
    stopifnot(methods::is(eml_list,"list"))
    stopifnot(is.character(element))

    if (is.character(test)) {
        value = test
        test = function(x) {x == value}

    } else {
        stopifnot(is.function(test))
    }

    # Find location
    location <- unlist(lapply(seq_along(eml_list), function(i) {
        elements_test <- unlist(EML::eml_get(eml_list[[i]], element))

        if (is.null(elements_test)) {
            out <- NULL

        } else {
            result <- test(elements_test)

            if (length(result) > 1) {
                stop("Test must only return one value.")

            } else if (result == TRUE) {
                out <- i

            } else {
                out <- NULL
            }
        }
        return(out)
    }))

    return(location)
}
