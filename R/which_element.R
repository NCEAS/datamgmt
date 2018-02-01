#' which_element
#'
#' This function returns locations within an EML list object that contain an instance where \code{element == value}
#' @param eml an EML list object
#' @param element element to evaluate
#' @param value string to match \code{element} to
#'
#' @keywords eml
#'
#' @examples
#' which_element(eml@@dataset@@creator,"surName","Smith")
#'
#' which_element(eml@@dataset@@dataTable,"objectName","file.csv")
#'
#' which_element(eml@@dataset@@dataTable[[1]]@@attributeList@@attribute,"numberType","real")
#' @export
which_element <- function(eml,element,value) {

    stopifnot(isS4(eml))
    stopifnot(is(eml,"list"))
    stopifnot(is.character(element))
    stopifnot(is.character(value))

    # find location
    location <- unlist(lapply(seq_along(eml), function(i) {
        elements_test <- eml_get(eml[[i]],element)

        # some elements will be lists and need to be unlisted
        if(is(elements_test,"list")) {
            elements_test <- unlist(elements_test)
        }

        if (is.null(elements_test) || !(value %in% elements_test)) {
            NULL
        }else{
            i
        }
    }))

    return(location)

}

