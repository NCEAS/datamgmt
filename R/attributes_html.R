#' Handsontable attributes
#'
#' Function to call handsontable widget to build attributes
#'
#' @param df (data.frame) the data.frame of data that needs an attribute table
#' @param type (character) either "attributes", "units", or "factors"
#' @importFrom jsonlite toJSON
hot_attributes_table <- function(df, type = NULL) {

    stopifnot(is.data.frame(df))

    # Build handsontable columns
    columns <- lapply(colnames(df), function(col) {

        # Initiallize options
        render_type = "text"
        colWidths = 150

        if (col %in% c("attributeDefinition", "definition", "description")) {
            colWidths = 320
        }

        if (col %in% c("domain", "measurementScale", "numberType")) {
            render_type = "dropdown"
        }

        out = list(
            type = render_type,
            colWidths = colWidths,
            allowInvalid = TRUE # https://github.com/handsontable/handsontable/issues/4551
        )

        # Set renderers
        if (is.null(type)) {
            type = NULL

        } else if (type == "attributes") {

            if (out$type == "dropdown") {
                out$renderer = htmlwidgets::JS("customDropdown_att")

            } else {
                out$renderer = htmlwidgets::JS("customText_att")}

        } else if (type == "units") {
            out$renderer = htmlwidgets::JS("customText_units")

        } else if (type == "factors") {
            out$renderer = htmlwidgets::JS("customText")

        } else {
            out$renderer = NULL
        }

        out
    })

    # Create widget input
    x <- list(
        data = jsonlite::toJSON(as.matrix(df), na = "null"),
        colHeaders = colnames(df),
        columns = columns,
        rType = type
    )

    # Create the widget
    hot <- htmlwidgets::createWidget("hot_attributes_table", x, width = NULL, height = NULL, package = 'datamgmt')
    hot
}

hot_attributes_output <- function(outputId, width = "100%", height = "100%") {
    htmlwidgets::shinyWidgetOutput(outputId, "hot_attributes_table", width, height, package = "datamgmt")
}

render_hot_attributes <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) { expr <- substitute(expr) } # force quoted
    htmlwidgets::shinyRenderWidget(expr, hot_attributes_output, env, quoted = TRUE)
}
