#' Create/Edit EML attributes
#'
#' create/edit EML attributes, custom units, and factors in a shiny environment
#'
#' @param data (data.frame) the data.frame of data that needs an attribute table
#' @param attributes_table (data.frame) an existing attributes table for \code{data} that needs to be updated (if specified along with \code{data}, all non-empty fields will be used over any values automatically generated from \code{data})
#'
#' @importFrom shinyjs enable disable
#' @import shiny
#'
#' @return Returns a list of three items (an attributes table, a custom units table, a factors table)
#' @examples
#' \dontrun{
#' # Create a blank attributes table
#' foo <- create_attributes_table(NULL, NULL)
#'
#' # Create an attributes table from a data file
#' data <- read.csv('foo.csv')
#' foo <- create_attributes_table(data, NULL)
#'
#' # Edit an attributes table from a eml attributeList
#' attributes_table <- EML::get_attributes(eml@@dataset@@dataTable[[i]]@@attributeList)$attributes
#' foo <- create_attributes_table(NULL, attributes_table)
#'
#' # Edit an attributes table from a eml attributeList and data file
#' foo <- create_attributes_table(data, attributes_table)
#'
#' # Once finished, use EML commands to transform tables into EML objects
#' attributeList <- EML::set_attributes(attributes = foo$attributes, factors = foo$factors)
#' unitlist <- EML::set_unitList(units = foo$units)
#'}
#' @export
create_attributes_table <- function(data = NULL, attributes_table = NULL) {
    fields <- c("attributeName", "measurementScale", "domain", "unit", "numberType", "attributeLabel",
                "attributeDefinition", "definition", "formatString", "missingValueCode", "missingValueCodeExplanation")

    # Prepare attributes from attribute_table
    if (!is.null(attributes_table)) {

        if (!is(attributes_table, "data.frame")) {
            stop("attributes_table must be a data.frame")
        }

        if (length(attributes_table$attributeName) == 0) {
            stop("attributeName must be a non-empty field in attributes_table")
        }

        # Find which fields are not present and set to NA
        fields_not_in <- which(!(fields %in% colnames(attributes_table)))

        for (field in fields[fields_not_in]) {
            attributes_table[, field] <- NA
        }

        # Find which columns and rows are all NA
        attributes_table <- attributes_table[, fields]
        is_na_col <- apply(attributes_table, 2, function(x) {
            all(is.na(x))
        })

        is_na_row <- apply(attributes_table[, -which(colnames(attributes_table) == "attributeName")], 1, function(x) {
            all(is.na(x))
        })

        # Get all non-NA data
        df_att <- attributes_table[!is_na_row, !is_na_col]

    } else {
        df_att <- NULL
    }

    # Prepare attributes from data
    if (!is.null(data)) {

        if (!is(data, "data.frame")) {
            stop("data must be a data.frame")
        }

        if (!length((colnames(data)))) {
            stop("column names must be populated in data")
        }

        # Initiallize attribute table
        attributeName <- colnames(data)
        r <- length(attributeName)
        att_table <- as.data.frame(matrix(nrow = r, ncol = length(fields)))
        colnames(att_table) <- fields
        att_table[, "attributeName"] <- attributeName

        # Get numberType
        att_table[, "numberType"] <- unlist(lapply(data, function(x) get_numberType(x)))
        dateType <- unlist(lapply(data, function(x) {

            tryCatch(length(as.Date(x)) > 0, error = function(err) {
                FALSE
            })

        }), use.names = FALSE)

        # Get domain
        att_table[, "domain"] <- ifelse(!is.na(att_table$numberType), "numericDomain",
                                        ifelse(dateType, "dateTimeDomain", ""))

        # Get measurementScale
        att_table[, "measurementScale"] <- ifelse(att_table$domain == "dateTimeDomain", "dateTime", "")

        # Update values from attributes_table if given
        for (r in seq(nrow(df_att))) {
            row <- which(att_table$attributeName == df_att$attributeName[r])

            for (c in seq(ncol(df_att))) {
                col <- which(colnames(att_table) == colnames(df_att[c]))
                att_table[row, col] <- df_att[r, c]
            }

        }

    } else {
        att_table <- attributes_table
    }

    # If both data and attributes_table are NULL, initiallize a blank table
    if (is.null(att_table)) {
        att_table <- data.frame(matrix(nrow = 10, ncol = length(fields)), stringsAsFactors = FALSE)
        colnames(att_table) <- fields
    }

    att_table[is.na(att_table)] <- ""
    shiny_attributes_table(att_table, data)
}

#' Get EML numberType
#'
#' returns the EML numberType (either 'real', 'integer', 'whole', or 'natural') of input values
#'
#' @param values  (numeric/character) a vector of values, if vector is non-numeric will return NA
#' @return the numberType of \code{values} (either 'real', 'integer', 'whole', or 'natural').
#' @examples
#' \dontrun{
#' # To get numberType for each column in a data.frame:
#'
#' unlist(lapply(df, function(x) get_numberType(x)))
#' }
#' @export
get_numberType <- function(values) {
    # Unlist data. Use do.call to preserve date format
    if (is.list(values) == T) {
        values <- do.call("c", values)
    }

    numberType <- NA

    if (is.numeric(values)) {

        if (all(is.nan(values))) {
            # If all values are NaN
            numberType <- "real"

        } else if (any(round(values) != values, na.rm = T)) {
            # If any values are a fraction
            numberType <- "real"

        } else if (any(values < 0, na.rm = T)) {
            # if any values are less than 0
            numberType <- "integer"

        } else if (any(values == 0, na.rm = T)) {
            # if any values are == 0
            numberType <- "whole"

        } else {
            numberType <- "natural"
        }

    }

    return(numberType)
}

#' Outputs data.frame to text for shiny app
#'
#' @param df (data.frame)
output_text_func <- function(df) {

    # Initiallize text
    output_text <- c()

    # Get text for each column
    for (c in colnames(df)) {
        values <- "c("

        for (r in 1:length(df[, c])) {

            if (is.na(df[r, c])) {
                values <- paste0(values, "''", ",")

            } else {
                values <- paste0(values, "'", df[r, c], "',")
            }

        }

        # Clean text
        values <- paste0(substr(values, 1, (nchar(values) - 1)), ")")
        output_text[c] <- paste0(c, " = ", values, ",\n")
    }

    cat("\n\ndata.frame(\n", output_text, "stringsAsFactors = FALSE)\n\n")
}


#' Create/Edit EML attributes
#'
#' create/edit EML attributes, custom units, and factors in a dynamic setting
#'
#' @param data (data.frame) the data.frame of data that needs an attribute table
#' @param att_table (data.frame) initial attributes table
shiny_attributes_table <- function(att_table, data) {

    # UI
    ui <- shiny::fluidPage(shinyjs::useShinyjs(),
                           shiny::br(),
                           shiny::tags$button(id = "quit",
                                              type = "button",
                                              class = "btn action-button btn-danger btn-lg",
                                              onclick = "setTimeout(function(){window.close();},100);", "Quit App"),
                           shiny::tags$button(id = "help",
                                              type = "button",
                                              class = "btn action-button btn-info btn-lg",
                                              "Help"),
                           shiny::br(),
                           shiny::br(),
                           shiny::mainPanel(width = 12,
                                            shiny::tabsetPanel(id = "tabs",
                                                               shiny::tabPanel("Attributes Table",
                                                                               shiny::br(),
                                                                               shiny::tags$button(id = "print_att",
                                                                                                  type = "button",
                                                                                                  class = "btn action-button btn-primary",
                                                                                                  "Print Attributes to Console"),
                                                                               shiny::downloadButton("download_att", label = "Download Attributes",
                                                                                                     class = "btn btn-primary"),
                                                                               shiny::br(),
                                                                               shiny::br(),
                                                                               hot_attributes_output("att_table")),
                                                               shiny::tabPanel("Units Table",
                                                                               shiny::br(),
                                                                               shiny::tags$button(id = "print_units",
                                                                                                  type = "button",
                                                                                                  class = "btn action-button btn-primary",
                                                                                                  "Print Custom Units to Console"),
                                                                               shiny::downloadButton("download_units",
                                                                                                     label = "Download Custom Units",
                                                                                                     class = "btn btn-primary"),
                                                                               shiny::br(),
                                                                               shiny::br(),
                                                                               hot_attributes_output("units_table")),
                                                               shiny::tabPanel("Factors Table",
                                                                               shiny::br(),
                                                                               shiny::tags$button(id = "print_factors",
                                                                                                  type = "button",
                                                                                                  class = "btn action-button btn-primary",
                                                                                                  "Print Factors to Console"),
                                                                               shiny::downloadButton("download_factors",
                                                                                                     label = "Download Factors",
                                                                                                     class = "btn btn-primary"),
                                                                               shiny::br(),
                                                                               shiny::br(),
                                                                               hot_attributes_output("factors_table")))))

    # Server
    server <- function(input, output, session) {
        #################### Attributes Table Inputs ####################
        # Intialize units
        first_build_units <- unlist(lapply(att_table$unit, function(x) {

            if (x != "") {
                build_units_table(x)$id

            } else {
                ""
            }
        }))
        att_table$unit <- first_build_units

        # Attributes reactive
        df_att <- reactiveVal(att_table)

        # Disable/Enable download button
        observeEvent(input$att_table, {

            tryCatch({
                out_att()
                shinyjs::enable("download_att")
            }, error = function(err) {
                shinyjs::disable("download_att")
            })

        })

        #################### Units Table Inputs ####################
        # Initiallize units
        df_units <- reactiveVal(build_units_table(att_table$unit))

        # Get new units from att_table (update attributes and units tables)
        observeEvent(input$att_table$changes, {

            df_att(table_to_r(input$att_table))

            changes <- input$att_table$changes

            for(i in seq(length(changes))) {

                if (colnames(df_att())[changes[[i]][[2]] + 1] == "unit") {

                    # Get units
                    new <- build_units_table(changes[[i]][[4]])
                    id <- new$id

                    # Update Attributes Table
                    if (length(id) > 0) {
                        att_out <- df_att()
                        att_out$unit[(changes[[i]][[1]] + 1)] <- id
                        df_att(att_out)
                    }

                    # Update Units Table
                    out <- rbind(df_units(), new, stringsAsFactors = FALSE)
                    out <- out[!duplicated(out$id), ]
                    out <- out[which(out$id %in% df_att()$unit), ]

                    df_units(out)
                }

                # Clean up units when removed by deleting measurementScale
                out <-df_units()
                out <- out[which(out$id %in% df_att()$unit), ]
                df_units(out)
            }

        })

        # Save changes to units_table
        observeEvent(input$units_table$changes, {
            out <- table_to_r(input$units_table)
            out <- out[, !(colnames(out) %in% c("isStandardUnit"))]
            df_units(out)
        })

        EML_units <- EML::get_unitList()$units
        # Disable/Enable download button
        observeEvent(input$units_table, {

            tryCatch({
                out_units()
                shinyjs::enable("download_units")
            }, error = function(err) {
                shinyjs::disable("download_units")
            })

        })

        #################### Factors Inputs ####################
        # Initiallize factors
        df_factors <- reactiveVal(build_factors(att_table, data))

        # Get new factors from att_table
        observeEvent(input$att_table$changes, {

            changes <- input$att_table$changes

            for(i in seq(length(changes))) {

                if (colnames(df_att())[changes[[i]][[2]] + 1] == "domain") {

                    if (!is.null(changes[[i]][[4]]) && changes[[i]][[4]] == "enumeratedDomain") {
                        change <- df_att()[changes[[i]][[1]] + 1, ]
                        new <- build_factors(change, data)
                        out <- rbind(df_factors(), new)
                        df_factors(out)

                    } else if (!is.null(changes[[i]][[3]]) && changes[[i]][[3]] ==
                               "enumeratedDomain" && changes[[i]][[4]] != "enumeratedDomain") {
                        out <- df_factors()[df_factors()$attributeName != df_att()$attributeName[changes[[i]][[1]] + 1], ]
                        df_factors(out)
                    }

                }
            }

        })

        # Save changes to factors_table
        observeEvent(input$factors_table$changes, {
            df_factors(table_to_r(input$factors_table))
        })

        # Disable/Enable download button
        observeEvent(input$factors_table, {

            tryCatch({
                out_factors()
                shinyjs::enable("download_factors")
            }, error = function(err) {
                shinyjs::disable("download_factors")
            })

        })


        #################### Attributes Table Outputs ####################
        # Attributes output table
        out_att <- reactive({
            table_to_r(input$att_table)
        })

        # Attributes hot table
        output$att_table <- render_hot_attributes({
            hot_attributes_table(df_att(), type = "attributes")
        })

        shiny::observeEvent(input$print_att, {
            output_text_func(out_att())
        })

        output$download_att <- shiny::downloadHandler(filename = "Attributes_Table.csv", content = function(file) {
            data <- out_att()
            write.csv(data, file, row.names = FALSE)
        })

        #################### Units Table Outputs ####################
        # Units output table
        out_units <- reactive({
            out <- table_to_r(input$units_table)
            out <- out[!(out$id %in% EML_units$id), ]
            out[, !(colnames(out) %in% c("unit", "isStandardUnit"))]
        })

        # Units hot table
        output$units_table <- render_hot_attributes({
            out <- df_units()
            out$isStandardUnit <- paste((out$id %in% EML_units$id))
            hot_attributes_table(out, type = "units")
        })

        shiny::observeEvent(input$print_units, {
            tryCatch(output_text_func(out_units()), error = function(err) cat("\n\n no custom units!\n\n"))
        })

        output$download_units <- shiny::downloadHandler(filename = "Custom_Units.csv", content = function(file) {
            data <- out_units()
            write.csv(data, file, row.names = FALSE)
        })

        #################### Factors Outputs ####################
        # Factors hot table
        output$factors_table <- render_hot_attributes({
            hot_attributes_table(df_factors(), type = "factors")
        })

        # Factors output table
        out_factors <- reactive({
            out <- table_to_r(input$factors_table)
        })

        shiny::observeEvent(input$print_factors, {
            tryCatch(output_text_func(out_factors()), error = function(err) cat("\n\n no factors!\n\n"))
        })

        output$download_factors <- shiny::downloadHandler(filename = "Factors.csv", content = function(file) {
            data <- out_factors()
            write.csv(data, file, row.names = FALSE)
        })

        #################### Quit ####################
        shiny::observeEvent(input$quit, {

            attributes = out_att()

            units <- tryCatch({
                out_units()
            }, error = function(err) {
                NA
            })

            factors <- tryCatch({
                out_factors()
            }, error = function(err) {
                NA
            })

            out <- list(attributes = attributes, units = units, factors = factors)
            shiny::stopApp(out)
        })

        #################### Help ####################
        shiny::observeEvent(input$help, {

            shiny::showModal(modalDialog(
                size = "l",
                title = "Help",
                shiny::HTML("Use the <b>Attributes Table</b> tab to build attribute information.<br>
                        <font color='#ff6666'>Pink</font> cells indicate an incomplete cell that needs information.<br>
                        <font color='#999999'>Grey</font> cells indicate no information is needed for that cell.<br><br>

                        Units will automatically update to an EML format.<br>
                        To prevent automatic generation of EML format, input unit in quotes (e.g. 'pH').<br><br>

                        Use the <b>Units Table</b> tab to build units information.<br>
                        <font color='#99cc33'>Green</font> cells indicate the id corresponds to an EML standard unit.<br>
                        <font color='#99cc33'>Green</font> cells will not be exported when exporting the <b>Units Table</b>.<br><br>

                        Use the <b>Factors Table</b> tab to build factors information.<br>
                        Factors are needed for attributes with enumeratedDomains.<br>
                        Factor codes will automatically generate for each enumeratedDomain when data is present.<br><br>

                        After quitting the app, the tables will be returned as a list.<br>
                        Additionally, you can use the buttons above each table to export.<br>
                        Either print the table to the R console or download the table to a csv file.<br>")
            ))
        })

    }

    shiny::runApp(shiny::shinyApp(ui, server, options = list(launch.browser = T)))
}

#' Hot table to r
#'
#' Takes a hot table and converts to r data.frame
#'
#' @param table input table
table_to_r <- function(table) {

    # Initiallize
    table_data <- table$data
    table_colnames <- table$colnames
    out <- do.call(rbind, lapply(table_data, rbind))

    # Change NULL to NA in list
    out[sapply(out, is.null)] <- NA

    if (nrow(out) > 1) {
        out <- as.data.frame(apply(out, 2, unlist), stringsAsFactors = FALSE)

    } else {
        out <- unlist(out)
        out <- as.data.frame(t(out), stringsAsFactors = FALSE)
    }

    colnames(out) <- table_colnames
    out
}

#' Build units table
#'
#' Get units and custom units
#'
#' @param units (character) input units
build_units_table <- function(units) {

    # Get custom units
    units_table <- suppressWarnings(as.data.frame(return_eml_units(units, quiet = TRUE), stringsAsFactors = FALSE))

    # Remove escape characters
    units <- gsub("\"|\'", "", units)

    # Clean
    units_table <- units_table[!is.na(units_table$id),]
    units_table <- units_table[units_table$id != "",]
    units_table <- units_table[!duplicated(units_table$id),]
    return(units_table)
}

#' Build factor table
#'
#' Get factors
#'
#' @param att_table (data.frame) input attributes table
#' @param data (data.frame) input data
build_factors <- function(att_table, data) {

    # Get attribute names of enumeratedDomains
    attributeNames <- att_table[att_table$domain == "enumeratedDomain", "attributeName"]

    if (length(attributeNames) == 0) {
        out <- data.frame(matrix(nrow = 0, ncol = 3), stringsAsFactors = F)
        colnames(out) <- c("attributeName", "code", "definition")

        # If data is null or attribute is not in data make one blank row
    } else if (is.null(data) || !all(attributeNames %in% colnames(data))) {
        out <- data.frame(matrix(nrow = length(attributeNames), ncol = 3), stringsAsFactors = F)
        colnames(out) <- c("attributeName", "code", "definition")
        out$attributeName <- attributeNames

        # Else, get all codes
    } else {
        en_data <- data[attributeNames]
        en_data <- lapply(seq_along(en_data), function(i) {
            attributeName <- colnames(en_data)[i]
            code <- unique(en_data[, i])
            definition <- NA
            out <- data.frame(attributeName, code, definition, stringsAsFactors = F)
            colnames(out) <- c("attributeName", "code", "definition")
            out <- out[!is.na(code),]
            out
        })

        out <- do.call(rbind, en_data)
    }

    out
}

