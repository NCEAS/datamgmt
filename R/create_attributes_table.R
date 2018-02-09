#' Returns the numberType (either "real", "integer", "whole", or "natural") of input values
#'
#' @param values A vector of values. If vector is non-numeric will return NA
#' @return The numberType of \code{values} (either "real", "integer", "whole", or "natural").
#' @examples
#' # To get numberType for each column in a data.frame \code{df}:
#' unlist(lapply(df, function(x) get_numberType(x)))
#' @export
get_numberType <- function(values) {

    #Unlist data. Use do.call to preserve date format
    if (is.list(values)==T) {
        values <- do.call("c",values)}

    numberType=NA

    if (is.numeric(values)) {
        if (all(is.nan(values))) { # If all values are NaN
            numberType="real"
        } else if (any(round(values)!=values,na.rm=T)) { # If any values are a fraction
            numberType="real"
        } else if (any(values<0,na.rm = T)) { # if any values are less than 0
            numberType="integer"
        } else if (any(values==0,na.rm = T)) { # if any values are == 0
            numberType="whole"
        } else {
            numberType="natural"
        }
    }
    return(numberType)
}

#' Allows editing of an attribute table and custom units table in a shiny environment
#'
#' @param data The data.frame of data that needs an attribute table
#' @param attributes_table A existing attributes table for \code{data} that needs to be updated. If specified, all non empty fields will be used (i.e. if numberType is specified in \code{attributes_table}, then this function will use those values instead of automatically generating values from \code{data}).
#'
#' @examples
#' create_attributes_table(NULL, NULL)
#'
#' data <- read.csv("Test.csv")
#' create_attributes_table(data, NULL)
#'
#'attributes_table <- EML::get_attributes(eml@@dataset@@dataTable[[i]]@@attributeList)$attributes
#' create_attributes_table(NULL, attributes_table)
#'
#' create_attributes_table(data, attributes_table)
#' @export
create_attributes_table <- function(data = NULL, attributes_table = NULL){

    if(!is.null(data)){
        stopifnot(is(data,"data.frame"))
        if(!length((colnames(data)))) {
            stop("column names must be populated in data if no attributes_table is entered")
        }
    }

    if(!is.null(attributes_table)){
        stopifnot(is(attributes_table,"data.frame"))
    }

    colnames_att_table <- c("attributeName",
                            "attributeDefinition",
                            "unit",
                            "measurementScale",
                            "domain",
                            "formatString",
                            "definition",
                            "numberType",
                            "missingValueCode",
                            "missingValueCodeExplanation")

    # Initialize attributes with attribute_table
    colnames_input <- colnames(attributes_table)

    if(!("attributeName" %in% colnames_input) && !is.null(attributes_table)){
        stop("attribute_table must have a column 'attributeName'")
    }

    for(c in colnames_att_table) {
        if (c %in% colnames_input) {
            var <- attributes_table[,c]
            assign(c,var)
        } else {
            assign(c,NULL)}
    }


    # Get attributeName
    if(is.null(data) & is.null(attributes_table)){
        attributeName <- ""
    }

    if (!is.null(attributeName)){
        data_colnames <- colnames(data)
        data_in_att <- (data_colnames %in% attributeName)
        if(!all(data_in_att)){
            stop("Data and attributes_table must have the same variables. The following are in data and not in attributes_table:\n",
                 paste(data_colnames[!data_in_att],collapse=", "))
        }
        data <- data[,attributeName]
    } else {
        attributeName <- colnames(data)
    }

    n <- length(attributeName)

    # Get numberType
    if (is.null(numberType) || all(is.na(numberType))) {
        if (!is.null(data)){
            numberType <- unlist(lapply(data, function(x) get_numberType(x)))
        } else {
            numberType <- rep("", n)}
    }

    # Get domain
    is_Date_func <- function(values) {
        tryCatch(length(as.Date(values))>0,
                 error = function(err) {FALSE})}

    if (is.null(domain) || all(is.na(domain))) {
        if (!is.null(data)){
            dateType <- unlist(lapply(data,function(x) is_Date_func(x)),use.names = FALSE)
            domain <- ifelse(!is.na(numberType),"numericDomain",
                             ifelse(dateType,"dateTimeDomain",""))
        } else {
            domain <- rep("", n)
        }
    }

    # Get measurementScale
    if (is.null(measurementScale) || all(is.na(measurementScale))) {
        measurementScale <- ifelse(domain=="dateTimeDomain","dateTime","")
    }

    # Get attributeDefinition
    if (is.null(attributeDefinition)){
        attributeDefinition <- rep("", n)}

    # Get unit
    if (is.null(unit)){
        unit <- rep("", n)}

    # Get formatString
    if (is.null(formatString)) {
        formatString <- rep("", n)}

    # Get definition
    if (is.null(definition)) {
        definition <- rep("", n)}

    # Get missingValueCode
    if (is.null(missingValueCode)) {
        missingValueCode <- rep("", n)}

    # Get missingValueCodeExplanation
    if (is.null(missingValueCodeExplanation)) {
        missingValueCodeExplanation <- rep("", n)}

    att_table <- data.frame(attributeName = attributeName,
                            domain = domain,
                            attributeDefinition = attributeDefinition,
                            definition = definition,
                            measurementScale = measurementScale,
                            formatString = formatString,
                            numberType = numberType,
                            unit = unit,
                            missingValueCode = missingValueCode,
                            missingValueCodeExplanation = missingValueCodeExplanation,
                            stringsAsFactors = F)

    att_table[is.na(att_table)] = ""

    shiny_attributes_table(att_table, data)
}

#' Build shiny custom units table
#'
#' @param inputdf attributes table
#' @param standardUnits standardUnits table
#' @param inputdf2 current units table
#' @param unq_unitType unique standardUnits unitTypes
#' @param unq_parentSI unique standardUnits parentSI
build_custom_units <- function(inputdf, standardUnits, inputdf2, unq_unitType, unq_parentSI) {

    old_units <- inputdf2$id
    new_units <- inputdf$unit
    inputdf2 <- inputdf2[inputdf2$id %in% new_units,]

    # select unique/non standard units
    new_units <- new_units[!(new_units %in% old_units)]
    new_units <- new_units[!(new_units %in% standardUnits$id)]
    new_units <- new_units[!duplicated(new_units)]
    new_units <- new_units[new_units != ""]
    new_units <- new_units[!is.na(new_units)]

    if(length(new_units) == 0) {
        new_units <- data.frame(matrix(ncol=3, nrow=0))
        colnames(new_units) <- c("id","unitType","parentSI")

    } else {
        new_units <- as.data.frame(new_units)
        colnames(new_units) <- "id"
        new_units$unitType <- ""
        new_units$parentSI <- ""
    }

    units_table <- rbind(new_units,inputdf2)

    units_table$unitType <- factor(units_table$unitType, levels = unq_unitType, ordered=T)

    units_table$parentSI <- factor(units_table$parentSI, levels = unq_parentSI, ordered=T)

    return(units_table)
}

#' Build shiny factors table
#'
#' @param inputdf attributes table
#' @param inputdf2 current factors table
#' @param data initial data inputed by user
build_factors <- function(inputdf, inputdf2 , data) {

    attributeName <- inputdf[inputdf$domain == "enumeratedDomain", "attributeName"]
    attributeName <- attributeName[!is.na(attributeName)]
    #inputdf2 <- inputdf2[inputdf2$attributeName %in% attributeName,]
    attributeName <- attributeName[!(attributeName %in% inputdf2$attributeName)]

    if(length(attributeName) == 0 || is.null(data)) {

        new_factor <- data.frame(matrix(ncol=3,nrow=0), stringsAsFactors = F)
        colnames(new_factor) <- c("attributeName", "code", "definition")

    } else {

        code <- unlist(unique(data[attributeName]), use.name = F)
        n <- length(code)
        attributeName <- rep(paste0(attributeName),n)
        definition <- rep("",n)
        new_factor <- data.frame(attributeName, code, definition, stringsAsFactors = F)
    }

    factors_table <- rbind(new_factor, inputdf2)
    return(factors_table)
}

#' Creates factors for attributes table
#'
#' @param inputdf attributes table
factor_att_table <- function(inputdf){

    domain_levels <- c("numericDomain","textDomain","enumeratedDomain","dateTimeDomain","")
    inputdf$domain <- factor(inputdf$domain, levels=domain_levels)

    numberType_levels <- c("real","natural","whole","integer","")
    inputdf$numberType <- factor(inputdf$numberType, levels=numberType_levels)

    measurementScale_levels <- c("nominal","ordinal","dateTime", "ratio","interval","")
    inputdf$measurementScale <- factor(inputdf$measurementScale, levels=measurementScale_levels)

    return(inputdf)
}

#' Outputs data.frame to text for shiny app
#'
#' @param df data.frame
output_text_func <- function(df){
    output_text <- c()
    for(c in colnames(df)){
        values <- "c("
        for(r in 1:length(df[,c])){
            if (is.na(df[r,c])){
                values <- paste0(values,df[r,c],",")}else{
                    values <- paste0(values,"'",df[r,c],"',")}
        }
        values <- paste0(substr(values,1,(nchar(values)-1)),")")
        output_text[c] <- paste0(c," = ",values,",\n")}

    cat("\n\ndata.frame(\n",
        output_text,
        "stringsAsFactors = FALSE)\n\n")}

#' Build shiny UI for editing attributes table within function create_attributes_table()
#'
#' @param att_table an attribute table built from create_attributes_table()
shiny_attributes_table <- function(att_table, data){

    require(shiny)
    require(EML)
    require(rhandsontable)

    cur_version = packageVersion("rhandsontable")
    if(cur_version < '0.3.6'){
        stop("Update rhandsontable package")}

    # Get standardUnits
    unitList <- get_unitList()
    standardUnits <- unitList$units

    unq_unitType <- unique(c(standardUnits$unitType,""))
    unq_unitType <- unq_unitType[order(unq_unitType)]

    unq_parentSI <- unique(c(standardUnits$parentSI,""))
    unq_parentSI <- unq_parentSI[order(unq_parentSI)]

    # UI
    ui <- fluidPage(
        br(),
        tags$button(
            id = "quit",
            type = "button",
            class = "btn action-button btn-danger btn-lg",
            onclick = "setTimeout(function(){window.close();},100);",
            "Quit App"
        ),
        h3("Create attribute table and custom units tables"),
        br(),
        actionButton("print_att", "Print Attribute Table"),
        h5("Edit attribute table as needed, then print attribute table code to console.
           Red boxes must be completed for complete metadata.
           Grey boxes should be cleared for complete metadata."),
        rHandsontableOutput("att_table"),
        br(),
        br(),

        fluidRow(

            column(6,
                   actionButton("print_units", "Print Custom Units Table")
            ),

            column(6,
                   actionButton("print_factors", "Print Factors Table")
            )
        ),

        fluidRow(

            column(6,
                   h5("Edit custom unit table table as needed, then print table code to console.")
            ),

            column(6,
                   h5("Edit factors table for enumerated domains as needed, then print table code to console.")
            )
        ),

        fluidRow(

            column(6,
                   rHandsontableOutput("custom_unit_table", height = 500)
            ),

            column(6,
                   rHandsontableOutput("factors_table")
            )
        )
    )

    # Server
    server <- function(input, output) {

        # Attribute Table Reactive
        DF_att = reactive({
            if (is.null(input$att_table)) {
                factor_att_table(att_table)
                } else {
                    factor_att_table(hot_to_r(input$att_table))
                }
        })

        DF_custom_units = reactive( {
            if (is.null(input$custom_unit_table)) {
                custom_unit_table <- data.frame(matrix(ncol = 3, nrow = 0))
                colnames(custom_unit_table) <- c("id", "unitType", "parentSI")
                custom_unit_table
            } else {
                build_custom_units(DF_att(), standardUnits,hot_to_r(input$custom_unit_table), unq_unitType, unq_parentSI)
            }
        } )

        DF_factors = reactive( {
            if (is.null(input$factors_table)) {
                factors_table <- data.frame(matrix(ncol = 3, nrow = 0))
                colnames(factors_table) <- c("attributeName", "code", "definition")
                factors_table
            } else {
                build_factors(DF_att(), hot_to_r(input$factors_table), data)}
        } )

        output$att_table=renderRHandsontable({
            rhandsontable(DF_att())%>%
                hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%

                hot_col(col = "attributeName",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if(!isNaN(value)){
                        td.style.background = 'pink';
                        }}")%>%

                hot_col(col = "domain",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments)
                    if(!isNaN(value)){
                    td.style.background = 'pink';
                    }}")%>%

                hot_col(col = "attributeDefinition",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if(!isNaN(value)){
                    td.style.background = 'pink';
                    }}")%>%

                hot_col(col = "definition",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (instance.getData()[row][1] == 'textDomain' & !isNaN(value)) {
                    td.style.background = 'pink';
                    }
                    else if (instance.getData()[row][1] != 'textDomain' & isNaN(value)){
                    td.style.background = 'grey';
                    }
                    }")%>%

                hot_col(col = "measurementScale",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if(!isNaN(value)){
                    td.style.background = 'pink';
                    }}")%>%

                hot_col(col = "formatString",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (instance.getData()[row][1] == 'dateTimeDomain' & !isNaN(value)) {
                    td.style.background = 'pink';
                    }
                    else if (instance.getData()[row][1] != 'dateTimeDomain' & isNaN(value)){
                    td.style.background = 'grey';
                    }}")%>%

                hot_col(col = "numberType",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (instance.getData()[row][1] == 'numericDomain' & !isNaN(value)) {
                    td.style.background = 'pink';
                    }
                    else if (instance.getData()[row][1] != 'numericDomain' & isNaN(value)){
                    td.style.background = 'grey';
                    }
                    }")%>%

                hot_col(col = "unit",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (instance.getData()[row][1] == 'numericDomain' & !isNaN(value)) {
                    td.style.background = 'pink';
                    }
                    else if (instance.getData()[row][1] != 'numericDomain' & isNaN(value)){
                    td.style.background = 'grey';
                    }
                    }")%>%

                hot_col(col = "missingValueCodeExplanation",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (isNaN(instance.getData()[row][8]) & !isNaN(value)) {
                    td.style.background = 'pink';
                    }
                    else if (!isNaN(instance.getData()[row][8]) & isNaN(value)){
                    td.style.background = 'grey';
                    }
                    }")%>%
                hot_cols(allowInvalid = T)
        })

        output$custom_unit_table=renderRHandsontable({
            rhandsontable(DF_custom_units())%>%

                hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%

                hot_col(col = "id",
                        readOnly = TRUE,
                        renderer = "
                        function (instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        td.style.fontWeight = 'bold'}")%>%

                hot_col(col = "unitType",
                        type = "dropdown",
                        width = '250px')%>%

                hot_col(col = "parentSI",
                        width = '200px')
        })

        output$factors_table=renderRHandsontable({
            rhandsontable(DF_factors())%>%

                hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%

                hot_col(col = "attributeName",
                        type = "text",
                        renderer = "
                        function (instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        td.style.fontWeight = 'bold'}")%>%

                hot_col(col = "code",
                        type = "text")%>%

                hot_col(col = "definition",
                        type = "text",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if(!isNaN(value)){
                        td.style.background = 'pink';
                        }}")
        })

        # Output from print command
        observeEvent(input$print_att, {
            DF_out_att <- DF_att()
            DF_out_att$domain <- as.character(DF_out_att$domain)
            DF_out_att$numberType <- as.character(DF_out_att$numberType)
            DF_out_att[DF_out_att == ""] = NA
            output_text_func(DF_out_att)
        })

        observeEvent(input$print_units, {
            DF_out_unit <- DF_custom_units()
            if (nrow(DF_out_unit)>0){
                DF_out_unit$id <- as.character(DF_out_unit$id)
                DF_out_unit$unitType <- as.character(DF_out_unit$unitType)
                DF_out_unit[DF_out_unit == ""] = NA
                output_text_func(DF_out_unit)}else{
                    cat("\n\nNothing to print!\n\n")
                }
        })

        observeEvent(input$print_factors, {
            DF_out_unit <- DF_factors()
            if (nrow(DF_out_unit)>0){
                DF_out_unit[DF_out_unit == ""] = NA
                output_text_func(DF_out_unit)}else{
                    cat("\n\nNothing to print!\n\n")
                }
        })

        observeEvent(input$quit, {
            stopApp()
        })

    }

    shinyApp(ui, server,options = list(launch.browser=T))
}