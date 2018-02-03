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
#' @param df A data frame of values or eml attributeList
#' @param attributeDefinition (optional) A vector of attributeDefinitions
#' @param unit (optional) A vector of units
#' @param measurementScale (optional) A vector of measurementScales
#' @param domain (optional) A vector of domains
#' @param formatString (optional) A vector of formatStrings
#' @param definition (optional) A vector of definitions
#' @param numberType (optional) A vector of numberTypes
#' @param missingValueCode (optional) A vector of missingValueCodes
#' @param missingValueCodeExplanation (optional) A vector of missingValueCodeExplanations
#'
#' @examples
#' df <- read.csv("Test.csv")
#' create_attributes_table(df)
#'
#' df <- eml@@dataset@@dataTable[[1]]@@attributeList
#' create_attributes_table(df)
#' @export
create_attributes_table <- function(df,
                                    attributeDefinition = NULL,
                                    unit = NULL,
                                    measurementScale = NULL,
                                    domain = NULL,
                                    formatString = NULL,
                                    definition = NULL,
                                    numberType = NULL,
                                    missingValueCode = NULL,
                                    missingValueCodeExplanation = NULL){

    # Libraries
    require(dplyr)
    attributeName <- NULL

    # Initialize attributes if input is attribute table
    if (is(df,"attributeList")) {
        att_table <- EML::get_attributes(df)$attributes
        colnames_input <- colnames(att_table)
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
        for(c in colnames_att_table) {
            if (c %in% colnames_input) {
                var <- att_table[,c]
                assign(c,var)}
        }
        df = NULL
    }

    # Get attributeName
    if (is.null(attributeName)) {

        if(!length((colnames(df)))) {
            stop("column names must be populated")
        }

        attributeName <- colnames(df)

    }
    n <- length(attributeName)

    # Get numberType
    numberType_levels <- c("real","natural","whole","integer","")
    if (is.null(numberType)) {
        numberType <- unlist(lapply(df, function(x) get_numberType(x)))}
    if (length(numberType)!=n) {
        stop("numberType is not the same length as input data frame")
    }
    numberType <- factor(numberType,levels=numberType_levels)
    names(numberType) <- NULL

    # Get attributeDefinition
    if (is.null(attributeDefinition)){
        attributeDefinition <- rep("", n)}
    if (length(attributeDefinition)!=n){
        stop("attributeDefinition is not the same length as input data frame")
    }
    # Get unit
    if (is.null(unit)){
        unit <- rep("", n)}
    if (length(unit)!=n){
        stop("unit is not the same length as input data frame")
    }

    # Get domain
    is_Date_func <- function(values) {
        tryCatch(length(as.Date(values))>0,
                 error = function(err) {FALSE})}
    dateType <- unlist(lapply(df,function(x) is_Date_func(x)))
    names(dateType) <- NULL
    domain_levels <- c("numericDomain","textDomain","enumeratedDomain","dateTimeDomain","")
    if (is.null(domain)) {
        domain <- ifelse(!is.na(numberType),"numericDomain",
                         ifelse(dateType,"dateTimeDomain",""))}
    if (length(domain)!=n) {
        stop("domain is not the same length as input data frame")
    }
    domain <- factor(domain,levels=domain_levels)

    # Get measurementScale
    measurementScale_levels <- c("nominal","ordinal","dateTime", "ratio","interval","")
    if (is.null(measurementScale)) {
        measurementScale <- ifelse(domain=="dateTimeDomain","dateTime","")}
    if (length(measurementScale)!=n) {
        stop("measurementScale is not the same length as input data frame")
    }
    measurementScale <- factor(measurementScale,levels=measurementScale_levels)

    # Get formatString
    if (is.null(formatString)) {
        formatString <- rep("", n)}
    if (length(formatString)!=n) {
        stop("formatString is not the same length as input data frame")
    }

    # Get definition
    if (is.null(definition)) {
        definition <- rep("", n)}
    if (length(definition)!=n) {
        stop("definition is not the same length as input data frame")
    }

    # Get missingValueCode
    if (is.null(missingValueCode)) {
        missingValueCode <- rep("", n)}
    if (length(missingValueCode)!=n) {
        stop("missingValueCode is not the same length as input data frame")
    }

    # Get missingValueCodeExplanation
    if (is.null(missingValueCodeExplanation)) {
        missingValueCodeExplanation <- rep("", n)}
    if (length(missingValueCodeExplanation)!=n) {
        stop("missingValueCodeExplanation is not the same length as input data frame")
    }

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
    shiny_attributes_table(att_table, df)
}

#' Build shiny UI for editing attributes table within function create_attributes_table()
#'
#' @param att_table an attribute table built from create_attributes_table()
#'
shiny_attributes_table <- function(att_table, df){

    require(shiny)
    require(EML)
    require(dplyr)
    require(rhandsontable)

    cur_version = packageVersion("rhandsontable")
    if(cur_version < '0.3.6'){
        stop("Update rhandsontable package")}

    # Get standardUnits
    unitList <- get_unitList()
    standardUnits <- unitList$units

    # Build Custom Units Table
    build_factors <- function(inputdf, inputdf2) {

        attributeName <- inputdf[inputdf$domain == "enumeratedDomain", "attributeName"]
        inputdf2 <- inputdf2[inputdf2$attributeName %in% attributeName,]
        attributeName <- attributeName[!(attributeName %in% inputdf2$attributeName)]

        if(length(attributeName) == 0) {

            new_factor <- data.frame(matrix(ncol=3,nrow=0))
            colnames(new_factor) <- c("attributeName", "code", "definition")

        } else {

        code <- unlist(unique(df[attributeName]), use.name = F)
        n <- length(code)
        attributeName <- rep(paste0(attributeName),n)
        definition <- rep("",n)
        new_factor <- data.frame(attributeName, code, definition ,stringsAsFactors = F)
        }

        factors_table <- rbind(new_factor, inputdf2)
        return(factors_table)
    }

    build_custom_units <- function(inputdf,standardUnits,inputdf2) {

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

        unq_unitType <- unique(c(standardUnits$unitType,""))
        unq_unitType <- unq_unitType[order(unq_unitType)]
        units_table$unitType <- factor(units_table$unitType, levels = unq_unitType, ordered=T)

        unq_parentSI <- unique(c(standardUnits$parentSI,""))
        unq_parentSI <- unq_parentSI[order(unq_parentSI)]
        units_table$parentSI <- factor(units_table$parentSI, levels = unq_parentSI, ordered=T)

        return(units_table)
    }

    # UI
    ui <- fluidPage(
        h3("Create attribute table and custom units tables"),
        br(),
        actionButton("print_att", "Print Attribute Table"),
        h5("Edit attribute table as needed, then print attribute table code to console.
           Red boxes must be completed for complete metadata.
           Grey boxes should be cleared for complete metadata."),
        rHandsontableOutput("att_table"),
        br(),
        br(),
        actionButton("print_units", "Print Custom Units Table"),
        h5("Edit custom unit table table as needed, then print table code to console."),
        rHandsontableOutput("custom_unit_table"),
        br(),
        br(),
        actionButton("print_factors", "Print Factors Table"),
        h5("Edit factors table for enumerated domains as needed, then print table code to console."),
        rHandsontableOutput("factors_table")
        )

    # Server
    server <- function(input, output) {

        # Attribute Table Reactive
        DF_att = reactive({
            if (is.null(input$att_table)) {
                att_table}else{
                    hot_to_r(input$att_table)
                }
        })

        DF_custom_units = reactive( {
            if (is.null(input$custom_unit_table)) {
                custom_unit_table <- data.frame(matrix(ncol=3,nrow=0))
                colnames(custom_unit_table) <- c("id", "unitType", "parentSI")
                custom_unit_table
            } else {
                build_custom_units(DF_att(),standardUnits,hot_to_r(input$custom_unit_table))}
                    } )

        DF_factors = reactive( {
            if (is.null(input$factors_table)) {
                factors_table <- data.frame(matrix(ncol=3,nrow=0))
                colnames(factors_table) <- c("attributeName", "code", "definition")
                factors_table
            } else {
                build_factors(DF_att(),hot_to_r(input$factors_table))}
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
                    }
}")%>%
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
                    }")
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
                        width = '250px')%>%
                hot_col(col = "parentSI",
                        width = '200px')
        })
        output$factors_table=renderRHandsontable({
            rhandsontable(DF_factors())%>%
                hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
                hot_col(col = "attributeName",
                        readOnly = TRUE,
                        renderer = "
                        function (instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        td.style.fontWeight = 'bold'}")%>%
                hot_col(col = "code",
                        readOnly = TRUE)%>%
                hot_col(col = "definition",
                        strict = FALSE,
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if(!isNaN(value)){
                        td.style.background = 'pink';
                        }}")
        })



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
            output_text[length(output_text)] <- sub("[,]\\n$","",output_text[length(output_text)])

            cat("\n\ndata.frame(\n",
                output_text,
                ")\n\n")}

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
                    }

    shinyApp(ui, server,options = list(launch.browser=T))
}