#' Returns the numberType (either "real", "integer", "whole", or "natural") of input values
#'
#' @param values A vector of values. If vector is non-numeric will return NA
#' @return The numberType of \code{values} (either "real", "integer", "whole", or "natural").
#' @examples
#' # To get numberType for each column in a data.frame \code{df}:
#' unlist(lapply(df, function(x) get_numberType(x)))
get_numberType <- function(values){
    if (is.list(values)==T){
        values <- do.call("c",values)}
    numberType=NA
    if (is.numeric(values)) {
        if (all(is.nan(values))) {
            numberType="real"
        } else {
            if (any(round(values)!=values)) {
                numberType="real"
            } else {
                if (any(values<0,na.rm = T)) {
                    numberType="integer"
                } else {
                    if (any(values==0,na.rm = T)) {
                        numberType="whole"
                    } else {
                        numberType="natural"
                    }
                }
            }
        }
    }
    return(numberType)
}

#' Allows editing of an attribute table and custom units table in a shiny environment
#'
#' @param att_table An attribute table
#' @return Prints attribute table and custom units table to console
edit_attributes_table <- function(att_table){
    ### Libraries
    require(shiny)
    require(rhandsontable)
    require(EML)
    require(dplyr)
    # Get standardUnits
    unitList <- get_unitList()
    standardUnits <- unitList$units
    # table functions
    build_additional_meta_intial <- function(){
        additional_meta <- data.frame(matrix(ncol=3,nrow=0))
        colnames(additional_meta) <- c("id","unitType","parentSI")
        return(additional_meta)
    }
    build_additional_meta <- function(inputdf,standardUnits){
        additional_meta <- select(inputdf,unit)
        additional_meta$unitType <- ""
        additional_meta$parentSI <- ""
        colnames(additional_meta) <- c("id","unitType","parentSI")
        additional_meta <- additional_meta[!(additional_meta$id %in% standardUnits$id),]
        additional_meta <- additional_meta[!duplicated(additional_meta$id),]
        additional_meta <- additional_meta[additional_meta!="",]
        additional_meta <- additional_meta[!is.na(additional_meta$id),]
        unq_unitType <- unique(c(standardUnits$unitType,""))
        unq_unitType <- unq_unitType[order(unq_unitType)]
        additional_meta$unitType <- factor(additional_meta$unitType, levels = unq_unitType, ordered=T)
        unq_parentSI <- unique(c(standardUnits$parentSI,""))
        unq_parentSI <- unq_parentSI[order(unq_parentSI)]
        additional_meta$parentSI <- factor(additional_meta$parentSI, levels = unq_parentSI, ordered=T)
        return(additional_meta)
    }
    ### UI
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
        actionButton("print_meta", "Print Custom Units Table"),
        h5("Edit custom unit table table as needed, then print table code to console."),
        rHandsontableOutput("meta_table", width = "100%", height = 1000)
    )
    ### Server
    server <- function(input, output) {
        # Attribute Table Reactive
        DF_att_full = reactive({
            if (is.null(input$att_table)) {
                att_table}else{
                    hot_to_r(input$att_table)
                }
        })
        DF_att = reactive({
            DF_att_full()
        })
        DF_additional_meta = reactive({
            if (is.null(input$att_table)) {
                build_additional_meta_intial()}else{
                    build_additional_meta(hot_to_r(input$att_table),standardUnits)}
        })
        # Attribute Table Interface
        output$att_table=renderRHandsontable({
            rhandsontable(DF_att())%>%
                hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
                hot_col(col = "attributeDefinition",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if (value === '') {
                        td.style.background = 'pink';
                        }}")%>%
                hot_col(col = "measurementScale",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if (value === '') {
                        td.style.background = 'pink';
                        }}")%>%
                hot_col(col = "domain",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if (value === '') {
                        td.style.background = 'pink';
                        }}")%>%
                hot_col(col = "unit",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if (instance.getData()[row][1] == 'numericDomain' & value == '') {
                        td.style.background = 'pink';
                        }
                        else if (instance.getData()[row][1] != 'numericDomain' & value != ''){
                        td.style.background = 'grey';
                        }
                        }")%>%
                hot_col(col = "numberType",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if (instance.getData()[row][1] == 'numericDomain' & value == '') {
                        td.style.background = 'pink';
                        }
                        else if (instance.getData()[row][1] != 'numericDomain' & value != ''){
                        td.style.background = 'grey';
                        }
                        }")%>%
                hot_col(col = "missingValueCodeExplanation",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if (instance.getData()[row][8] != '' & value == '') {
                        td.style.background = 'pink';
                        }
                        else if (instance.getData()[row][8] == '' & value != ''){
                        td.style.background = 'grey';
                        }
                        }")%>%
                hot_col(col = "formatString",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if (instance.getData()[row][1] == 'dateTimeDomain' & value == '') {
                        td.style.background = 'pink';
                        }
                        else if (instance.getData()[row][1] != 'dateTimeDomain' & value != ''){
                        td.style.background = 'grey';
                        }
                        }")%>%
                hot_col(col = "definition",
                        renderer= "function(instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if (instance.getData()[row][1] == 'textDomain' & value == '') {
                        td.style.background = 'pink';
                        }
                        else if (instance.getData()[row][1] != 'textDomain' & value != ''){
                        td.style.background = 'grey';
                        }
                        }")
        })
        output$meta_table=renderRHandsontable({
            rhandsontable(DF_additional_meta())%>%
                hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
                hot_col(col = "id",
                        readOnly = TRUE,
                        renderer = "
                        function (instance, td, row, col, prop, value, cellProperties) {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        td.style.fontWeight = 'bold'}")%>%
                hot_col(col = "unitType",
                        width = '250px',
                        type = "autocomplete")%>%
                hot_col(col = "parentSI",
                        width = '200px',
                        type = "autocomplete")
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
        observeEvent(input$print_meta, {
            DF_out_unit <- DF_additional_meta()
            if (nrow(DF_out_unit)>0){
                DF_out_unit$id <- as.character(DF_out_unit$id)
                DF_out_unit$unitType <- as.character(DF_out_unit$unitType)
                DF_out_unit[DF_out_unit == ""] = NA
                output_text_func(DF_out_unit)}else{
                    cat("\n\nNothing to print!\n\n")
                }
        })

    }
    shinyApp(ui, server)
}

#' Allows editing of an attribute table and custom units table in a shiny environment
#'
#' @param df A data frame of values
#' @return Prints attribute table and custom units table to console
#' @examples
create_attributes_table <- function(df,
                                    is.attribute.table = F,
                                    attributeDefinition = NULL,
                                    unit = NULL,
                                    measurementScale = NULL,
                                    domain = NULL,
                                    formatString = NULL,
                                    definition = NULL,
                                    numberType = NULL,
                                    missingValueCode = NULL,
                                    missingValueCodeExplanation = NULL){

    ### Libraries
    require(dplyr)
    # Initialize attributes if input is attribute table
    attributeName <- NULL
    if(is.attribute.table==T){
        colnames_input <- colnames(df)
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
        for(c in colnames_att_table){
            if (c %in% colnames_input){
                var <- df[,c]
                assign(c,var)}
        }
    }
    # Get column names
    if(!length((colnames(df)))){
        stop("column names be populated")
    }
    # Get attributeName
    if (is.null(attributeName)){
        attributeName <- colnames(df)
    }
    n <- length(attributeName)
    # Get numberType
    numberType_levels <- c("real","natural","whole","integer","")
    if (is.null(numberType)){
        numberType <- unlist(lapply(df, function(x) get_numberType(x)))}
    if (length(numberType)!=n){
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
    is_Date_func <- function(values){
        tryCatch(length(as.Date(values))>0,
                 error = function(err) {FALSE})}
    dateType <- unlist(lapply(df,function(x) is_Date_func(x)))
    names(dateType) <- NULL
    domain_levels <- c("numericDomain","textDomain","enumeratedDomain","dateTimeDomain","")
    if (is.null(domain)){
        domain <- ifelse(!is.na(numberType),"numericDomain",
                         ifelse(dateType,"dateTimeDomain",""))}
    if (length(domain)!=n){
        stop("domain is not the same length as input data frame")
    }
    domain <- factor(domain,levels=domain_levels)
    # Get measurementScale
    measurementScale_levels <- c("nominal","ordinal","dateTime", "ratio","interval","")
    if (is.null(measurementScale)){
        measurementScale <- ifelse(domain=="dateTimeDomain","dateTime","")}
    if (length(measurementScale)!=n){
        stop("measurementScale is not the same length as input data frame")
    }
    measurementScale <- factor(measurementScale,levels=measurementScale_levels)
    # Get formatString
    if (is.null(formatString)){
        formatString <- rep("", n)}
    if (length(formatString)!=n){
        stop("formatString is not the same length as input data frame")
    }
    # Get definition
    if (is.null(definition)){
        definition <- rep("", n)}
    if (length(definition)!=n){
        stop("definition is not the same length as input data frame")
    }
    # Get missingValueCode
    if (is.null(missingValueCode)){
        missingValueCode <- rep("", n)}
    if (length(missingValueCode)!=n){
        stop("missingValueCode is not the same length as input data frame")
    }
    # Get missingValueCodeExplanation
    if (is.null(missingValueCodeExplanation)){
        missingValueCodeExplanation <- rep("", n)}
    if (length(missingValueCodeExplanation)!=n){
        stop("missingValueCodeExplanation is not the same length as input data frame")
    }
    # Get unitType

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
    edit_attributes_table(att_table)
}
