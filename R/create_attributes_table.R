#' Returns the numberType (either "real", "integer", "whole", or "natural") of input values
#'
#' @param values A vector of values. If vector is non-numeric will return NA
#' @return The numberType of \code{values} (either "real", "integer", "whole", or "natural").
#' @examples
#' # To get numberType for each column in a data.frame \code{df}:
#' apply(df, 2, function(x) get_numberType(x))
get_numberType <- function(values){
    values <- unlist(values)
    numberType=NA
    if (is.numeric(values)){
        if (all(is.nan(values))){numberType="real"}
        else{
            if (any(round(values)!=values)){numberType="real"}
            else{
                if (any(values<0,na.rm = T)){numberType="integer"}
                else{
                    if (any(values==0,na.rm = T)){numberType="whole"}
                    else{
                        numberType="natural"
                    }
                }
            }
        }
    }
    return(numberType)
}

shiny_AttributeTable <- function(att_table){
    ### Libraries
    require(shiny)
    require(rhandsontable)
    ### UI
    ui <- fluidPage(
        h3("Edit attribute table as needed, then print attribute table code to console."),
        rHandsontableOutput("table"),
        actionButton("print", "Print Code")
    )
    ### Server
    server <- function(input, output) {
        # Attribute Table Reactive
        DF_R = reactive({
            if (is.null(input$table)) {
                att_table}else{
                    hot_to_r(input$table)
                }
        })
        # Attribute Table Interface
        output$table=renderRHandsontable({
            rhandsontable(DF_R())%>%
                hot_cols(renderer = "
                         function (instance, td, row, col, prop, value, cellProperties) {
                         Handsontable.renderers.NumericRenderer.apply(this, arguments);
                         if (row%2 ==0) {
                         td.style.background = '#e8e8e8';
                         }
                         }")
        })
        # Output from print command
        observeEvent(input$print, {
            DF_out <- DF_R()
            DF_out$domain <- as.character(DF_out$domain)
            DF_out[DF_out == ""] = NA
            DF_out$definition = ifelse(DF_out$definition =="",DF_out$attributeDefinition,DF_out$definition)
            output_text <- c()
            for(c in colnames(DF_out)){
                output_text[c] <- paste0(c," = ",DF_out[c],",\n")}
            output_text[length(output_text)] <- sub("[,]\\n$","",output_text[length(output_text)])
            # ---
            cat("\n\ndata.frame(\n",
                output_text,
                ")\n\n")
        })
    }
    shinyApp(ui, server)
}

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
    ### Libraries
    require(dplyr)
    # Get column names
    if(!length((colnames(df)))){
        stop("column names be populated")
    }
    col_names <- colnames(df)
    n <- length(col_names)
    # Get numberType
    numberType_levels <- c("real","natural","whole","integer")
    if (is.null(numberType)){
        numberType <- paste0(apply(df, 2, function(x) get_numberType(x)))}
    if (length(numberType)!=n){
        stop("numberType is not the same length as input data frame")
    }
    numberType <- factor(numberType,levels=numberType_levels)
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
    # Get measurementScale
    measurementScale_levels <- c("nominal","ordinal","dateTime", "ratio","interval")
    if (is.null(measurementScale)){
        measurementScale <- rep("", n)}
    if (length(measurementScale)!=n){
        stop("measurementScale is not the same length as input data frame")
    }
    measurementScale <- factor(measurementScale,levels=measurementScale_levels)
    # Get domain
    domain_levels <- c("numericDomain","textDomain","enumeratedDomain","dateTimeDomain")
    if (is.null(domain)){
        domain <- ifelse(is.na(numberType),"","numericDomain")}
    if (length(domain)!=n){
        stop("domain is not the same length as input data frame")
    }
    domain <- factor(domain,levels=domain_levels)
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

    att_table <- data.frame(attributeName = col_names,
                            attributeDefinition = attributeDefinition,
                            unit = unit,
                            measurementScale = measurementScale,
                            domain = domain,
                            formatString = formatString,
                            definition = definition,
                            numberType = numberType,
                            missingValueCode = rep("", n),
                            missingValueCodeExplanation = rep("", n),
                            stringsAsFactors = F)
    shiny_AttributeTable(att_table)
}