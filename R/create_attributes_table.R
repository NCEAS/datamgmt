get_numberType <- function(values){
  values <- unlist(values)
  numberType=NA
  if (is.numeric(values)){
    if (any(round(values)!=values)){numberType='real'}
    else{
      if (any(values<0,na.rm = T)){numberType='integer'}
      else{
        if (any(values==0,na.rm = T)){numberType='whole'}
        else{
          numberType='natural'
        }
      }
    }
  }
  return(numberType)
}

shiny_AttributeTable <- function(att_table){
  require(shiny)
  require(rhandsontable)
  # ----------
  #UI
  ui <- fluidPage(
    rHandsontableOutput("table"),
    actionButton("print", "Print Code")
  )
  
  server <- function(input, output) {
    
    DF_R = reactive({
      if (is.null(input$table)) {
        DF = att_table}else{
          DF = hot_to_r(input$table)
        }
    })
    
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
    
    observeEvent(input$print, {
      DF_out <- DF_R()
      DF_out$domain <- as.character(DF_out$domain)
      DF_out[DF_out == ''] = NA
      DF_out$definition = ifelse(DF_out$definition =='',DF_out$attributeDefinition,DF_out$definition)
      output_text <- c()
      for(c in colnames(DF_out)){
        output_text[c] <- paste0(c,' = ',DF_out[c],',\n')}
      output_text[length(output_text)] <- sub('[,]\\n$','',output_text[length(output_text)])
      # ---
      cat('\n\ndata.frame(\n',
          output_text,
          ')\n\n')
    })
    
  }
  
  shinyApp(ui, server)
}

create_attributes_table <- function(df, is.attribute.table = F) {
  require(dplyr)
  
  if (is.attribute.table ==F){
    if(!length((colnames(df)))) {
      stop("column names be populated")
    }
    
    col_names <- colnames(df)
    n <- length(col_names)
    
    if (any(grepl(" ", col_names))) {
      stop(paste0("column names cannot contain whitespace"))
    }
    # ----------
    # Get numberType of dataframe
    numberType <- c()
    for (c in 1:n){
      numberType[c] <- get_numberType(df[,c])
    }
    # ----------
    
    att_table <- data.frame(attributeName = col_names,
                            attributeDefinition = rep('', n),
                            unit = rep('', n),
                            measurementScale = rep('', n),
                            domain = factor(ifelse(is.na(numberType),NA,'numericDomain'),
                                                         levels = c('numericDomain',
                                                                    'textDomain',
                                                                    'enumeratedDomain',
                                                                    'dateTimeDomain')),
                            formatString = rep('', n),
                            definition = factor(rep('A', n),levels = letters[10:1]),
                            numberType = numberType,
                            missingValueCode = rep('', n),
                            missingValueCodeExplanation = rep('', n),
                            stringsAsFactors = F)
  }else
  {att_table=df}
  
  shiny_AttributeTable(att_table)
}



=======
    numberType=NA
    if (is.numeric(values)){
        if (any(values%%1!=0,na.rm = T)){numberType='real'}
        else{
            if (any(values<0,na.rm = T)){numberType='integer'}
            else{
                if (any(values==0,na.rm = T)){numberType='whole'}
                else{
                    numberType='natural'
                }
            }
        }
    }
    return(numberType)
}

create_attributes_table <- function(data, definitions = NULL, missingValueCode = NULL,
                                    missingValueCodeExplanation = NULL) {
    if(!length((colnames(data)))) {
        stop("column names be populated")
    }

    col_names <- colnames(data)
    n <- length(col_names)

    if (any(grepl(" ", col_names))) {
        stop(paste0("column names cannot contain whitespace"))
    }
    if (!(is.null(definitions))) {
        if (length(definitions) != n) {
            stop(paste0("definitions has length = ", length(definitions), ", rather than ", n))
        }
    } else {
        definitions = rep(NA, n)
    }
    if (!(is.null(missingValueCode))) {
        stopifnot(is.character(missingValueCode))
    }
    if (!(is.null(missingValueCodeExplanation))) {
        stopifnot(is.character(missingValueCodeExplanation))
    }

    table <- data.frame(attributeName = col_names,
                        attributeDefinition = definitions,
                        measurementScale = rep("NA", n),
                        domain = rep("NA", n),
                        formatString = rep("NA", n),
                        definition = definitions,
                        unit = rep("NA", n),
                        numberType = rep("NA", n),
                        missingValueCode = rep("NA", n),
                        missingValueCodeExplanation = rep("NA", n),
                        stringsAsFactors = F)

    for (i in seq_len(n)) {

        if (is.na(table$attributeDefinition[i])) {
            table$attributeDefinition[i] <- readline(prompt = paste0("Enter attribute definition for ",
                                                                     table$attributeName[i],": \n"))
            table$definition[i] <- table$attributeDefinition[i]
        }

        measurementScale1 <- readline(prompt = paste0("Enter measurement scale for ", table$attributeName[i],
                                                      ". Choose one of: \n dateTime: 'd' \n interval: 'i' \n nominal: 'n' \n ordinal: 'o' \n ratio: 'r' \n"))

        switch(measurementScale1,
               "d" = {
                   table$measurementScale[i] = "dateTimeDomain"
               },
               "i" = {
                   table$measurementScale[i] = "interval"
               },
               "n" = {
                   table$measurementScale[i] = "nominal"
               },
               "o" = {
                   table$measurementScale[i] = "ordinal"
               },
               "r" = {
                   table$measurementScale[i] = "ratio"
               })

        if (table$measurementScale[i] %in% c("nominal", "ordinal")) {
            table$domain[i] = "textDomain"
        } else if (table$measurementScale[i] %in% c("ratio", "interval")) {
            table$domain[i] = "numericDomain"
        } else {
            table$domain[i] = "dateTimeDomain"
        }

        if (table$domain[i] == "dateTimeDomain") {
            table$formatString[i] <- readline(prompt = paste0("Enter string format for ", table$attributeName[i],
                                                              ": \nExample: 'MM/DD/YYYY'"))
        } else if (table$domain[i] == "numericDomain") {
            table$unit[i] <- readline(prompt = paste0("Enter units of measurement for ", table$attributeName[i],
                                                      ": \n"))
            table$numberType[i] <- get_numberType(data[,i])
        } else {
            table$unit[i] = NA
            table$numberType[i] = NA
        }

    }

    message("If any nonimal or ordinal variables are 'enumeratedDomain', these were incorrectly set to 'textDomain'. Please update these fields by hand.")

    return(table)

}