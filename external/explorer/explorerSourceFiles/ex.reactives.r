#This file handles the reactive expressions for data management and statistical operations.


# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file


DatMain <- reactive({ # data load moved to serverhead
  # data is loaded from serverHead.R load call
  dat <- netrevTable
})


DatVars <- reactive({
  # create a list of variable names used in the sidebar inputs
  dat <- DatMain()
  datVars <- with(dat, 
    list(SURVEY_YEAR = unique(SURVEY_YEAR),
      SHORTDESCR = factorOrder$shortdescr,
      CATEGORY = unique(CATEGORY),
      FISHAK = unique(FISHAK),
      STAT =  unique(STAT)))
})


Variable <- reactive({
  dat <- DatMain()
  if(input$CategorySelect == "Homeport"){
    variable = factorOrder$port
  } else if(input$CategorySelect == "State"){
    variable = factorOrder$state
  } else {
  subByCategory <- dat[dat$CATEGORY == input$CategorySelect,] 
  variable <- unique(subByCategory$VARIABLE)
  }
  variable
  
})


# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the qouted method in dcast
DatSub <- reactive({
#   if(is.null(input$DataButton) || input$DataButton == 0) return()
#   input$ShortdescrSelect
#   isolate(  
    if(!is.null(DatMain())){
      dat <- DatMain()      
      
      #subsetting
      datSub <- subset(dat, SURVEY_YEAR %in% input$YearSelect &  
        SHORTDESCR %in% input$ShortdescrSelect & 
        CATEGORY %in% input$CategorySelect &
        VARIABLE %in% input$VariableSelect &
        FISHAK == input$FishAkSelect &
        STAT == input$StatSelect)
      
      datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
        levels = factorOrder$shortdescr)
      
      if(input$CategorySelect == "Homeport"){
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
      } else if(input$CategorySelect == "State"){
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
      }
      
      
      return(datSub)
      
    } else return()
#   )
})


# Plotting/staging
PermitPlot <- reactive({
  if(!(is.null(input$YearSelect) | is.null(input$CategorySelect) | 
    is.null(input$VariableSelect) | is.null(input$FishAkSelect) | 
    is.null(input$StatSelect))){
    if(!(input$YearSelect[1]=="" | 
      input$CategorySelect[1] == "" | input$StatSelect[1] == "" | 
      input$VariableSelect[1] == "")){      
        x <- TRUE
    } else {
      x <- FALSE
    }
  } else x <- FALSE
  x
})
