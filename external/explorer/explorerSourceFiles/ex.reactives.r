#This file handles the reactive expressions for data management and statistical operations.


# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file


DatMain <- reactive({ # data load moved to serverhead
  # data is loaded from serverHead.R load call
  dat <- netrevTable
})

DatThirds <- reactive({
  dat <- netrevThirds
})

DatVars <- reactive({
  # create a list of variable names used in the sidebar inputs
  dat <- DatMain()
  datVars <- with(dat, 
    list(SURVEY_YEAR = unique(SURVEY_YEAR),
      SHORTDESCR = factorOrder$shortdescr,
      CATEGORY = unique(CATEGORY),
      FISHAK = unique(FISHAK),
      STAT =  c("Mean", "Total", "Mean (per day)", "Mean (per metric ton)"))
  )
})


Variable <- reactive({
  dat <- DatMain()
  if(input$CategorySelect == "Homeport"){
    variable = factorOrder$port
  } else if(input$CategorySelect == "State"){
    variable = factorOrder$state
  } else if(input$CategorySelect == "Fisheries"){
    variable = factorOrder$fisheries
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
      
      statSwitch <- switch(input$StatSelect,
        "Mean" = "mean",
        "Total" = "sum",
        "Mean (per day)" = "mean per day",
        "Mean (per metric ton)" = "mean per metric ton")

      #subsetting
      datSub <- subset(dat, SURVEY_YEAR %in% input$YearSelect &  
                            SHORTDESCR %in% input$ShortdescrSelect & 
                            CATEGORY %in% input$CategorySelect &
                            VARIABLE %in% input$VariableSelect &
                            FISHAK == input$FishAkSelect &
                            STAT == statSwitch)
      
      # order for plotting
      datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
        levels = factorOrder$shortdescr)
      
      if(input$CategorySelect == "Homeport"){
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
      } else if(input$CategorySelect == "State"){
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
      } else if(input$CategorySelect == "Fisheries"){
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$fisheries)
      }
      
      return(datSub)
      
    } else return()
#   )
})

# create an additional subset for thirds plot...this is where OO would be handy
DatSubThirds <- reactive({
  if(!is.null(DatThirds())){
    
    validate(
      need(input$StatSelect == "Mean", 'This feature currently supports only the "Mean" Summary Stastic')
    )
    
    validate(
      need(length(input$VariableSelect) == 1, "This feature currently supports only one Summary Variable selection"))
    
    
    dat <- DatThirds()
        
    statSwitch <- switch(input$StatSelect,
                         "Mean" = "mean",
                         "Total" = "sum",
                         "Mean (per day)" = "mean per day",
                         "Mean (per metric ton)" = "mean per metric ton")
    
    #subsetting
    datSub <- subset(dat, SURVEY_YEAR %in% input$YearSelect &
                          SHORTDESCR %in% input$ShortdescrSelect &
                          CATEGORY %in% input$CategorySelect &
                          VARIABLE %in% input$VariableSelect &
                          FISHAK == input$FishAkSelect &
                          STAT == statSwitch  
                     )
    
    datSub$THIRDS <- factor(datSub$THIRDS,
                            levels = factorOrder$thirds)
    
    datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                                levels = factorOrder$shortdescr)
    
    if(input$CategorySelect == "Homeport"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
    } else if(input$CategorySelect == "State"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
    } else if(input$CategorySelect == "Fisheries"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$fisheries)
    }
    
    return(datSub)
    
  }
})


# Plotting/staging
PermitPlot <- reactive({
  if(!(is.null(input$YearSelect) | is.null(input$CategorySelect) | 
    is.null(input$VariableSelect) | is.null(input$FishAkSelect) | 
    is.null(input$StatSelect) | is.null(input$ShortdescrSelect))){
    if(!(input$YearSelect[1]=="" | 
      input$CategorySelect[1] == "" | input$StatSelect[1] == "" | 
      input$VariableSelect[1] == "" | input$ShortdescrSelect[1] == "")){      
        x <- TRUE
    } else {
      x <- FALSE
    }
  } else x <- FALSE
  x
})
