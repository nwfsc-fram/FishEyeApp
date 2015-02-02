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
      SHORTDESCR = unique(SHORTDESCR),
      CATEGORY = unique(CATEGORY),
      FISHAK = unique(FISHAK),
      STAT =  unique(STAT)))
  return(datVars)
})

Variable <- reactive({
  dat <- DatMain()
  subByCategory <- dat[dat$CATEGORY == input$CategorySelect,] 
  Variable <- unique(subByCategory$VARIABLE)  
  return(Variable)
})

# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the qouted method in dcast
DatSub <- reactive({
  if(!is.null(input$dataButton) && input$dataButton > 0)
  isolate(
    if(PermitPlot()){
      
      dat <- DatMain()

      #subsetting
      datSub <- subset(dat, SURVEY_YEAR %in% input$years & variable %in% input$dat.name & category %in% input$topics)
      
      # droping the "topic" var, I don't think we actually need it, ever
      datSub <- dat.sub[!names(dat.sub) %in% "topic"]
      return(dat.sub)
      
    } else return()
  )
})

# Plotting/staging
PermitPlot <- reactive({
  if(!(is.null(input$YearSelect) | is.null(input$ShortdescrSelect) | 
    is.null(input$CategorySelect) | is.null(input$VariableSelect) | 
    is.null(input$FishAkSelect) | is.null(input$StatSelect))){
    if(!(input$YearSelect[1]=="" | input$ShortdescrSelect[1] == "" | 
      input$CategorySelect[1] == "" | input$StatSelect[1] == "" | 
      input$VariableSelect[1] == "")){      
      x <- TRUE
    } else {
      x <- FALSE
    }
  } else x <- FALSE
  x
})
