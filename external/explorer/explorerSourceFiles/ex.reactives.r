#This file handles the reactive expressions for data management and statistical operations.

# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file

dat <- reactive({ # data load moved to serverhead
  input$dataButton
  isolate(
    if(permitPlot()){
    
    if (input$stat == "Total") {
      dat <- netrevTabsSum
    } else {
      dat <- netrevTabsMean
    }
      
       dat[,1] <- ordered(dat[,1], labels = c("Revenue", "MTS", "DAS", "Total cost net revenue", "Fixed cost", "Variable cost", "Variable cost net revenue"))        
      return(dat)
      
    } else return()
  )
})

dat.vars <- reactive({
  load("data/dat.vars.RData")
#   print("datvars:")
#   print(str(dat.vars))
  dat.vars
})

# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the qouted method in dcast
dat.sub <- reactive({
  if(!is.null(input$dataButton) && input$dataButton > 0)
  isolate(
    if(permitPlot()){
      
      print(input$dataButton)
      
      dat <- dat()

      #subsetting
      dat.sub <- subset(dat, SURVEY_YEAR %in% input$years & variable %in% input$dat.name & category %in% input$topics)
      #re-ordering values
#       dat.sub[,5] <- reorder(dat.sub[,5], dat.sub[,3])
      
      # droping the "topic" var, I don't think we actually need it, ever
      dat.sub <- dat.sub[!names(dat.sub) %in% "topic"]
      return(dat.sub)
      
    } else return()
  )
})

# Plotting/staging
permitPlot <- reactive({
  if(!(is.null(input$years) | is.null(input$dat.name) | is.null(input$topicSelect) | is.null(input$stat) | is.null(input$topics))){
    if(!(input$years[1]=="" | input$dat.name[1] == "" | input$topicSelect[1] == "" | input$stat[1] == "" | input$topics[1] == "")){
      x <- TRUE
    } else {
      x <- FALSE
    }
  } else x <- FALSE
  x
})
