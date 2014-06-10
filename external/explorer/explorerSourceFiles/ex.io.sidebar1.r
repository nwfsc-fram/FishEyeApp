#this page handles all of the reactive expressions for the dynamic user interface
#this file is getting a bit long, consider splitting into two parts

output$dat.name <- renderUI({ #data selection. there are two reactives that are dependent on these names in ex.reactives
  selectInput("dat.name", "Measured variable:", choices=c("", "Disaggregated Cost", "Revenue"), selected="")
})

output$subsetChoice <- renderUI({
  if(!is.null(dat())){
    checkboxInput("subsetChoice", "View subsetting options", value= TRUE)
  }
})

# Inputs variables will be pulled at this stage because we are not preloading data via .r files
output$years <- renderUI({
  if(!is.null(dat())){
    selectInput("years", "Years:", choices=c(levels(dat()$SURVEY_YEAR)), selected=c(levels(dat()$SURVEY_YEAR)), multiple=T)
  }
})

output$fishery <- renderUI({
  if(!is.null(dat())){
    selectInput("fishery", "Fisheries:", choices=c(levels(dat()$FISHERIES)),selected=c(levels(dat()$FISHERIES)), multiple=T)
  } 
})

output$placeUnit <- renderUI({
  if(!is.null(dat())){
    selectInput("placeUnit", "Geographic unit", choices=c("Homeport", "State"))
  }
})

output$place <- renderUI({
  if(!is.null(input$placeUnit)){
    if(input$placeUnit == "Homeport"){
      selectInput("place", "Geographic location:", choices=c(unique(dat()$HOMEPT)), selected=c(unique(dat()$HOMEPT)), multiple=TRUE)
        }else selectInput("place", "Geographic location:", choices=c(unique(dat()$STATE)), selected=c(unique(dat()$STATE)), multiple=TRUE)
        
  }else return()
})


output$length <- renderUI({
  if(!is.null(dat())){
    selectInput("length", "Vessel length class:", choices=c(levels(dat()$VSSLNGCLASS)), selected=c(levels(dat()$VSSLNGCLASS)), multiple=TRUE)
  }
})

output$costtyp <- renderUI({
  if(!is.null(dat()) && dat.measure.var()=="DISCOST"){
    selectInput("costtyp", "Cost type:", choices=c(levels(dat()$COSTTYP)), selected=c(levels(dat()$COSTTYP)), multiple=TRUE)
  } else return(NULL)
})


# Data subsetting action button
output$dataGo <- renderUI({
  if(dataGo()) actionButton("dataGo",label="Select Data", icon=icon("filter"))
})

# begin wellPanel2, plot options

output$by.var <- renderUI({
  if(!is.null(dat.sub())){
    selectInput("by.var", "By:", choices=c("Survey year", "Fishery"))
  }
})

output$group.var <- renderUI({
  if(!is.null(dat.sub())){
    selectInput("group.var", "Group/color:", group.choices())
  }
})

output$facet.var <- renderUI({
  if(!is.null(dat.sub())){
    selectInput("facet.var", "Facet:", c(facet.choices()), selected="None")
  }
})

output$stat <- renderUI({
  if(!is.null(dat.sub())){
    selectInput("stat", "Summary Statistic:", choices= c("sum", "mean", "N"))
  }
})

output$plotType <- renderUI({
  if(!is.null(dat.cast())){
    selectInput("plotType", "Plot type:", choices= c("bar", "point", "line"))
  }
})

output$dodge <- renderUI({
  if(!is.null(input$plotType)){
    if(input$plotType=="bar"){
    radioButtons("dodge", "Position:", choices= c("stack", "dodge"))
    } else return()
  }
})

output$groupMean <- renderUI({
  if(!is.null(dat.cast())){
    checkboxInput("groupMean", "Group mean CI", value=F)
  }
})

output$palette <- renderUI({
  if(!is.null(dat.cast())){
    selectInput("palette", "Palette:", choices=c("CB-friendly", "Brewer", "Hipster1"))
  }
})