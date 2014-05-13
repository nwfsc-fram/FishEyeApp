output$dat.name <- renderUI({
  selectInput("dat.name", "Data:", choices=c("", "Catcher Vessel Cost Data"), selected="")
})

# Inputs variables will be pulled at this stage because we are not preloading data via .r files
output$fishery <- renderUI({
  if(!is.null(dat())){
    selectInput("fishery", "Fisheries:", choices=c("", unique(dat()$FISHERIES)),selected="", multiple=T)
  } 
})

output$years <- renderUI({
  if(!is.null(dat())){
    selectInput("years", "Years:", choices=c("", unique(dat()$SURVEY_YEAR)), selected="", multiple=T)
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
      selectInput("place", "Geographic location:", choices=c("", unique(dat()$HOMEPT)), selected="", multiple=T)
        }else selectInput("place", "Geographic location:", choices=c("", unique(dat()$STATE)), selected="", multiple=T)
        
  }else return()
})


output$length <- renderUI({
  if(!is.null(dat())){
    selectInput("length", "Vessel length class:", choices=c("", unique(dat()$VSSLNGCLASS)), selected="", multiple=T)
  }
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
    radioButtons("plotType", "Plot type:", choices= c("bar", "point"))
  }
})

output$groupMean <- renderUI({
  if(!is.null(dat.cast())){
    checkboxInput("groupMean", "Group mean CI", value=F)
  }
})

output$groupRange <- renderUI({
  if(!is.null(dat.cast())){
    checkboxInput("groupRange", "Group Range", value=F)
  }
})

# Data subsetting action button
output$dataGo <- renderUI({
  if(dataGo()) actionButton("dataGo", "Select Data")
})