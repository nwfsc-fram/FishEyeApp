output$dat.name <- renderUI({
  selectInput("dat.name", "Data:", choices=c("", "Catcher Vessel Cost Data"), selected="")
})

# Inputs variables will be pulled at this stage because we are not preloading data via .r files
output$fishery <- renderUI({
  if(!is.null(input$dat.name)){
    selectInput("fishery", "Fisheries:", choices=c("", levels(dat()$FISHERIES)),selected="", multiple=T)
  } 
})

# Data subsetting action button
output$dataGo <- renderUI({
  if(dataGo()) actionButton("dataGo", "Select Data")
})