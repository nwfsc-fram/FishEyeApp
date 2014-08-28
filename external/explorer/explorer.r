#this is the main content or output page for the explorer app

#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.r", local=T)
source("external/explorer/explorerSourceFiles/ex.plot.reactives.r", local=T)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.r",local=T) # source input/output objects associated with sidebar1

output$tableTest <- renderDataTable({
  if(!is.null(dat.sub())){
    table <- dat.sub()
    names(table)[1:3] <- c(input$topicSelect, "Year", input$dat.name)
    table
  } else return()
})

output$plotTest <- renderPlot({
    if(!is.null(dat.sub()) && !is.null(input$plotType)){ 
      print(plotBase() + plotGeom() + plotGroupMean() + plotPalette() + plotLabels() + plotScales() + plotTheme())
    } else  return()
})