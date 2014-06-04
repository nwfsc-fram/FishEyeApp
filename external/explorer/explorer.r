#this is the main content or output page for the explorer app

#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.r", local=T)
source("external/explorer/explorerSourceFiles/ex.plot.reactives.r", local=T)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.r",local=T) # source input/output objects associated with sidebar1

output$tableTest <- renderTable(dat.cast())

output$plotTest <- renderPlot({
  if(is.null(input$by.var) | is.null(input$group.var) | is.null(input$plotType)){ return()
  }else{
    print(plotBase() + plotGeom() + plotFacet() + plotGroupMean() + plotPalette() + plotLabels() + plotScales() + plotTheme())
  }
})