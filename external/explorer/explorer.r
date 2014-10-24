#this is the main content or output page for the explorer app

#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.r", local=T)
source("external/explorer/explorerSourceFiles/ex.plot.reactives.r", local=T)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.r",local=T) # source input/output objects associated with sidebar1

output$tableTest <- renderDataTable({
  if(!is.null(dat.sub())){
    table <- dat.sub()
    names(table) <- c("Topic", "Year", "Value", "N", input$topicSelect)
    table
  } else return()
})

output$plotTest <- renderPlot({
    if(!is.null(dat.sub()) && !is.null(input$plotType)){ 
      print(plotOut())
    } else  return()
})

output$dlPlot <- downloadHandler( # render plot from  to pdf for download
  filename = 'dataexplorerPlot.pdf',
  content = function(file){
    pdf(file = file, width=11, height=8.5)
    print(plotOut())
    dev.off()
  }
)

output$dlTable <- downloadHandler( # render table of data subset to csv for download
  filename = function() { 'dataexplorerTable.csv' },
  content = function(file) {
    table <- dat.sub()
    names(table) <- c("Topic", "Year", "Value", "N", input$topicSelect)
    write.csv(table, file)
  }
)