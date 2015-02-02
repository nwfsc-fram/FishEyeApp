#this is the main content or output page for the explorer app


#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.r", local=T)
source("external/explorer/explorerSourceFiles/ex.plot.reactives.r", local=T)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.r",local=T) # source input/output objects associated with sidebar1


# output$TableMain <- renderDataTable({  
#   input$dataButton 
#   isolate(
#     if(permitPlot() & !is.null(dat.sub())) {
#       table <- dat.sub()
#       names(table) <- c("Topic", "Year", "Value", "N", input$topicSelect)
#       table
#     }
#   )
# })


# output$PlotMain<- renderPlot({
#     if(!is.null(dat.sub)) { 
#       print(plotOut())
#     } else  return()
# })


# render plot from  to pdf for download
output$dlPlot <- downloadHandler(
    filename = function() {'dataexplorerPlot.pdf'},
    content = function(file){
      pdf(file = file, width=11, height=8.5)
      print(plotOut())
      dev.off()
    }
)


# render table of data subset to csv for download
output$dlTable <- downloadHandler(
    filename = function() { 'dataexplorerTable.csv' },
    content = function(file) {
      table <- dat.sub()
      names(table) <- c("Topic", "Year", "Value", "N", input$topicSelect)
      write.csv(table, file)
   }
)
