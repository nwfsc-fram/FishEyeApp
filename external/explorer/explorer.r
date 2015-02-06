#this is the main content or output page for the explorer app


#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.r", local = TRUE)
source("external/explorer/explorerSourceFiles/ex.plot.reactives.r", local = TRUE)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.r", local = TRUE) 
source("external/explorer/explorerSourceFiles/doPlot.r", local = TRUE)

output$PlotMain<- renderPlot({
    if(!PermitPlot()) return()
    if(is.null(input$DataButton) || input$DataButton == 0) return()
    input$PlotSelect
    input$DodgeSelect
    input$ShortdescrSelect
    isolate( doPlot(dat = DatSub(), x = "SURVEY_YEAR", y = "VALUE/1000"))
    
}, height = 700, width = 1200)


output$TableMain <- renderDataTable({  
  input$DataButton 
  isolate(
    if(PermitPlot() & !is.null(DatSub())) {
      table <- DatSub()
      # names(table) <- c("Topic", "Year", "Value", "N", input$topicSelect)
    }
  )
})


# render plot from  to pdf for download
output$dlPlotMain <- downloadHandler(
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
