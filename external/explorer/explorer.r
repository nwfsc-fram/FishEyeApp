#this is the main content or output page for the explorer app


#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.r", local = TRUE)
source("external/explorer/explorerSourceFiles/ex.plot.reactives.r", local = TRUE)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.r", local = TRUE) 
source("external/explorer/explorerSourceFiles/doPlot.r", local = TRUE)
source("external/explorer/explorerSourceFiles/defaultText.R", local = TRUE)
source("external/explorer/explorerSourceFiles/doPlotThirds.r", local = TRUE)


output$PlotMain <- renderPlot({
  if(!PermitPlot()) return()
  doPlot(dat = DatSub(), x = "SURVEY_YEAR", y = "VALUE/1000", type = "summary")
}, height = 700, width = 1200)


output$TableMain <- renderDataTable({  
  if(!PermitPlot()) return(div(class = "block", height="700px"))
    if(PermitPlot() & !is.null(DatSub())) {
      table <- DatSub()
      names(table) <- c("Year", "Description", "Value", "N", "Variable", 
        "Category", "FishAK", "Stat")
      table
    }
})


output$PlotThirds <- renderPlot({
  if(!PermitPlot()) return()
  doPlot(dat = DatSubThirds(), x = "SURVEY_YEAR", y = "VALUE/100", type = "thirds")
}, height = 700, width = 1200)


# render plot from  to pdf for download
output$dlPlotMain <- downloadHandler(
    filename = function() {'dataexplorerPlot.pdf'},
    content = function(file){
      pdf(file = file, width=11, height=8.5)
      doPlot(dat = DatSub(), x = "SURVEY_YEAR", y = "VALUE/1000", type = "summary")
      dev.off()
    }
)


# render table of data subset to csv for download
output$dlTable <- downloadHandler(
    filename = function() { 'dataexplorerTable.csv' },
    content = function(file) {
      table <- DatSub()
      names(table) <- c("Year", "Description", "Value", "N", "Variable", 
                        "Category", "FishAK", "Stat")
      write.csv(table, file)
   }
)


output$dlPlotThirds <- downloadHandler(
    filename = function() {'ThirdsAnalysis.pdf'},
    content = function(file){
      pdf(file = file, width=11, height=8.5)
      doPlot(dat = DatSubThirds(), x = "SURVEY_YEAR", y = "VALUE/1000", type = "thirds")
      dev.off()
    }
)
