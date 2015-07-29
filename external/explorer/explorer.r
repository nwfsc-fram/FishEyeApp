#this is the main content or output page for the explorer app


#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.R", local = TRUE)
#source("external/explorer/explorerSourceFiles/ex.plot.reactives.R", local = TRUE)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.R", local = TRUE) 
source("external/explorer/explorerSourceFiles/doPlot.R", local = TRUE)
source("external/explorer/explorerSourceFiles/doPlotDownload.R", local = TRUE)
source("external/explorer/explorerSourceFiles/defaultText.R", local = TRUE)
#source("external/explorer/explorerSourceFiles/doPlotThirds.R", local = TRUE)


output$PlotMain <- renderPlot({
  if(!PermitPlot()) return()
  if(PermitPlot() & input$PlotSelect != "Bar"){
    doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")
  } else {
  if(PermitPlot() & input$DodgeSelect == "Compare economic measures side-by-side"){
  doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
  if(PermitPlot() & input$DodgeSelect == "Total cost revenue figure"){
    doPlot(dat = DatSub2(), x = "YEAR", y = "VALUE/1000", type = "summary")}
  if(PermitPlot() & input$DodgeSelect == "Variable cost revenue figure"){
    doPlot(dat = DatSub3(), x = "YEAR", y = "VALUE/1000", type = "summary")}
}}, height = 700, width = 1200)


output$TableMain <- renderDataTable({  
  if(!PermitPlot()) return()#return(div(class = "block", height="700px"))
    if(PermitPlot() & !is.null(DatSubTable())
       ) {
      table <- subset(DatSubTable(), select = -CATEGORY)
    #  table$YEAR <- as.numeric(table$YEAR),
      table$VALUE <- paste('$', prettyNum(table$VALUE, 
        big.mark = ",", format = 'f', digits = 5, trim=T))
      names(table) <- c("Year", "Summary Variable", "Economic measure", "N",
                        "Statistic", "Value", "FishAK")
      datatable(table, filter="bottom", rownames=F)
    }
})


output$PlotThirds <- renderPlot({
  if(!PermitPlot()) return()
  doPlot(dat = DatSubThirds(), x = "YEAR", y = "VALUE/1000", type = "thirds")
}, height = 700, width = 1200)


# download buttons ------------------------------------------------------------

# render table of data subset to csv for download
output$dlTable <- downloadHandler(
    filename = function() { 'dataexplorerTable.csv' },
    content = function(file) {
 #     if(!PermitPlot()) return()
      table <- DatSubTable()
      names(table) <- c("Year", "Summary variable category","Economic Measure", "N","Statistic",   "Value",
                        "Summary Variable", "FishAK")
      write.csv(table, file)
   }
)


# render plot from  to pdf for download
output$dlFigure <- downloadHandler(
  filename = function() {'dataexplorerPlot.pdf'},
  content = function(file){
    pdf(file = file, width=11, height=8.5)
    if(input$tabs=="Variability <br> Analysis"){ 
      doPlotDownload(dat = DatSubThirds(), x = "YEAR", y = "VALUE/1000", type = "thirds") }
    else {
      if(input$tabs=="Visualize <br> Data"){
        if(PermitPlot() & input$PlotSelect != "Bar"){
          doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")
        } else {
      if(PermitPlot() & input$DodgeSelect == "Compare economic measures side-by-side"){
        doPlotDownload(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
      else if(PermitPlot() & input$DodgeSelect == "Total cost revenue figure"){
        doPlotDownload(dat = DatSub2(), x = "YEAR", y = "VALUE/1000", type = "summary")}
      else if(PermitPlot() & input$DodgeSelect == "Variable cost revenue figure"){
        doPlotDownload(dat = DatSub3(), x = "YEAR", y = "VALUE/1000", type = "summary")#} 
      }
        }  
      }
    }
    dev.off()
    
    
  }
)


#old download button
# render plot from  to pdf for download
#output$dlPlotMain <- downloadHandler(
#    filename = function() {'dataexplorerPlot.pdf'},
#    content = function(file){
#      pdf(file = file, width=11, height=8.5)
#      if(PermitPlot() & input$PlotSelect != "Bar"){
#        doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")
#      } else {
        
#           if(PermitPlot() & input$DodgeSelect == "Compare economic measures side-by-side"){
#        doPlotDownload(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
#      else if(PermitPlot() & input$DodgeSelect == "Total cost revenue figure"){
#        doPlotDownload(dat = DatSub2(), x = "YEAR", y = "VALUE/1000", type = "summary")}
#      else if(PermitPlot() & input$DodgeSelect == "Variable cost revenue figure"){
#        doPlotDownload(dat = DatSub3(), x = "YEAR", y = "VALUE/1000", type = "summary")#} 
# }     }
#      dev.off()
#    }
#)

#output$dlPlotThirds <- downloadHandler(
#    filename = function() {'ThirdsAnalysis.pdf'},
#    content = function(file){
#      pdf(file = file, width=11, height=8.5)
#      doPlotDownload(dat = DatSubThirds(), x = "YEAR", y = "VALUE/1000", type = "thirds")
#      dev.off()
#    }
#)

