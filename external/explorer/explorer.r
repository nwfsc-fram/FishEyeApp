#this is the main content or output page for the explorer app


#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.R", local = TRUE)
#source("external/explorer/explorerSourceFiles/ex.plot.reactives.R", local = TRUE)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.R", local = TRUE) 
source("external/explorer/explorerSourceFiles/doPlot.R", local = TRUE)
source("external/explorer/explorerSourceFiles/doPlotDownload.R", local = TRUE)
source("external/explorer/explorerSourceFiles/defaultText.R", local = TRUE)
#source("external/explorer/explorerSourceFiles/doPlotThirds.R", local = TRUE)

#observeEvent(input$send, {
#  session$sendCustomMessage(type = 'testmessage',
#                            message = 'Your message has been sent')
#})

  
#output$PlotMain <- renderPlot({
#  if(!PermitPlot()) return()
#  if(PermitPlot() & input$PlotSelect != "Bar"){
#    doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")
#  } else {
#  if(PermitPlot() & input$PlotSelect=="Bar" & input$DodgeSelect == "Economic measures side-by-side"){
#  doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
#  if(PermitPlot() & input$PlotSelect=="Bar" & input$DodgeSelect == "Composition of total cost revenue"){
#    doPlot(dat = DatSub2(), x = "YEAR", y = "VALUE/1000", type = "summary")}
#  if(PermitPlot() & input$PlotSelect=="Bar" & input$DodgeSelect == "Composition of variable cost revenue"){
#    doPlot(dat = DatSub3(), x = "YEAR", y = "VALUE/1000", type = "summary")}
#}}, height = 700, width = 1200)

scale_height <- function(){
 if(length(input$VariableSelect)<=2){ 
   700 }  else if(length(input$VariableSelect)>2 & length(input$VariableSelect)<=4) { 
     800}  else if(length(input$VariableSelect)>4 & length(input$VariableSelect)<=6) { 
       900} else if(length(input$VariableSelect)>6 & length(input$VariableSelect)<=8) { 
         1000} else if(length(input$VariableSelect)>8 & length(input$VariableSelect)<=10) {
           1100} else { 
             1200 }
}

output$PlotMain <- renderPlot({
  if(!PermitPlot()) return()
   
 if(PermitPlot() &  input$DodgeSelect == "Economic measures side-by-side"){
      doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
    if(PermitPlot() & input$DodgeSelect == "Composition of total cost net revenue"){
      doPlot(dat = DatSub2(), x = "YEAR", y = "VALUE/1000", type = "summary")}
    if(PermitPlot() & input$DodgeSelect == "Composition of variable cost net revenue"){
      doPlot(dat = DatSub3(), x = "YEAR", y = "VALUE/1000", type = "summary")}
 },  height=scale_height, width = "auto")

#print(length(input$VariableSelect))

output$TableMain <- renderDataTable({  
  if(!PermitPlot()) return()#return(div(class = "block", height="700px"))
    if(#PermitPlot() & 
      !is.null(DatSubTable())
       ) {
      if(input$CategorySelect == "Fisheries"){
      table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
      table$VALUE <- paste('$', prettyNum(table$VALUE, 
        big.mark = ",", format = 'f', digits = 5, trim=T))
      names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Fished in Alaska", "Value", "Number of vessels")
      } else {
      table <- subset(DatSubTable(), select = -CATEGORY)  
      table$VALUE <- paste('$', prettyNum(table$VALUE, 
                                          big.mark = ",", format = 'f', digits = 5, trim=T))
      names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure","Fished in Alaska", "Value", "Number of vessels")
      }
    #  table$YEAR <- as.numeric(table$YEAR),
      #names(table) <- c("Year", "Summary Variable", "FishAK", "Value","Statistic", "N", "Economic measure","")
   #   datatable(table, filter="bottom", rownames=F)
      table
    }
})


#############################################
#ADDED
##############################################
#output$TableMain <- renderDataTable({  
#  if(!PermitPlot()) return()#return(div(class = "block", height="700px"))
#  if(#PermitPlot() & 
#    !is.null(DatThirdsTable())
#  ) {
#    if(input$CategorySelect == "Fisheries"){
#      table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
#      table$VALUE <- paste('$', prettyNum(table$VALUE, 
#                                          big.mark = ",", format = 'f', digits = 5, trim=T))
#      names(table) <- c("Year", "Summary Variable", "Fished in Alaska", "Value","Statistic", "Number of vessels", "Economic measure")
#    } else {
#      table <- subset(DatSubTable(), select = -CATEGORY)  
#      table$VALUE <- paste('$', prettyNum(table$VALUE, 
#                                          big.mark = ",", format = 'f', digits = 5, trim=T))
#      names(table) <- c("Year", "Summary Variable", "Fished in Alaska","Fisheries Category", "Value","Statistic", "Number of vessels", "Economic measure")
#    }
    #  table$YEAR <- as.numeric(table$YEAR),
    #names(table) <- c("Year", "Summary Variable", "FishAK", "Value","Statistic", "N", "Economic measure","")
    #   datatable(table, filter="bottom", rownames=F)
#    table
#  }
#})
#############################################
#ADDED
##############################################

output$PlotThirds <- renderPlot({
  if(!PermitPlot()) return()
  doPlot(dat = DatSubThirds(), x = "YEAR", y = "VALUE/1000", type = "thirds")
}, height = 700, width = "auto")


# download buttons ------------------------------------------------------------
################################################################
##MODIFIED
#################################################################
# render table of data subset to csv for download
output$dlTable <- downloadHandler(
    filename = function() { 'dataexplorerTable.csv' },
    content = function(file) {
 #     if(!PermitPlot()) return()
      if(input$tabs=="Panel2"){ 
        table <- DatThirdsTable()
        
        # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server
       names(table) <- c(4,1,3,2,"a "," b","c ","d "," e","f")##c("Year", "Summary variable","FishAK", "Summary Variable category","Fisheries Category", "Value","Statistic",  "N", "Economic Measure")
        temp <-    data.frame("Year", "Summary variable","Summary Variable category","Fisheries Category","Statistic", "Economic Measure", "Thirds", "Fished in Alaska", "Value",  "Number of vessels")
        colnames(temp)=colnames(table)
        table <- rbindCommonCols(temp, table) 
        names(table) <- c(paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheriess NWFSC on ",
                                format(Sys.Date(), format="%B %d %Y")),"","","","","","","","")
      }
      else {
         table <- DatSubTable()
      
      # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server
      names(table) <- c(4,1,3,2,"a "," b","c ","d "," e")##c("Year", "Summary variable","FishAK", "Summary Variable category","Fisheries Category", "Value","Statistic",  "N", "Economic Measure")
      temp <-    data.frame("Year", "Summary variable","Summary Variable category", "Fisheries Category","Statistic", "Economic Measure", "Fished in Alaska","Value", "Number of vessels")
      colnames(temp)=colnames(table)
      table <- rbindCommonCols(temp, table) 
      names(table) <- c(paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheriess NWFSC on ",
                              format(Sys.Date(), format="%B %d %Y")),"","","","","","","","")
       } 
           write.csv(table, file)
   }
)
################################################################
##MODIFIED
#################################################################


# render plot from  to pdf for download
output$dlFigure <- downloadHandler(
  filename = function() {'dataexplorerPlot.pdf'},
  content = function(file){
     if(!PermitPlot()) return()
    pdf(file = file, width=10.25, height=7.5, onefile=F)
    if(input$tabs=="Panel2"){ 
      doPlotDownload(dat = DatSubThirds(), x = "YEAR", y = "VALUE/1000", type = "thirds")}
    else {
       ## if(input$PlotSelect != "Bar"){
       #   doPlotDownload(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")
      #  } else {
      if(input$DodgeSelect == "Economic measures side-by-side"){
        doPlotDownload(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
      else if(input$DodgeSelect == "Composition of total cost net revenue"){
        doPlotDownload(dat = DatSub2(), x = "YEAR", y = "VALUE/1000", type = "summary")}
      else if(input$DodgeSelect == "Composition of variable cost net revenue"){
        doPlotDownload(dat = DatSub3(), x = "YEAR", y = "VALUE/1000", type = "summary")#} 
      }
        }  
    #  }
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
        
#           if(PermitPlot() & input$DodgeSelect == "Economic measures side-by-side"){
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
