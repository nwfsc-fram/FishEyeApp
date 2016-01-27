#this is the main content or output page for the explorer app


#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.R", local = TRUE)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.R", local = TRUE) 
source("external/explorer/explorerSourceFiles/doPlot.R", local = TRUE)
source("external/explorer/explorerSourceFiles/doPlotDownload.R", local = TRUE)
source("external/explorer/explorerSourceFiles/defaultText.R", local = TRUE)

observeEvent(input$istat, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'The median and average both attempt to provide information about the results for a representative vessel, however they do it in different ways. The median means that half of the vessels have a larger result than the median, and half are smaller. The average, or mean, is the sum of the values divided by the number of responses. If the data do not have many extreme responses in a particular direction, the median and mean will be very similar. However, if the data are skewed by extreme responses, then the median is a better measure of the result for a typical vessel. The total provides a measure of the fleet as a whole. The fleet-wide total is used to measure how the entire fleet is doing, rather than a representative vessel.')
})
#observeEvent(input$ipo, {
#  session$sendCustomMessage(type = 'testmessage',
#                            message = 'Economic measures side-by-side shows economic measures across years for different summary statistics. A drop-down menu below Plot Options allows you to switch between bar, point, and line plots. Composition of net revenue shows revenue, costs, and net revenue (revenue minus costs). You can examine either Variable Cost Net Revenue (VCNR) or Total Cost Net Revenue (TCNR). These plots are stacked bar plots. A figure demonstrating how VCNR and TCNR can be obtained by clicking the Explanation Of This Plot button.')
#})
observeEvent(input$ifg, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Use these buttons to automatically select a group of fisheries. Clicking the button a second time will deselect the selected fisheries. For example, clicking All fisheries will select each box below and will produce figures for each individual fishery, the combined activities across all fisheries, the combined activities across all catch share fisheries, and the combined activities across all non-catch share fisheries.')
})
observeEvent(input$iof, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'You can choose to show activities for an individual fisheries or activities across all fisheries, all catch share fisheries, or all non-catch share fisheries.')
})
observeEvent(input$isummed, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'For each homeport, state, or vessel length class, you can select to show activities across all fisheries, only catch share fisheries, or only non-catch share fisheries.')
})

observeEvent(input$iem, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Variable cost net revenue is revenue minus variable costs. Total cost net revenue is revenue minus fixed and total costs. A visual demonstration of how these economic measures are derived can be found on the Visual Data with Plots tab. Further information on the economic measures can be found in the Definitions page.'
  )
})

observeEvent(input$ivs, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'We provide the options to view activities for individual fisheries and activities combined over a group of fisheries. For example, the option All catch share fisheries shows the combined activies across the five catch share fisheries.')
})

observeEvent(input$iacross, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Select vessels by fisheries, homeport, state, or vessel length class. Then, for selected vessels, you can choose to show results for activities within the selected fisheries, across all catch share fisheries or across all 
                            West Coast fisheries. ')
})

observeEvent(input$iVesSum, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'For all vessels that fished within selected fisheries, show data for activities either within the selected fisheries, across all Catch Share fisheries, or across all West Coast fisheries. ')
})

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
  input$data
  if(vars$counter%%2 == 0) return()
    if(!PermitPlot()) return()
 if(PermitPlot()){
      doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
 },  height=scale_height, width = "auto")


output$TableMain <- renderDataTable({  
  input$data
  if(vars$counter%%2 != 0) return()
  else if(!PermitPlot()) return()
    if(!is.null(DatSubTable())) {
      if(input$CategorySelect == "Fisheries"){
      table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
      table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
      table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
      names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Fished in Alaska","Fished for whiting", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
      } else {
      table <- subset(DatSubTable(), select = -CATEGORY)  
      table$VALUE <- paste('$', prettyNum(table$VALUE, 
                                          big.mark = ",", format = 'f', digits = 5, trim=T))
      names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure","Fished in Alaska", "Fished for whiting","Number of vessels", "Value", "Variance \n(MAD, SD)")
      }
      table
    }
})

output$TableThirds <- renderDataTable({  
  input$data2
  if(vars2$counter%%2 != 0) return()
  else if(!PermitPlot()) return()
  if(!is.null(DatThirdsTable())) {
    if(input$CategorySelect == "Fisheries"){
      table <- subset(DatThirdsTable(), select = -c(CATEGORY, CS))
      table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
      table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
      names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Thirds", "Fished in Alaska","Fished for whiting", "Number of vessels", "Value","Variance \n(MAD, SD)")
    } else {
      table <- subset(DatThirdsTable(), select = -CATEGORY)  
      table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
      table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
      names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic","Economic measure","Thirds", "Fished in Alaska","Fished for whiting", "Number of vessels", "Value","Variance \n(MAD, SD)")
    }
    table
  }
})


output$PlotThirds <- renderPlot({
  input$data2
  if(vars2$counter%%2 == 0) return()
  else
    if(!PermitPlot()) return()
  doPlot(dat = DatSubThirds(), x = "YEAR", y = "VALUE/1000", type = "thirds")
}, height = 700, width = "auto")


# download buttons ------------------------------------------------------------
output$dlTable <- downloadHandler(
    filename = function() { 'dataexplorerTable.csv' },
    content = function(file) {
       if(input$tabs=="Panel2"){ 
        table <- DatThirdsTable()
        
        # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server
       names(table) <- c(4,1,3,2,"a "," b","c ","d "," e","f","g","h")##c("Year", "Summary variable","FishAK", "Summary Variable category","Fisheries Category", "Value","Statistic",  "N", "Economic Measure")
        temp <-    data.frame("Year", "Summary variable","Summary Variable category","Fisheries Category","Statistic", "Economic Measure", "Thirds", "Fished in Alaska", "Fished for whiting", "Number of vessels", "Value","Variance (MAD,SD)")
        colnames(temp)=colnames(table)
        table <- rbindCommonCols(temp, table) 
        names(table) <- c(paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheriess NWFSC on ",
                                format(Sys.Date(), format="%B %d %Y")),"","","","","","","","","","")
      }
      else {
         table <- DatSubTable()
      
      # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server
      names(table) <- c(4,1,3,2,"a "," b","c ","d "," e","g","h")##c("Year", "Summary variable","FishAK", "Summary Variable category","Fisheries Category", "Value","Statistic",  "N", "Economic Measure")
      temp <-    data.frame("Year", "Summary variable","Summary Variable category", "Fisheries Category","Statistic", "Economic Measure", "Fished in Alaska", "Fished for whiting", "Number of vessels","Value", "Variance (MAD, SD)")
      colnames(temp)=colnames(table)
      table <- rbindCommonCols(temp, table) 
      names(table) <- c(paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheriess NWFSC on ",
                              format(Sys.Date(), format="%B %d %Y")),"","","","","","","","")
       } 
           write.csv(table, file)
   })

# render plot from  to pdf for download
output$dlFigure <- downloadHandler(
  filename = function() {'dataexplorerPlot.pdf'},
  content = function(file){
     if(!PermitPlot()) return()
    pdf(file = file, width=10.25, height=7.5, onefile=F)
    if(input$tabs=="Panel2"){ 
      doPlotDownload(dat = DatSubThirds(), x = "YEAR", y = "VALUE/1000", type = "thirds")}
    else {
        doPlotDownload(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
    dev.off()
 })

