#======================================
#
# this page  
#  1. calls the different R files needed
#  2. calls java script for the information icons
#  3. calls functions for plotting and data tables
#======================================


#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.R", local = TRUE)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.R", local = TRUE) 
source("external/explorer/explorerSourceFiles/doPlot.R", local = TRUE)
source("external/explorer/explorerSourceFiles/doPlotDownload.R", local = TRUE)
source("external/explorer/explorerSourceFiles/defaultText.R", local = TRUE)

observeEvent(input$iprod, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Although all processors included in FISHEyE hold a site license for the catch share program, not all processors process catch share species. Select CATCH SHARE PROCESSORS to show activities only for processors that processed catch share species or ALL PROCESSORS to show activities for all processors regardless of whether they processed catch share species.')
})
observeEvent(input$istat, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'The median and average provide information about a representative vessel or processor; however, they do it in different ways. The median means that half of the vessels or processors have a larger result than the median, and half are smaller. The average, or mean, is the sum of the values divided by the number of responses. If the data are skewed by extreme responses, then the median is a better measure of the result for a typical vessel or processor. The fleet- or processor-wide total is the sum over all vessels or processors selected and measures how the entire fleet is doing, rather than a representative vessel or processor.')
})
observeEvent(input$ifg, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Use these buttons to automatically select a group of fisheries. Clicking the button a second time will deselect the selected fisheries. For example, clicking ALL FISHERIES will select each box below and will produce figures for each individual fishery, the combined activities across all fisheries, the combined activities across all catch share fisheries, and the combined activities across all non-catch share fisheries.')
})
observeEvent(input$iof, {
  session$sendCustomMessage(type = 'testmessage',
                                    message = 'You can choose to show activities for an individual fisheries or activities across all fisheries, all catch share fisheries, or all non-catch share fisheries.')
  })
observeEvent(input$isummed, {
  session$sendCustomMessage(type = 'testmessage',
                            if(input$Sect_sel=="CV"){
                                    message = 'For each homeport, state of homeport, or vessel length class, you can select to show activities across all fisheries, only catch share fisheries, or only non-catch share fisheries.'
                            } else if(input$Sect_sel=="FR"){
                                    message = 'For each region or processor size class, you can select to show activities across all production, only groundfish production, or only other species production.'
                            })
})

#observeEvent(input$iem, {
#  session$sendCustomMessage(type = 'testmessage',
#                            message = 'COST INFORMATION ICON.'
#  )
#})

observeEvent(input$ivs, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Select individual fisheries or activities combined over a group of fisheries. For example, the option ALL CATCH SHARE FISHERIES shows the combined activities across the five catch share fisheries.')
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
  else
    if(!PermitPlot()) return()
  if(PermitPlot()) {
    doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000")
    }
},  height=scale_height, width = "auto")


output$TableMain <- renderDataTable({  
  input$data
  if(vars$counter%%2 != 0) return()
  else if(!PermitPlot()) return()
  if(
    !is.null(DatSubTable())
  ) {
    table <- subset(DatSubTable(), select = -c(CATEGORY))
    table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
    table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
      
    if(input$CategorySelect == "Fisheries"){
            if(input$Sect_sel=="CV"){
                table <- subset(table, select = -CS)
                names(table) <- c("Year", "Summary Variable", "Statistic", "Costs measure", "Data summed across","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            } else {
                names(table) <- c("Year", "Summary Variable", "Statistic", "Costs measure","Data summed across","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            }
       } else {
            if(input$Sect_sel=="CV"){
                names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Costs measure","Data summed across", "Number of vessels", "Value", "Variance \n(MAD, SD)")
            } else if(input$Sect_sel=="M"|input$Sect_sel=="CP"){
              names(table) <- c("Year","Summary Variable","Statistic", "Costs measure","Data summed across","Number of vessels", "Value", "Variance \n(MAD, SD)")
            } else {
       if(input$CategorySelect=="Production activities"){
        table <- subset(table, select = -CS)
                names(table) <- c("Year", "Summary Variable","Statistic", "Costs measure","Data summed across","Number of processors", "Value", "Variance \n(MAD, SD)")
       }else {
                names(table) <- c("Year","Summary Variable","Production Category", "Statistic", "Costs measure","Data summed across","Number of processors", "Value", "Variance \n(MAD, SD)")
       }}}
    table
  }
}, options = list(autoWidth=FALSE))

#############################################


# download buttons ------------------------------------------------------------
# render table of data subset to csv for download
output$dlTable <- downloadHandler(
  filename = function() { 'CostExplorerTable.csv' },
  content = function(file) {
    table <- DatSubTable() %>% mutate(Sector =input$Sect_sel)
      table$Sector <-c('Catcher Vessels','First Receivers and Shorebased Processors','Mothership vessels','Catcher-Processor vessels')[match(table$Sector, c('CV','FR','M','CP'))]
      if(input$Sect_sel=="CV"){
      # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server
      names(table) <- c(4,1,3,2,"a "," b","c ","d "," e","g","h")##c("Year", "Summary variable","FishAK", "Summary Variable category","Fisheries Category", "Value","Statistic",  "N", "Economic Measure")
      temp <-    data.frame("Year", "Summary variable","Summary Variable category", "Fisheries Category","Statistic", "Costs measure", "Data summed across","Number of vessels","Value", "Variance (MAD, SD)","Sector")
      colnames(temp)=colnames(table)
      table <- rbindCommonCols(temp, table) 
      names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheriess NWFSC on ",
                              format(Sys.Date(), format="%B %d %Y")),"","","","","","","","","","")
      } else if(input$Sect_sel=="FR"){
        # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server
        names(table) <- c(4,1,3,2,"a "," b","c ","d "," e","g","h")##c("Year", "Summary variable","FishAK", "Summary Variable category","Fisheries Category", "Value","Statistic",  "N", "Economic Measure")
        temp <-    data.frame("Year", "Data summed across",'Summary variable',"Summary Variable category", "Production Category","Statistic", "Costs measure","Number of processors","Value", "Variance (MAD, SD)","Sector")
        colnames(temp)=colnames(table)
        table <- rbindCommonCols(temp, table) 
        names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheriess NWFSC on ",
                                format(Sys.Date(), format="%B %d %Y")),"","","","","","","","","","")
      } else {
        names(table) <- c(4,1,3,2,"a "," b","c ","d "," e","g")
        temp <-    data.frame("Year", "Summary variable","Summary Variable category", "Fisheries Category","Statistic", "Costs measure","Number of vessels","Value", "Variance (MAD, SD)","Sector")
        colnames(temp)=colnames(table)
        table <- rbindCommonCols(temp, table) 
        names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheriess NWFSC on ",
                                format(Sys.Date(), format="%B %d %Y")),"","","","","","","","","")
        
      } 
    write.csv(table, file)
  }
)


# render plot from  to pdf for download
output$dlFigure <- downloadHandler(
  filename = function() {'dataexplorerPlot.pdf'},
  content = function(file){
    if(!PermitPlot()) return()
    pdf(file = file, width=10.25, height=7.5, onefile=T)
    if(length(input$VariableSelect)<=6){
         doPlotDownload(dat = DatSub(), x = "YEAR", y = "VALUE/1000")
     } else {
          dat = DatSub()
          dat <- subset(dat, VARIABLE %in% input$VariableSelect[1:6])
          doPlotDownload(dat, x = "YEAR", y = "VALUE/1000")
          dat = DatSub()
          dat <- subset(dat, VARIABLE %in% input$VariableSelect[7:length(input$VariableSelect)])
          doPlotDownload(dat, x = "YEAR", y = "VALUE/1000")
          
      }
    dev.off()
  }
)
