#This page 
#  1. calls the different R files needed
#  2. calls java script for the information icons
#  3. calls functions for plotting and data tables


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
observeEvent(input$ipo, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Economic measures side-by-side shows economic measures across years for different summary statistics. A drop-down menu below Plot Options allows you to switch between bar, point, and line plots. Composition of net revenue shows revenue, costs, and net revenue (revenue minus costs). You can examine either Variable Cost Net Revenue (VCNR) or Total Cost Net Revenue (TCNR). These plots are stacked bar plots. A figure that describes VCNR and TCNR is available by clicking the EXPLANATION OF THIS PLOT button (the button is visible once these plots are shown).')
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

observeEvent(input$iem, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Variable cost net revenue is revenue minus variable costs. Total cost net revenue is revenue minus variable costs and fixed costs. A visual demonstration of how these economic measures are derived can be found on the Summary Plots and Data start page. Further information on the economic measures can be found in the Definitions page.'
  )
})

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
  
  #  isolate({
  if(vars$counter%%2 == 0) return()
  else
    if(!PermitPlot()) return()
  
  if(PermitPlot() &  input$DodgeSelect == "Economic measures side-by-side"){
    doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
  if(PermitPlot() & input$DodgeSelect == "Composition of Total Cost Net Revenue"){
    doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
  if(PermitPlot() & input$DodgeSelect == "Composition of Variable Cost Net Revenue"){
    doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
  #  })
},  height=scale_height, width = "auto")

#print(length(input$VariableSelect))

output$TableMain <- renderDataTable({  
  input$data
  #  isolate({
  if(vars$counter%%2 != 0) return()
  else if(!PermitPlot()) return()#return(div(class = "block", height="700px"))
  if(#PermitPlot() & 
    !is.null(DatSubTable())
  ) {
    table <- subset(DatSubTable(), select = -c(CATEGORY))
    table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
    table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
      
    if(input$CategorySelect == "Fisheries"){
        table <- subset(table, select = -CS)
            if(input$Sect_sel=="CV"){
                names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Fished in Alaska", "Fished for whiting","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            } else {
                names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            }
       } else {
            if(input$Sect_sel=="CV"){
                names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure","Fished in Alaska","Fished for whiting", "Number of vessels", "Value", "Variance \n(MAD, SD)")
            } else if(input$Sect_sel=="M"|input$Sect_sel=="CP"){
              table <- subset(table, select = -CS)
              names(table) <- c("Year","Summary Variable","Statistic", "Economic measure","Number of vessels", "Value", "Variance \n(MAD, SD)")
            } else {
       if(input$CategorySelect=="Production activities"){
        table <- subset(table, select = -CS)
                names(table) <- c("Year", "Data summed across","Summary Variable","Statistic", "Economic measure","Number of processors", "Value", "Variance \n(MAD, SD)")
       }else {
                names(table) <- c("Year", "Data summed across","Summary Variable","Production Category", "Statistic", "Economic measure","Number of processors", "Value", "Variance \n(MAD, SD)")
       }}}
    #  table$YEAR <- as.numeric(table$YEAR),
    #names(table) <- c("Year", "Summary Variable", "FishAK", "Value","Statistic", "N", "Economic measure","")
    #   datatable(table, filter="bottom", rownames=F)
    table
  }
  #  })
}, options = list(autoWidth=FALSE))

output$TableThirds <- renderDataTable({ 
  input$data2
  # isolate({
  if(vars2$counter%%2 != 0) return()
  else
    
    if(!PermitPlot()) return()#return(div(class = "block", height="700px"))
  if(#PermitPlot() & 
    !is.null(DatThirdsTable())
  ) {
    table <- subset(DatThirdsTable(), select = -CATEGORY)  
    table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
    table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
      if(input$CategorySelect == "Fisheries"){
      table <- subset(table, select = -CS)
          if(input$Sect_sel=="CV"){
                names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Thirds", "Fished in Alaska","Fished for whiting", "Number of vessels", "Value","Variance \n(MAD, SD)")
          } else {
                names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Thirds", "Number of vessels", "Value","Variance \n(MAD, SD)")
          }
      } else{
          if(input$Sect_sel=="CV"){
                names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic","Economic measure","Thirds", "Fished in Alaska","Fished for whiting", "Number of vessels", "Value","Variance \n(MAD, SD)")
          } else {
          if(input$Sect_sel=="M"|input$Sect_sel=="CP"){
            table <- subset(table, select = -CS)
            names(table) <- c("Year", "Summary Variable", "Statistic","Economic measure","Thirds", "Fished in Alaska","Fished for whiting", "Number of vessels", "Value","Variance \n(MAD, SD)")
        
          } else {
      if(input$CategorySelect=="Production activities"){
              table <- subset(table, select = -CS)
              names(table) <- c("Year",'Data summed across', "Summary Variable","Statistic","Economic measure","Thirds",  "Number of processors", "Value","Variance \n(MAD, SD)")
       } else {
              names(table) <- c("Year","Data summed across", "Summary Variable","Production Category", "Statistic","Economic measure","Thirds", "Number of processors", "Value","Variance \n(MAD, SD)")
    }}}}
    #  table$YEAR <- as.numeric(table$YEAR),
    #names(table) <- c("Year", "Summary Variable", "FishAK", "Value","Statistic", "N", "Economic measure","")
    #   datatable(table, filter="bottom", rownames=F)
    table
  }
  #  })
}, options = list(autoWidth=FALSE))

#############################################

output$PlotThirds <- renderPlot({
  input$data2
  
  # isolate({
  if(vars2$counter%%2 == 0) return()
  else
    if(!PermitPlot()) return()
  doPlot(dat = DatSubThirds(), x = "YEAR", y = "VALUE/1000", type = "thirds")
  #})
}, height = 700, width = "auto")


# download buttons ------------------------------------------------------------
# render table of data subset to csv for download
output$dlTable <- downloadHandler(
  filename = function() { 'dataexplorerTable.csv' },
  content = function(file) {
    #     if(!PermitPlot()) return()
    if(input$tabs=="Panel2"){ 
      table <- DatThirdsTable() %>% mutate(Sector =input$Sect_sel)
      table$Sector <-c('Catcher Vessels','First Receivers and Shorebased Processors','Mothership vessels','Catcher Processor vessels')[match(table$Sector, c('CV','FR','M','CP'))]
      # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server
      if(input$Sect_sel=="FR"){
      names(table) <- c(4,1,3,2,"a "," b","c ","d "," e","f","g","h")
      temp <-    data.frame("Year", "Data summed across","Summary variable","Summary Variable category","Production Category","Statistic", "Economic Measure", "Thirds", "Number of processors", "Value","Variance (MAD,SD)","Sector")
      colnames(temp)=colnames(table)
      table <- rbindCommonCols(temp, table) 
      names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheriess NWFSC on ",
                              format(Sys.Date(), format="%B %d %Y")),"","","","","","","","","","","")
      } else {
        names(table) <- c(4,1,3,2,"a "," b","c ","d "," e","f","g","h","i")
        temp <-    data.frame("Year","Summary variable","Summary Variable category","Fisheries Category","Statistic", "Economic Measure", "Thirds", "Fished in Alaska","Fished for whiting",  "Number of vessels", "Value","Variance (MAD,SD)","Sector")
        colnames(temp)=colnames(table)
        table <- rbindCommonCols(temp, table) 
        names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheriess NWFSC on ",
                                format(Sys.Date(), format="%B %d %Y")),"","","","","","","","","","","","")
      }
    }else {
      table <- DatSubTable() %>% mutate(Sector =input$Sect_sel)
      table$Sector <-c('Catcher Vessels','First Receivers and Shorebased Processors','Mothership vessels','Catcher Processor vessels')[match(table$Sector, c('CV','FR','M','CP'))]
      if(input$Sect_sel=="CV"){
      # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server
      names(table) <- c(4,1,3,2,"a "," b","c ","d "," e","g","h","i")##c("Year", "Summary variable","FishAK", "Summary Variable category","Fisheries Category", "Value","Statistic",  "N", "Economic Measure")
      temp <-    data.frame("Year", "Summary variable","Summary Variable category", "Fisheries Category","Statistic", "Economic Measure", "Fished in Alaska", "Fished for whiting","Number of vessels","Value", "Variance (MAD, SD)","Sector")
      colnames(temp)=colnames(table)
      table <- rbindCommonCols(temp, table) 
      names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheriess NWFSC on ",
                              format(Sys.Date(), format="%B %d %Y")),"","","","","","","","","","","")
      } else if(input$Sect_sel=="FR"){
        # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server
        names(table) <- c(4,1,3,2,"a "," b","c ","d "," e","g","h")##c("Year", "Summary variable","FishAK", "Summary Variable category","Fisheries Category", "Value","Statistic",  "N", "Economic Measure")
        temp <-    data.frame("Year", "Data summed across",'Summary variable',"Summary Variable category", "Production Category","Statistic", "Economic Measure","Number of processors","Value", "Variance (MAD, SD)","Sector")
        colnames(temp)=colnames(table)
        table <- rbindCommonCols(temp, table) 
        names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheriess NWFSC on ",
                                format(Sys.Date(), format="%B %d %Y")),"","","","","","","","","","")
      } else {
        names(table) <- c(4,1,3,2,"a "," b","c ","d "," e","g")
        temp <-    data.frame("Year", "Summary variable","Summary Variable category", "Fisheries Category","Statistic", "Economic Measure","Number of vessels","Value", "Variance (MAD, SD)","Sector")
        colnames(temp)=colnames(table)
        table <- rbindCommonCols(temp, table) 
        names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheriess NWFSC on ",
                                format(Sys.Date(), format="%B %d %Y")),"","","","","","","","","")
        
      }
       } 
    print(table[1,])
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
    if(input$tabs=="Panel2"){ 
      doPlotDownload(dat = DatSubThirds(), x = "YEAR", y = "VALUE/1000", type = "thirds")}
    else {
         doPlotDownload(dat = DatSub(), x = "YEAR", y = "VALUE/1000", type = "summary")}
      }else {
        if(input$tabs=="Panel2"){ 
          dat = DatSubThirds()
          dat <- subset(dat, VARIABLE %in% input$VariableSelect[1:6])
          doPlotDownload(dat, x = "YEAR", y = "VALUE/1000", type = "thirds")
          dat = DatSubThirds()
          dat <- subset(dat, VARIABLE %in% input$VariableSelect[7:length(input$VariableSelect)])
          doPlotDownload(dat, x = "YEAR", y = "VALUE/1000", type = "thirds")
          }
        else {
          dat = DatSub()
          dat <- subset(dat, VARIABLE %in% input$VariableSelect[1:6])
          doPlotDownload(dat, x = "YEAR", y = "VALUE/1000", type = "summary")
          dat = DatSub()
          dat <- subset(dat, VARIABLE %in% input$VariableSelect[7:length(input$VariableSelect)])
          doPlotDownload(dat, x = "YEAR", y = "VALUE/1000", type = "summary")
          
          }
      }
    #  }
    dev.off()
  }
)

