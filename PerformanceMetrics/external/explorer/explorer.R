#this is the main content or output page for the explorer app


#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.R", local = TRUE)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.R", local = TRUE) 
source("external/explorer/explorerSourceFiles/doPlot.R", local = TRUE)
source("external/explorer/explorerSourceFiles/doPlotDownload.R", local = TRUE)
source("external/explorer/explorerSourceFiles/defaultText.R", local = TRUE)

enableBookmarking("url")


observeEvent(input$istat, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Statistics may not be appliable for all metrics. The median and mean both attempt to provide information about the results for a representative vessel, however they do it in different ways. The median means that half of the vessels have a larger result than the median, and half are smaller. The mean, is the sum of the values divided by the number of responses. If the data do not have many extreme responses in a particular direction, the median and mean will be very similar. However, if the data are skewed by extreme responses, then the median is a better measure of the result for a typical vessel. The total provides a measure of the fleet as a whole. The fleet-wide total is used to measure how the entire fleet is doing, rather than a representative vessel.')
})
observeEvent(input$ipo, {
  session$sendCustomMessage(type = 'testmessage',
                            if(input$LayoutSelect=="Metrics") {
                            message = 'See the Definitions Page for a description of each metric. Not all metrics may be applicable for a given statistic.'
                            } else { 
                              message = 'See the Definitions Page for a description of each metric. Not all statistics may be applicable for a given metric.'}
                       
                            #if(input$demSelect=="Number of vessels"){
                              #message = 'Number of vessels actively participating (i.e., had an active permit and non-zero revenue).'
                            #} else if(input$demSelect=="Vessel length"){
                             # message = 'The length of vessels in feet.'
                            #}else if(input$demSelect=="Fishery participation"){
                              #message = 'Count of fisheries that vessels participated in. Changes may indicate specialization or diversification.'
                            #}else if(input$demSelect=="Exponential Shannon Index"){
                             # message = 'Measures the income diversification of a vessel across revenue sources. A larger number corresponds to increased diversification. Changes may indicate specialization or diversification.'
                            #}else if(input$demSelect=="Proportion of revenue from CS fishery"){
                             # message = "The average proportion of a vessel's total revenue that comes from fish caught in the limited entry trawl or catch share fishery measures how reliant vessels are on revenue from the limited entry/catch shares fishery."
                            #}else if(input$demSelect=="Days at sea"){
                            #  message = 'The number of days at sea may indicate specialization, efficiency, or consolidation.'
                            #}else if(input$demSelect=="Gini coefficient"){
                            #  message = 'Measures the degree of catch share revenue concentration among vessels. A value of zero would represent all vessels earning the same revenue, and a value of one would represent one vessel earning all of the revenue. The value of the Gini coefficient can be affected by fleet consolidation and specialization.'
                            #}else if(input$socSelect=="Number of positions"){
                             # message = 'Number of positions (including captain and crew) is a lower bound for employment in the fishery, and is affected by positions per vessel and the number of vessels fishing.'
                            #}else if(input$SocSelect=="Crew wage per day"){
                            #  message = 'Daily wage paid to a crewmember operating in the limited entry/catch shares fishery.'
                            #}else if(input$SocSelect=="Revenue per position-day"){
                            #  message = 'Revenue divided by position-days, where position-days are calculated as days at sea multipled by number of positions per vessel. This metric is a measure of productivity (in terms of revenue generation) of captain and crew.'
                           # }else if(input$SocSelect=="Revenue per crew-day"){
                            #  message = 'Revenue divided by crew-days, where position days are calculated as days at sea multipled by number of positions per vessel. This metric is a measure of productivity (in terms of revenue generation) of crew members.'
                            #}else if(input$SocSelect=="Seasonality"){
                            #  message = 'The date (day of year, Jan. 1 = 1) on which 50% of the total volume of catch was landed in the fishery. Metric measures broad-scale changes in the seasonality of fishing for catch shares fish. It can also indicate changes in total allowable catch (TAC); it may take the fleet longer to catch a higher TAC/ACL.'
                            #}else if(input$SocSelect=="Share of landings by state"){
                             # message = 'Share of total landings, share of landings by whiting vessels, and share of landings by non-whiting groundfish vessels in each state or at sea. Shares are in terms of revenue. Vessels may deliver to more than one location.'
                            #}#else if(input$demSelect=="Herfindahl-Hirschman Index"){
                            #  message = 'The Herfindahl-Hirschman Index is a measure of market concentration. It is calculated as the sum of the squares of the market shares, defined here by the share of the catch share revenues earned by each vessel. Values range from 0 to 10,000. Lower values indicate low market concentration (and higher competition).'
                            #}
                              )
})
observeEvent(input$FRr, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'The region corresponding to each processors. For First Receivers and Shorebased Processors, two regions are recognized: Washington/Oregon and California.')
})
observeEvent(input$FRs, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'First receivers and shorebased processors are grouped into three size classes based on the average number of workers: large (> 200 workers), medium (100 - 200 workers), and small (< 100 workers). Processor size was determined by examining the maximum weekly number of production workers employed by a processor (as reported on the EDC form) for each year. Then the weekly maximum was averaged across all EDC years to place processors in one size category for all years.')
})
observeEvent(input$FRi, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'For shorebased processors, information on the EDC form  is collected at the species level (e.g. fish production information), not the fishery level like the catcher vessels.')
})
observeEvent(input$ifg, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Use these buttons to automatically select a group of fisheries. Clicking the button a second time will deselect the selected fisheries. For example, clicking All fisheries will select each box below and will produce figures for each individual fishery, the combined activities across all fisheries, the combined activities across all catch share fisheries, and the combined activities across all non-catch share fisheries.')
})
observeEvent(input$iof, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'You can choose to show activities for an individual fisheries or activities across all fisheries, all catch share fisheries, or all non-catch share fisheries.')
})
####EDIT HERE
observeEvent(input$isummed, {
  session$sendCustomMessage(type = 'testmessage',
                            if(input$Sect_sel=="CV"){
                            message = 'For each homeport, state, or vessel length class, you can select to show activities across all fisheries, only catch share fisheries, or only non-catch share fisheries.'
                            } else {
                            message = 'For each region or processor size class, you can select to show activities across all production, only groundfish production, or only other species production.'
                            })
})
observeEvent(input$iem, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Variable cost net revenue is revenue minus variable costs. Total cost net revenue is revenue minus fixed and total costs. Further information on the economic measures can be found in the Definitions page.'
  )
})
observeEvent(input$ivs, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'We provide the options to view activities for individual fisheries and activities combined over a group of fisheries. For example, the option All catch share fisheries shows the combined activies across the five catch share fisheries.')
})
observeEvent(input$iVesSum, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'For all vessels that fished within selected fisheries, show data for activities either within the selected fisheries, across all catch share fisheries, or across all West Coast fisheries. ')
})
observeEvent(input$icompare, {
  session$sendCustomMessage(type= 'testmessage',
                            if(input$Sect_sel=="CV"){
                            message = 'You are selecting to either 1) look at a single metric for vessels grouped by fisheries, states, homeports, or vessel length or 2) compare multiple metrics for a single fishery, state, homeport, or vessel length. When comparing multiple metrics, results do not include data from activites in Alaskan fisheries.'
                            } else {
                              message = 'You are selecting to either 1) look at a single metric for processors grouped by production activities, region, or processor size or 2) compare multiple metrics for a single production activities, region, or processor size.'
                            }
                            )
                            })
observeEvent(input$ivariance, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'When MEAN is selected, we show one standard deviation about the mean, When MEDIAN is selected, we show the upper and lower quartiles (25th/75th percentiles). We use algorithm 8 from Hyndman and Fan (1996), which is particularly suited to non-normally distributed data. m=(p+1)/3. p_k = (k - 1/3)/(n+1/3). Then p_k =~ median[F(x_k)]. ')
})
observeEvent(input$iwhiting, {
  session$sendCustomMessage(type = 'testmessage'#'JsonObject'
                            ,message= 'See the Definitions Page for a diagram of how vessels are divided into whiting or non-whiting categories.'#
                             # ('www/WhitingFigure.png',200,150,'alt text')
                            )
})
##bsPopover('iwhiting', 'Title', content='test', trigger='click')

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
   else if(!PermitPlot()) return()
      if(PermitPlot()) #& input$Ind_sel!="Economic")
    {
#   if(input$MetricSelect=="revpcrewday"|input$MetricSelect=="wage"){
#      doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000")}  
#   else if(input$MetricSelect=="Share of landings by state"){
#     doPlot(dat = DatSub(), x = "YEAR", y = "VALUE*100")
# }  else { 
        doPlot(dat = DatSub(), x = "YEAR", y = "VALUE")
               
      }
#else if(PermitPlot() & input$Ind_sel=="Economic"){
#   doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000")}
 },  height=scale_height, width = "auto")

output$PlotMain2 <- renderPlot({
  input$data
  doPlot(dat = DatSub(), x = "YEAR", y = "VALUE")
},  height=400, width = 700)


output$PlotMain3 <- renderPlot({
  input$data
  doPlot(dat = DatSub(), x = "YEAR", y = "VALUE")
},  height=400, width = 700)

output$TableMain <- renderDataTable({  
  input$data
  if(vars$counter%%2 != 0) return()
  else if(!PermitPlot()) return()
    if(!is.null(DatSubTable())) {
      if(input$LayoutSelect=="Metrics"){
        if(input$Ind_sel=="Economic"){
              table <- subset(DatSubTable(), select = -c(CATEGORY))
              table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
              #table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              if(input$CategorySelect == "Fisheries"){
                table <- subset(table, select = -c(CS))
                if(input$Sect_sel =="FR"){
                  names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Data summed\nacross", "Number of processors","Value",  "Variance \n\n(Quartiles or SD)")
                } else {
                  names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(Quartiles or SD)")
                }
              }  else {
                if(input$Sect_sel =="FR"){
              names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Economic measure", "Data summed\nacross","Number of processors", "Value", "Variance \n(Quartiles or SD)")
              } else {
                names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Data summed\nacross","Number of vessels", "Value", "Variance \n(Quartiles or SD)")
                }
                    }
        } #End Economic
        else {
          if(input$CategorySelect == "Fisheries"){
            table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
            table <- subset(table, is.na(table$VALUE)==F)
            if(input$Sect_sel == "FR"){
            names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of processors","Value",  "Variance \n\n(Quartiles or SD)")
            } else {
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of vessels","Value",  "Variance \n\n(Quartiles or SD)")
            }
            } else{
            table <- subset(DatSubTable(), select = -c(CATEGORY))
            table <- subset(table, is.na(table$VALUE)==F)
            if(input$Sect_sel =="FR"){
              names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed\nacross","Number of processors","Value",  "Variance \n\n(Quartiles or SD)")
            } else {
              names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross","Number of vessels","Value",  "Variance \n\n(Quartiles or SD)")
            }
            }
        }
      } # End compare metrics
      #Begin Compare vessles
      else {
      
      if(input$Ind_sel=="Economic"){
          table <- subset(DatSubTable(), select = -CATEGORY)  
          table$VALUE <- paste('$', prettyNum(table$VALUE,  big.mark = ",", format = 'f', digits = 5, trim=T))
          table$VARIANCE <- paste('$', prettyNum(table$VARIANCE,  big.mark = ",", format = 'f', digits = 5, trim=T))
          table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
      if(input$Sect_sel=="CV"&input$CategorySelect != "Fisheries"){
        names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Data summed\nacross","Number of vessels", "Value", "Variance \n(Quartiles or SD)")
        }
       else if(input$Sect_sel=="FR"&input$CategorySelect != "Fisheries"){
          names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Economic measure", "Data summed\nacross","Number of processors", "Value", "Variance \n(Quartiles or SD)")
      }else if(input$Sect_sel=="FR"&input$CategorySelect == "Fisheries"){
        table <- subset(table, select = -c(CS))
        names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure", "Data summed\nacross","Number of processors", "Value", "Variance \n(Quartiles or SD)")
      } else {
          table <- subset(table, select = -c(CS))
                    names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(Quartiles or SD)")
      }
      } #end economic for non-metrics comparison
      else if(input$Ind_sel=="Other"){
          table <- subset(DatSubTable(), select = -c(CATEGORY)) 
          if(input$CategorySelect == "Fisheries"){
           table <- subset(table, select = -CS)
          table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
            
            if(input$socSelect=="Share of landings by state"){
            table$VALUE <- paste(prettyNum((table$VALUE), big.mark = ",", format = 'f', digits = 5, trim=T), '%')
            names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross",("Number of vessels;\nVessels may deliver\nto multiple locations"), "Value", "Delivery \nlocation")
          } else {
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              if(input$Sect_sel=="FR"){
                names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of processors","Value",  "Variance \n\n(Quartiles or SD)")
              }else
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(Quartiles or SD)")
            }
           } # end fisheries
       
                 else if(input$socSelect=="Share of landings by state"){
            table$VALUE <- paste('%', prettyNum((table$VALUE), big.mark = ",", format = 'f', digits = 5, trim=T))
            table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
            names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross","Number of vessels \nVessels may deliver \nto multiple locations", "Value", "Delivery \nlocation")
          } else {
               table <- subset(DatSubTable(), select = -CATEGORY)    
               table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
               table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
               table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
               if(input$Sect_sel=="FR"){
                 names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed\nacross", "Number of processors","Value",  "Variance \n\n(Quartiles or SD)")
               } else {
               names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(Quartiles or SD)")
               }
              }
             }
        #End Other
        else if(input$Ind_sel == 'Labor') {
          table <- subset(DatSubTable(), select =-c(CATEGORY))
          if(input$CategorySelect == 'Fisheries') {
            table <- subset(table, select = -CS)
            table$YEAR <- factor(table$YEAR, levels = c(min(table$YEAR):max(table$YEAR)))
            if(input$crewSelect %in% c('Crew wage per day', 'Crew wage per year', 'Crew wage per dollar revenue', 'Revenue per crew-day')){
              table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
              table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(Quartiles or SD)")
            } else if(input$Sect_sel != 'FR') {
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(Quartiles or SD)")
              } else {
                table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
                table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
                names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of processors","Value",  "Variance \n\n(Quartiles or SD)")
          }} 
          else if(input$crewSelect %in% c('Crew wage per day', 'Crew wage per year', 'Crew wage per dollar revenue', 'Revenue per crew-day')) {
            table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
            table$VARIANCE <- paste('$',prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
            table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
            names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric", "Data summed\nacross","Number of vessels", "Value", "Variance \n(Quartiles or SD)")
          } else if(input$Sect_sel != 'FR') {
            table <- subset(DatSubTable(), select = -CATEGORY)    
            table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
            table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
            table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
            names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(Quartiles or SD)")
          } else {
            table <- subset(DatSubTable(), select = -CATEGORY)    
            table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
            table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
            table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
            names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(Quartiles or SD)")
          }
        } ## End crew
        
        else if(input$Ind_sel=="Vessel characteristics" ||
                input$Ind_sel == 'Processor characteristics'){
            table <- subset(DatSubTable(), select = -c(CATEGORY))
          if(input$CategorySelect == "Fisheries"){
            table <- subset(table, select = -c(CS))
            if(input$demSelect=="Number of vessels"){
            table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of vessels")
            } else if(input$demSelect=="Number of processors"){
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of processors")
            }  else {
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
                if(input$demSelect=="Revenue diversification"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Number of fisheries"){
                if(input$Sect_sel=="CV"){
                  names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Alaskan fisheries","Number of vessels","Value",  
                                    "Variance \n\n(Quartiles or SD)")
                } else if(input$Sect_sel=="FR"){                
                  names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of processors","Value",  "Variance \n\n(Quartiles or SD)")
                } else {
                  names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of vessels","Value",  "Variance \n\n(Quartiles or SD)")
                }
              }
                  }

          } # end fisheries
          else {
            if(input$demSelect=="Number of vessels"|input$demSelect=="Number of processors"){
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              if(input$Sect_sel!="FR"){
              names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across","Number of vessels")
              } else{
              names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across","Number of processors")
              }
              } else if(input$demSelect=="Revenue diversification"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Number of fisheries"){
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              if(input$Sect_sel=="FR"){
                names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value", 
                                  "Variance \n\n(Quartiles or SD)")
              } else {
                names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Alaskan fisheries","Number of vessels","Value",
                                  "Variance \n\n(Quartiles or SD)")
              }
            }else {
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              if(input$Sect_sel=="FR"){
                names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed\nacross", "Number of processors",'Value',
                                  "Variance \n\n(Quartiles or SD)")
              }else {
              names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  
                                "Variance \n\n(Quartiles or SD)")
            }}
          } 
        }#End Dempgraphic
      }#end compare vessels 
           table
    }
})


# download buttons ------------------------------------------------------------
##---Table-----#
#####
output$dlTable <- downloadHandler(
    filename = function() { 'perfmetricsTable.csv' },
    content = function(file) {
 #Remove columns that do not need to be displayed     
#      if(input$Ind_sel=="Economic"){
          if(input$CategorySelect == "Fisheries"){
              table <- subset(DatSubTable(), select = -c(CATEGORY, CS)) %>% mutate(Sector =input$Sect_sel)
          } else {
              table <- subset(DatSubTable(), select = -CATEGORY)  %>% mutate(Sector =input$Sect_sel) 
          }
      table$Sector <-c('Catcher Vessels','First Receivers and Shorebased Processors','Mothership vessels','Catcher-Processor vessels')[match(table$Sector, c('CV','FR','M','CP'))]
#Rename the columns      
      if(input$LayoutSelect=="Metrics"){
        if(input$Ind_sel=="Economic"){
          if(input$Sect_sel=='CV'&input$CategorySelect!='Fisheries'){
            temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Data summed across","Number of vessels", "Value", "Variance (Quartiles or SD)","Sector")
          } else if(input$Sect_sel=='FR'&input$CategorySelect!='Fisheries'){
            temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Economic measure", "Data summed across","Number of processors", "Value", "Variance (Quartiles or SD)","Sector")
          }else if(input$Sect_sel=='FR'&input$CategorySelect=='Fisheries'){
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure", "Data summed across","Number of processors", "Value", "Variance (Quartiles or SD)","Sector")
          } else {
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure","Data summed across", "Number of vessels","Value",  "Variance (Quartiles or SD)","Sector")
          }
        } #End Economic
        else {
             if(input$Sect_sel=="CV"&input$CategorySelect!='Fisheries'){
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across","Number of vessels","Value",  "Variance (Quartiles or SD)","Sector")
              } else if(input$Sect_sel=="FR"&input$CategorySelect!='Fisheries') {
                temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across","Number of Processors","Value",  "Variance (Quartiles or SD)","Sector")
              }else if(input$Sect_sel=="FR"&input$CategorySelect=='Fisheries') {
                temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of Processors","Value",  "Variance (Quartiles or SD)","Sector")
              }
                else {
              temp <- data.frame("Year", "Summary Variable","Statistic", "Metric","Data summed across","Number of vessels","Value",  "Variance (Quartiles or SD)","Sector")
            }
        } #End Vessel characteristics and Other categories
      } # End compare metrics
      #Begin Compare vessels
      else {
        #Economic
        if(input$Ind_sel=="Economic"){
          if(input$Sect_sel=="CV"&input$CategorySelect != "Fisheries"){
            temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Data summed across","Number of vessels", "Value", "Variance (Quartiles or SD)","Sector")
          }
          else if(input$Sect_sel=="FR"&input$CategorySelect != "Fisheries"){
            temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Economic measure", "Data summed across","Number of processors", "Value", "Variance (Quartiles or SD)","Sector")
          }else if(input$Sect_sel=="FR"&input$CategorySelect == "Fisheries"){
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure", "Data summed across","Number of processors", "Value", "Variance (Quartiles or SD)","Sector")
          } else {
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure","Data summed across", "Number of vessels","Value",  "Variance (Quartiles or SD)","Sector")
          }
        } #end economic for non-metrics comparison
        #Other
        else if(input$Ind_sel=="Other"){
          if(input$CategorySelect == "Fisheries"){
             if(input$socSelect=="Share of landings by state"){
              temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of vessels; Vessels may deliver to multiple locations", "Value",
                                 "Delivery location","Sector")
            } else {
              if(input$Sect_sel=="FR"){
                temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of processors","Value",  "Variance (Quartiles or SD)","Sector")
              }else {
                temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (Quartiles or SD)","Sector")
            }}
          } # end fisheries
          else {
           if(input$socSelect=="Share of landings by state"){
              temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across",
                                 "Number of vessels\nVessels may deliver\nto multiple locations", "Value", "Delivery location","Sector")
            } else {
               if(input$Sect_sel=="FR"){
                 temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across", "Number of processors","Value",  
                                    "Variance (Quartiles or SD)","Sector")
              } else {
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  
                                   "Variance (Quartiles or SD)","Sector")
              }
            }
          }
        } #End Other
        ##Crew
        else if(input$Ind_sel == 'Labor') {
          if(input$CategorySelect == 'Fisheries') {
            if(input$Sect_sel == 'FR') {
              temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of processors","Value",  "Variance (Quartiles or SD)","Sector")
            }else {
              temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (Quartiles or SD)","Sector")
            }
          }
          else if(input$Sect_sel == 'FR') {
            temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across", "Number of processors","Value",  
                               "Variance (Quartiles or SD)","Sector")
          } else {
            temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  
                               "Variance (Quartiles or SD)","Sector")
          }
        }##End crew
        #Vessel characteristics
        else if(input$Ind_sel=="Vessel characteristics" ||
                input$Ind_sel == 'Processor characteristics'){
          if(input$CategorySelect == "Fisheries"){
            if(input$demSelect=="Number of vessels"){
              temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of vessels","Sector")
            } else if(input$demSelect=="Number of processors"){
              temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of processors","Sector")
            }  else {
              if(input$demSelect=="Revenue diversification"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Number of fisheries"){
                if(input$Sect_sel=="CV"){
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Alaskan fisheries","Number of vessels","Value",  
                                     "Variance (Quartiles or SD)","Sector")
                } else if(input$Sect_sel=="FR"){                
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of processors","Value","Variance (Quartiles or SD)","Sector")
                } else {
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of vessels","Value",  "Variance (Quartiles or SD)","Sector")
                }
              }
              else {
                if(input$Sect_sel=="FR"){
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of processors","Value",  "Variance (Quartiles or SD)","Sector")
                } else {
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (Quartiles or SD)","Sector")
                }
              }}
          } # end fisheries
          else {
            if(input$demSelect=="Number of vessels"){
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across","Number of vessels","Sector")
              } else if(input$demSelect=="Number of processors"){
                temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across","Number of processors","Sector")
            } else if(input$demSelect=="Revenue diversification"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Number of fisheries"){
              if(input$Sect_sel=="FR"){
                temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  
                                   "Variance (Quartiles or SD)","Sector")
              } else {
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across", "Alaskan fisheries","Number of vessels","Value", 
                                   "Variance (Quartiles or SD)","Sector")
              }
            }else {
              if(input$Sect_sel=="FR"){
                temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed cross", "Number of processors",'Value',
                                   "Variance (Quartiles or SD)","Sector")
              }else {
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across", "Number of vessels","Value", 
                                   "Variance (Quartiles or SD)","Sector")
              }}
          } 
        }#End Dempgraphic
      }#end compare vessels 
 #Final Formatting code chunk     
      colnames(temp)=colnames(table)
      # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server

table <- rbindCommonCols(temp, table) 
                names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                        format(Sys.Date(), format="%B %d %Y")), rep("", dim(temp)[2]-1))
           write.csv(table, file)
   })
#####

# render plot from  to pdf for download
#######
output$dlFigure <- downloadHandler(
  filename = function() {'perfmetricsPlot.pdf'},
  content = function(file){
     if(!PermitPlot()) return()
   
    pdf(file = file, width=10.25, height=7.5, onefile=T)
    if(length(input$VariableSelect)<=6){
            doPlotDownload(dat = DatSub(), x = "YEAR", y = "VALUE")
    } else {
      dat <- DatSub()
      dat <- subset(dat, VARIABLE %in% input$VariableSelect[1:6])
      doPlotDownload(dat = dat, x="YEAR", y= "VALUE")
      dat <- DatSub()
      dat <- subset(dat, VARIABLE %in% input$VariableSelect[7:length(input$VariableSelect)])
      doPlotDownload(dat = dat, x="YEAR", y= "VALUE")
      }
    dev.off()
 })
#######

#Interactives plots trial
#######
output$hover_info <- renderUI({
  if(!is.null(input$plot_hover)){
    dat <- DatSub()
    if(input$LayoutSelect!='Metrics'&length(input$VariableSelect)==1||
       input$LayoutSelect=='Metrics'&length(input$ShortdescrSelect)==1){
      lvls <- levels(as.factor(dat$YEAR))
    } else {
      lvls <- rep(levels(as.factor(dat$YEAR)),2) 
    }
    hover=input$plot_hover

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <-
      (.125+hover$domain$left) / (hover$domain$right - hover$domain$left)
 #     if(hover$x<.89){
 #         (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
 #     } else {
 #         (hover$x-.2 - hover$domain$left) / (hover$domain$right - hover$domain$left) 
 #       }#
#    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    top_pct <- .15#hover$domain$top
    
    # calculate distance from left and bottom side of the picture in pixels
    #left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style1 <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px-16, #left_px -5,
                    "px; top:", top_px + .1, "px;"
                   )
    
    
#  # actual tooltip created as wellPanel
    ###WITHIN PLOT
    wellPanel(
      style = style1,
      if (lvls[round(length(lvls)*input$plot_hover$x)]==2015) {
    tags$div(tags$p('The Blob and El', HTML("Ni&ntilde;o"), 'converge. Biomass is low.',tags$br(),
             'Council establishes catch limits for unfished and unmanaged forage fish.',tags$br(),
             'Canary rockfish and petrale sole are declared rebult.', tags$br(),
             'Dover sole and thornyhead limits increased.'))
      } else if(lvls[round(length(lvls)*input$plot_hover$x)]==2014) {
        tags$div('Fishery participants can buy and sell quota shares.', tags$br(), 
                 'Non-whiting groundfish gets MSC certification',tags$br(),
                 'Russia implements ongoing trade sanctions.')  
      }else if(lvls[round(length(lvls)*input$plot_hover$x)]==2013) {
        paste0('Ban on year-end quota pound transfers is lifted.')  
      }else if(lvls[round(length(lvls)*input$plot_hover$x)]==2012) {
        paste0('Widow rockfish is declared rebuilt.')  
      }else if(lvls[round(length(lvls)*input$plot_hover$x)]==2011) {
        tags$div(HTML("T&#333;hoku earthquake and tsunami hits Japan."), tags$br(), 
                 "Sablefish prices are high.", tags$br(), 
                 "Fishery participants can lease quota pounds.")  
      }else if(lvls[round(length(lvls)*input$plot_hover$x)]==2010) {
        paste0('Petrale sole is declared overfished.')  
      }else if(lvls[round(length(lvls)*input$plot_hover$x)]==2009) {
        tags$div('Sector-specific quota allocation for bycatch species.', tags$br(),
        'Pacific whiting gets MSC certification.')
      }
    )
  }
})

    
    #Interactives plots trial
    #######
    output$click_info <- renderUI({
      if(!is.null(input$plot_click)){
        dat <- DatSub()
        if(input$LayoutSelect!='Metrics'&length(input$VariableSelect)==1||
           input$LayoutSelect=='Metrics'&length(input$ShortdescrSelect)==1){
          lvls <- levels(as.factor(dat$YEAR))
        } else {
          lvls <- rep(levels(as.factor(dat$YEAR)),2) 
        }
        click=input$plot_click
        
        #  # actual tooltip created as wellPanel
        ####BELOW PLOT 
    wellPanel(
      if (lvls[round(length(lvls)*input$plot_click$x)]==2015) {
        tags$div(tags$p('The Blob (first detected in 2013) and El', HTML("Ni&ntilde;o"), 'converge, leading to record low biomass of key prey species for many catch share species; catch attainment is lower for Pacific whiting.', tags$a(href='http://www.nationalgeographic.com/magazine/2016/09/warm-water-pacific-coast-algae-nino/', 'Click here for more details.', target='_blank'),tags$br(),
                        'Council establishes catch limits for unfished and unmanaged forage fish.
                        Dover sole limits increased by over 100% and Longspine thornyhead limits increased by 60%.'))
      } else if(lvls[round(length(lvls)*input$plot_click$x)]==2014) {
        tags$div('Fishery participants can buy and sell quota shares.', tags$br(), 
                 'Russia implements an import ban on agricultural and processed food exports from the USA, EU, Norway, Canada, and Australia.', 
                  tags$a(href='https://www.nytimes.com/2014/08/08/world/europe/russia-sanctions.html','Read this NYTimes article for background.', target='_blank'), 
                  'The ban is still in place.
                 According to a', tags$a(href='http://trade.ec.europa.eu/doclib/docs/2015/december/tradoc_154025.pdf' , 'paper in Chief Economist Note,', target='_blank'), 
                 'the impact of the ban on the US fisheries industry has been minimal.', tags$br(),
                 'Non-whiting groundfish gets Marine Stewardship Council certification.')  
      }else if(lvls[round(length(lvls)*input$plot_click$x)]==2013) {
        paste0('Ban on year-end quota pound transfers is lifted')  
      }else if(lvls[round(length(lvls)*input$plot_click$x)]==2012) {
        paste0('Widow rockfish, a species that was constraining co-occurring target species, is declared rebuilt.')  
      }else if(lvls[round(length(lvls)*input$plot_click$x)]==2011) {
        tags$div(HTML("T&#333;hoku earthquake and tsunami hits Japan."), tags$br(), 
                 "Sablefish prices are high.", tags$br(), 
                 "Fishery participants can lease quota pounds but cannot yet purchase quota shares.")  
      }else if(lvls[round(length(lvls)*input$plot_click$x)]==2010) {
        paste0('Petrale sole is declared overfished.')  
      }else if(lvls[round(length(lvls)*input$plot_click$x)]==2009) {
        paste0('Sector-specific bycatch species quota allocation ends race-to-fish for constraining species between at-sea motherships.
               Pacific whiting gets MSC certification.')  
      }
    )
    
  }
})
#######
#End interactive plots code