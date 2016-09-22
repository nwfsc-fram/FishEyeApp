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
observeEvent(input$ipo, {
  session$sendCustomMessage(type = 'testmessage',
                            if(input$demSelect=="Number of vessels"){
                              message = 'Number of vessels actively participating (i.e., had an active permit and non-zero revenue).'
                            } else if(input$demSelect=="Vessel length"){
                              message = 'The length (ft) of vessels reflects the mix of active participants in the fishery in each year.'
                            }else if(input$demSelect=="Fishery participation"){
                              message = 'Count of fisheries that vessels participated in. Changes may indicate specialization or diversification.'
                            }else if(input$demSelect=="Exponential Shannon Index"){
                              message = 'Measures the income diversification of a vessel across revenue sources. A larger number corresponds to increased diversification. Changes may indicate specialization or diversification.'
                            }else if(input$demSelect=="Proportion of revenue from CS fishery"){
                              message = "The average proportion of a vessel's total revenue that comes from fish caught in the limited entry trawl or catch share fishery measures how reliant vessels are on revenue from the limited entry/catch shares fishery."
                            }else if(input$demSelect=="Days at sea"){
                              message = 'The number of days at sea may indicate specialization, efficiency, or consolidation.'
                            }else if(input$socSelect=="Number of positions"){
                              message = 'Number of positions (including captain and crew) is a lower bound for employment in the fishery, and is affected by positions per vessel and the number of vessels fishing.'
                            }else if(input$demSelect=="Gini coefficient"){
                              message = 'Measures the degree of catch share revenue concentration among vessels. A value of zero would represent all vessels earning the same revenue, and a value of one would represent one vessel earning all of the revenue. The value of the Gini coefficient can be affected by fleet consolidation and specialization.'
                            }else if(input$SocSelect=="Crew wage per day"){
                              message = 'Daily wage paid to a crewmember operating in the limited entry/catch shares fishery.'
                            }else if(input$SocSelect=="Revenue per crew day"){
                              message = 'Revenue divided by crew day, where crew days are calculated as days at sea multipled by number of crew per vessel. This metric is a measure of productivity (in terms of revenue generation) of a crew member.'
                            }else if(input$SocSelect=="Seasonality"){
                              message = 'The date (day of year, Jan. 1 = 1) on which 50% of the total volume of catch was landed in the fishery. Metric measures broad-scale changes in the seasonality of fishing for catch shares fish. It can also indicate changes in total allowable catch (TAC); it may take the fleet longer to catch a higher TAC/ACL.'
                            }else if(input$SocSelect=="Share of landings by state"){
                              message = 'Share of total landings, share of landings by whiting vessels, and share of landings by non-whiting groundfish vessels in each state or at sea. Shares are in terms of revenue.'
                            }else if(input$demSelect=="Herfindahl-Hirschman Index"){
                              message = 'The Herfindahl-Hirschman Index is a measure of market concentration. It is calculated as the sum of the squares of the market shares, defined here by the share of the catch share revenues earned by each vessel. Values range from 0 to 10,000. Lower values indicate low market concentration (and higher competition).'
                            })
})

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
                            message = 'Variable cost net revenue is revenue minus variable costs. Total cost net revenue is revenue minus fixed and total costs. Further information on the economic measures can be found in the Definitions page.'
  )
})

observeEvent(input$ivs, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'We provide the options to view activities for individual fisheries and activities combined over a group of fisheries. For example, the option All catch share fisheries shows the combined activies across the five catch share fisheries.')
})


observeEvent(input$iVesSum, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'For all vessels that fished within selected fisheries, show data for activities either within the selected fisheries, across all Catch Share fisheries, or across all West Coast fisheries. ')
})

observeEvent(input$icompare, {
  session$sendCustomMessage(type= 'testmessage',
                            message = 'You are selecting to either 1) look at a single metric for vessels grouped by fisheries, states, homeports, or vessel length or 2) compare multiple metrics for a single fishery, state, homeport, or vessel length.')
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
  
 if(PermitPlot() #& input$Ind_sel!="Economic"
    ){
#   if(input$MetricSelect=="revpcrewday"|input$MetricSelect=="wage"){
#      doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000")}  
#   else if(input$MetricSelect=="Share of landings by state"){
#     doPlot(dat = DatSub(), x = "YEAR", y = "VALUE*100")
# }  else { 
        doPlot(dat = DatSub(), x = "YEAR", y = "VALUE")}
#      }
#else if(PermitPlot() & input$Ind_sel=="Economic"){
#   doPlot(dat = DatSub(), x = "YEAR", y = "VALUE/1000")}
 },  height=scale_height, width = "auto")


output$TableMain <- renderDataTable({  
  input$data
  if(vars$counter%%2 != 0) return()
  else if(!PermitPlot()) return()
    if(!is.null(DatSubTable())) {
      if(input$LayoutSelect=="Metrics"){
        if(input$Ind_sel=="Economic"){
          if(input$Sect_sel=="CV"){
            if(input$CategorySelect == "Fisheries"){
              table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
              table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
              table$VARIANCE <- paste('$', prettyNum(table$VARIANC, big.mark = ",", format = 'f', digits = 5, trim=T))
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            } else {
              table <- subset(DatSubTable(), select = -CATEGORY)  
              table$VALUE <- paste('$', prettyNum(table$VALUE,  big.mark = ",", format = 'f', digits = 5, trim=T))
              table$VARIANCE <- paste('$', prettyNum(table$VARIANCE,  big.mark = ",", format = 'f', digits = 5, trim=T))
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Data summed\nacross","Number of vessels", "Value", "Variance \n(MAD, SD)")
            }
          } else {
            table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
            table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
            table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
            table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
            names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
          }
        } else {
          if(input$Sect_sel=="CV"){
            if(input$CategorySelect == "Fisheries"){
            table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
            table <- subset(table, is.na(table$VALUE)==F)
            names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
          } else{
            table <- subset(DatSubTable(), select = -c(CATEGORY))
            table <- subset(table, is.na(table$VALUE)==F)
            names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
          }}
          
        }
      } else {
      
      if(input$Ind_sel=="Economic"){
      if(input$Sect_sel=="CV"&input$CategorySelect == "Fisheries"){
          table <- subset(DatSubTable(), select = -CATEGORY)  
          table$VALUE <- paste('$', prettyNum(table$VALUE,  big.mark = ",", format = 'f', digits = 5, trim=T))
          table$VARIANCE <- paste('$', prettyNum(table$VARIANCE,  big.mark = ",", format = 'f', digits = 5, trim=T))
          table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
          names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Data summed\nacross","Number of vessels", "Value", "Variance \n(MAD, SD)")
      } else {
          table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
          table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
          table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
          table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
          names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
      }
      } #end economoic for non-metrrics comparison
      else if(input$Ind_sel=="Social and Regional"){
        if(input$CategorySelect == "Fisheries"){
          if(input$socSelect=="Revenue per crew day"|input$socSelect=="Crew wage per day"){
            table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
            table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
            table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
            table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
            names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
          } else if(input$socSelect=="Share of landings by state"){
            
            table <- subset(DatSubTable(), select = -c(CATEGORY, CS))  
            table$VALUE <- paste(prettyNum((table$VALUE), big.mark = ",", format = 'f', digits = 5, trim=T), '%')
            table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
            names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of vessels", "Value", "Delivery \nlocation")
          } else {
              table <- subset(DatSubTable(), select = -c(CATEGORY,CS))
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            }
           } # end fisheries
        else {
             if(input$socSelect=="Revenue per crew day"|input$socSelect=="Crew wage per day"){
               table <- subset(DatSubTable(), select = -CATEGORY)  
                table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
                table$VARIANCE <- paste('$',prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
                table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
                  names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric", "Data summed\nacross","Number of vessels", "Value", "Variance \n(MAD, SD)")
             } else if(input$socSelect=="Share of landings by state"){
          
            table <- subset(DatSubTable(), select = -CATEGORY)  
            table$VALUE <- paste('%', prettyNum((table$VALUE), big.mark = ",", format = 'f', digits = 5, trim=T))
            table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
            names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross","Number of vessels", "Value", "Delivery \nlocation")
          } else {
               table <- subset(DatSubTable(), select = -CATEGORY)    
               table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
               table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
               table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
               names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
             }
                  }}
        
        else if(input$Ind_sel=="Demographic"){
          if(input$CategorySelect == "Fisheries"){
            if(input$demSelect=="Number of vessels"){
              table <- subset(DatSubTable(), select = -c(CATEGORY, CS))  
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of vessels")
            }  else if(input$demSelect=="Exponential Shannon Index"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Fishery participation"){
              table <- subset(DatSubTable(), select = -c(CATEGORY,CS))
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              if(input$Sect_sel=="CV"){
                names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Alaskan fisheries","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
              } else{
                names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
              }
            }else if(input$demSelect=="Days at sea"){
              table <- subset(DatSubTable(), select = -c(CATEGORY,CS))
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Alaskan fisheries","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            } else {
              table <- subset(DatSubTable(), select = -c(CATEGORY,CS))
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            }
          } # end fisheries
          else {
            if(input$demSelect=="Number of vessels"){
              table <- subset(DatSubTable(), select = -CATEGORY)  
              #            table$VALUE <- paste('%', prettyNum((table$VALUE), big.mark = ",", format = 'f', digits = 5, trim=T))
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Vessel level","Number of vessels")
            } else if(input$demSelect=="Exponential Shannon Index"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Fishery participation"|input$demSelect=="Days at sea"){
              table <- subset(DatSubTable(), select = -CATEGORY)    
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              if(input$Sect_sel=="FR"){
                names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
              } else {
                names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Alaskan fisheries","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
              }
            }else {
              table <- subset(DatSubTable(), select = -CATEGORY)    
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            }
          } 
        }#End Dempgraphic
      }#end compare vessels 
           table
    }
})


# download buttons ------------------------------------------------------------
output$dlTable <- downloadHandler(
    filename = function() { 'perfmetricsTable.csv' },
    content = function(file) {

     # table <- DatSubTable()
      if(input$Ind_sel=="Economic"){
        if(input$CategorySelect == "Fisheries"){
          table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
         } else {
          table <- subset(DatSubTable(), select = -CATEGORY)  
        }
      }
      
      if(input$Ind_sel!="Economic"){
        if(input$CategorySelect == "Fisheries"){
          table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
          } 
        else {
            table <- subset(DatSubTable(), select = -CATEGORY)  
        }
      }
  
      if(input$LayoutSelect=="Metrics"){
        if(input$CategorySelect == "Fisheries"){
        names(table) <- c(4,1,3,2,"a ", " b", "c ", "d ") 
        } else {
          names(table) <- c(4,1,3,2,"a ", " b","c ", "d ", "e ") 
        }
      } else { 
  if(input$CategorySelect == "Fisheries"){
        if(input$Ind_sel=="Demographic"&input$demSelect=="Number of vessels"){
          names(table) <- c(4,1,3,2,"a ", " b")
        }  else if(input$Ind_sel=="Demographic"&input$demSelect=="Exponential Shannon Index"|input$Ind_sel=="Demographic"&input$demSelect=="Proportion of revenue from CS fishery"|input$Ind_sel=="Demographic"&input$demSelect=="Fishery participation"|input$Ind_sel=="Demographic"&input$demSelect=="Days at sea"){
          names(table) <- c(4,1,3,2,"a ", " b", "c ", "d "," e")
        }else {
          names(table) <- c(4,1,3,2,"a ", " b", "c ", "d ")
        }
      } 
      else {
        if(input$Ind_sel=="Demographic"&input$demSelect=="Number of vessels"){
          names(table) <- c(4,1,3,2,"a ", " b","c ")
        } else if(input$Ind_sel=="Demographic"&input$LayoutSelect!="Metrics"&input$demSelect=="Exponential Shannon Index"|input$Ind_sel=="Demographic"&input$LayoutSelect!="Metrics"&input$demSelect=="Proportion of revenue from CS fishery"|input$Ind_sel=="Demographic"&input$LayoutSelect!="Metrics"&input$demSelect=="Fishery participation"|input$Ind_sel=="Demographic"&input$LayoutSelect!="Metrics"&input$demSelect=="Days at sea"){
          names(table) <- c(4,1,3,2,"a ", " b","c ", "d ", "e ","f ")
        }else {
          names(table) <- c(4,1,3,2,"a ", " b","c ", "d ", "e ")
        }
      }
      }
      # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server
      
      if(input$Ind_sel=="Economic"){
        if(input$LayoutSelect=="Metrics"){
          if(input$CategorySelect == "Fisheries"){
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure","Fished for whiting","Number of vessels","Value",  "Variance (MAD, SD)")
          } else {
            temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure","Number of vessels", "Value", "Variance (MAD, SD)")
          }  
        } else {
        if(input$CategorySelect == "Fisheries"){
          temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure","Fished for whiting", "Number of vessels","Value",  "Variance (MAD, SD)")
        } else {
          temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Fished for whiting","Number of vessels", "Value", "Variance (MAD, SD)")
        }
        }
      }
      
      if(input$Ind_sel!="Economic"){
        if(input$CategorySelect == "Fisheries"){
          table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
          if(input$LayoutSelect=="Metrics"){
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric", "Fished for whiting","Number of vessels","Value",  "Variance (MAD, SD)")
          } else {
          if(input$Ind_sel=="Social and Regional"&input$socSelect=="Revenue per crew day"|input$Ind_sel=="Social and Regional"&input$socSelect=="Crew wage per day"){
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Fished for whiting", "Number of vessels","Value",  "Variance (MAD, SD)")
          } else if(input$Ind_sel=="Social and Regional"&input$socSelect=="Share of landings by state"){
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Fished for whiting","Number of vessels", "Value", "Delivery location")
          } else if(input$Ind_sel=="Demographic"&input$demSelect=="Number of vessels"){
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Fished for whiting","Number of vessels")
          }  else if(input$Ind_sel=="Demographic"&input$demSelect=="Exponential Shannon Index"|input$Ind_sel=="Demographic"&input$demSelect=="Proportion of revenue from CS fishery"|input$Ind_sel=="Demographic"&input$demSelect=="Fishery participation"|input$Ind_sel=="Demographic"&input$demSelect=="Days at sea"){
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Fished for whiting", "Alaskan fisheries activities","Number of vessels","Value",  "Variance (MAD, SD)")
          }else {
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Fished for whiting", "Number of vessels","Value",  "Variance (MAD, SD)")
          }
          }#End not metrics
          }#End fisheries
        else {
          if(input$LayoutSelect=="Metrics"){
            temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric", "Fished for whiting","Number of vessels","Value",  "Variance (MAD, SD)")
          } else {
          if(input$Ind_sel=="Social and Regional"&input$socSelect=="Revenue per crew day"|input$Ind_sel=="Social and Regional"&input$socSelect=="Crew wage per day"){
            temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric", "Fished for whiting","Number of vessels", "Value", "Variance (MAD, SD)")
          } else if(input$Ind_sel=="Social and Regional"&input$socSelect=="Share of landings by state"){
            temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Fished for whiting","Number of vessels", "Value", "Delivery location")
          } else if(input$Ind_sel=="Demographic"&input$demSelect=="Number of vessels"){
            temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Fished for whiting","Number of vessels")
          } else if(input$Ind_sel=="Demographic"&input$demSelect=="Exponential Shannon Index"|input$Ind_sel=="Demographic"&input$demSelect=="Proportion of revenue from CS fishery"|input$Ind_sel=="Demographic"&input$demSelect=="Fishery participation"|input$Ind_sel=="Demographic"&input$demSelect=="Days at sea"){
            temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Fished for whiting", "Alaskan fisheries activities","Number of vessels","Value",  "Variance (MAD, SD)")
          }else {
            temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Fished for whiting", "Number of vessels","Value",  "Variance (MAD, SD)")
          }
        }
      }}
        
      
      colnames(temp)=colnames(table)

            table <- rbindCommonCols(temp, table) 
            if(input$LayoutSelect=="Metrics"){
              if(input$CategorySelect == "Fisheries"){
                names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                        format(Sys.Date(), format="%B %d %Y")),"", "", "", "","", "")
              } else {
                names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                        format(Sys.Date(), format="%B %d %Y")),"", "", "", "","", "","")
              }
            } else {
        if(input$CategorySelect == "Fisheries"){
        if(input$Ind_sel=="Demographic"&input$demSelect=="Number of vessels"){
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "", "", "","")
        }  else if(input$Ind_sel=="Demographic"&input$demSelect=="Exponential Shannon Index"|input$Ind_sel=="Demographic"&input$demSelect=="Proportion of revenue from CS fishery"|input$Ind_sel=="Demographic"&input$demSelect=="Fishery participation"|input$Ind_sel=="Demographic"&input$demSelect=="Days at sea"){
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "", "", "","", "","")
        }else {
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "", "", "","", "","")
        }
      } 
      else {
        if(input$Ind_sel=="Demographic"&input$demSelect=="Number of vessels"){
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "","","","", "")
        } else if(input$Ind_sel=="Demographic"&input$demSelect=="Exponential Shannon Index"|input$Ind_sel=="Demographic"&input$demSelect=="Proportion of revenue from CS fishery"|input$Ind_sel=="Demographic"&input$demSelect=="Fishery participation"|input$Ind_sel=="Demographic"&input$demSelect=="Days at sea"){
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "","", "", "","", "","")
        }else {
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "","", "", "","", "","")
        }
      }
            }
           write.csv(table, file)
   })

# render plot from  to pdf for download
output$dlFigure <- downloadHandler(
  filename = function() {'perfmetricsPlot.pdf'},
  content = function(file){
     if(!PermitPlot()) return()
    pdf(file = file, width=10.25, height=7.5, onefile=F)
            doPlotDownload(dat = DatSub(), x = "YEAR", y = "VALUE")
    dev.off()
 })

