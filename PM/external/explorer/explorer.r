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
                            }else if(input$demSelect=="Gini coefficient"){
                              message = 'Measures the degree of catch share revenue concentration among vessels. A value of zero would represent all vessels earning the same revenue, and a value of one would represent one vessel earning all of the revenue. The value of the Gini coefficient can be affected by fleet consolidation and specialization.'
                            }else if(input$socSelect=="Number of positions"){
                              message = 'Number of positions (including captain and crew) is a lower bound for employment in the fishery, and is affected by positions per vessel and the number of vessels fishing.'
                            }else if(input$SocSelect=="Crew wage per day"){
                              message = 'Daily wage paid to a crewmember operating in the limited entry/catch shares fishery.'
                            }else if(input$SocSelect=="Revenue per crew day"){
                              message = 'Revenue divided by crew day, where crew days are calculated as days at sea multipled by number of crew per vessel. This metric is a measure of productivity (in terms of revenue generation) of a crew member.'
                            }else if(input$SocSelect=="Seasonality"){
                              message = 'The date (day of year, Jan. 1 = 1) on which 50% of the total volume of catch was landed in the fishery. Metric measures broad-scale changes in the seasonality of fishing for catch shares fish. It can also indicate changes in total allowable catch (TAC); it may take the fleet longer to catch a higher TAC/ACL.'
                            }else if(input$SocSelect=="Share of landings by state"){
                              message = 'Share of total landings, share of landings by whiting vessels, and share of landings by non-whiting groundfish vessels in each state or at sea. Shares are in terms of revenue.'
                            }#else if(input$demSelect=="Herfindahl-Hirschman Index"){
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
              table <- subset(DatSubTable(), select = -c(CATEGORY))
              table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
              table$VARIANCE <- paste('$', prettyNum(table$VARIANC, big.mark = ",", format = 'f', digits = 5, trim=T))
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              if(input$CategorySelect == "Fisheries"){
                table <- subset(table, select = -c(CS))
                if(input$Sect_sel!="CV"){
                  names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Data summed\nacross", "Number of processors","Value",  "Variance \n\n(MAD, SD)")
                } else {
                  names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
                }
              } else {
                if(input$Sect_sel!="CV"){
              names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Economic measure", "Data summed\nacross","Number of processors", "Value", "Variance \n(MAD, SD)")
              } else {
                names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Data summed\nacross","Number of vessels", "Value", "Variance \n(MAD, SD)")
                }
                    }
        } #End Economic
        else {
          if(input$CategorySelect == "Fisheries"){
            table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
            table <- subset(table, is.na(table$VALUE)==F)
            if(input$Sect_sel!="CV"){
            names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of processorss","Value",  "Variance \n\n(MAD, SD)")
            } else {
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of vessel","Value",  "Variance \n\n(MAD, SD)")
            }
            } else{
            table <- subset(DatSubTable(), select = -c(CATEGORY))
            table <- subset(table, is.na(table$VALUE)==F)
            if(input$Sect_sel=="CV"){
            names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            } else {
              names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed\nacross","Number of Processors","Value",  "Variance \n\n(MAD, SD)")
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
        names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Data summed\nacross","Number of vessels", "Value", "Variance \n(MAD, SD)")
        }
       else if(input$Sect_sel=="FR"&input$CategorySelect != "Fisheries"){
          names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Economic measure", "Data summed\nacross","Number of processors", "Value", "Variance \n(MAD, SD)")
      }else if(input$Sect_sel=="FR"&input$CategorySelect == "Fisheries"){
        table <- subset(table, select = -c(CS))
        names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure", "Data summed\nacross","Number of processors", "Value", "Variance \n(MAD, SD)")
      } else {
          table <- subset(table, select = -c(CS))
                    names(table) <- c("Year", "Summary Variable", "Statistic", "Economic measure","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
      }
      } #end economic for non-metrics comparison
      else if(input$Ind_sel=="Social and Regional"){
          table <- subset(DatSubTable(), select = -c(CATEGORY)) 
          if(input$CategorySelect == "Fisheries"){
           table <- subset(table, select = -CS)
          table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
            if(input$socSelect=="Revenue per crew day"|input$socSelect=="Crew wage per day"){
            table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
            table$VARIANCE <- paste('$', prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
              if(input$Sect_sel=="FR"){
                names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of processors","Value",  "Variance \n\n(MAD, SD)")
              } else {
                names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
              } 
            }else if(input$socSelect=="Share of landings by state"){
            table$VALUE <- paste(prettyNum((table$VALUE), big.mark = ",", format = 'f', digits = 5, trim=T), '%')
            names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of vessels", "Value", "Delivery \nlocation")
          } else {
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              if(input$Sect_sel=="FR"){
                names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of processors","Value",  "Variance \n\n(MAD, SD)")
              }else
              names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            }
           } # end fisheries
        else {
             if(input$socSelect=="Revenue per crew day"|input$socSelect=="Crew wage per day"){
                table$VALUE <- paste('$', prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T))
                table$VARIANCE <- paste('$',prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T))
                table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
                if(input$Sect_sel=="FR"){
                  names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Metric", "Data summed\nacross","Number of processors", "Value", "Variance \n(MAD, SD)")
                }else {
                  names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric", "Data summed\nacross","Number of vessels", "Value", "Variance \n(MAD, SD)")
             }
                } else if(input$socSelect=="Share of landings by state"){
            table$VALUE <- paste('%', prettyNum((table$VALUE), big.mark = ",", format = 'f', digits = 5, trim=T))
            table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
            names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross","Number of vessels", "Value", "Delivery \nlocation")
          } else {
               table <- subset(DatSubTable(), select = -CATEGORY)    
               table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
               table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
               table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
               if(input$Sect_sel=="FR"){
                 names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed\nacross", "Number of processors","Value",  "Variance \n\n(MAD, SD)")
               } else {
               names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
               }
              }
             }
        } #End social and regional
        
        else if(input$Ind_sel=="Demographic"){
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
                if(input$demSelect=="Exponential Shannon Index"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Fishery participation"){
                if(input$Sect_sel=="CV"){
                  names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Alaskan fisheries","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
                } else if(input$Sect_sel=="FR"){                
                  names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of processors","Value",  "Variance \n\n(MAD, SD)")
                } else {
                  names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
                }
              }else if(input$demSelect=="Days at sea"){
                if(input$Sect_sel!="FR"){
                 names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Alaskan fisheries","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
              } else {
                names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of processors","Value",  "Variance \n\n(MAD, SD)")
              }}
                else {
                  if(input$Sect_sel=="FR"){
                names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of processors","Value",  "Variance \n\n(MAD, SD)")
                  } else {
                    names(table) <- c("Year", "Summary Variable", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
                  }
                }}
          } # end fisheries
          else {
            if(input$demSelect=="Number of vessels"|input$demSelect=="Number of processors"){
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              if(input$Sect_sel!="FR"){
              names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across","Number of vessels")
              } else{
              names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across","Number of processors")
              }
              } else if(input$demSelect=="Exponential Shannon Index"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Fishery participation"|input$demSelect=="Days at sea"){
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              if(input$Sect_sel=="FR"){
                names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
              } else {
                names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Alaskan fisheries","Number of vessels","Value",  "Variance \n\n(MAD, SD)")
              }
            }else {
              table$VALUE <- prettyNum(table$VALUE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$VARIANCE <- prettyNum(table$VARIANCE, big.mark = ",", format = 'f', digits = 5, trim=T)
              table$YEAR <- factor(table$YEAR, levels=c(min(table$YEAR):max(table$YEAR)))
              if(input$Sect_sel=="FR"){
                names(table) <- c("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed\nacross", "Number of processors",'Value',"Variance \n\n(MAD, SD)")
              }else {
              names(table) <- c("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed\nacross", "Number of vessels","Value",  "Variance \n\n(MAD, SD)")
            }}
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
    
      
      if(input$Ind_sel=="Economic"){
          if(input$CategorySelect == "Fisheries"){
              table <- subset(DatSubTable(), select = -c(CATEGORY, CS)) %>% mutate(Sector =input$Sect_sel)
          } else {
              table <- subset(DatSubTable(), select = -CATEGORY)  %>% mutate(Sector =input$Sect_sel) 
          }
      } else if(input$Ind_sel!="Economic"){
          if(input$CategorySelect == "Fisheries"){
            table <- subset(DatSubTable(), select = -c(CATEGORY, CS)) %>% mutate(Sector =input$Sect_sel)
          } 
          else {
              table <- subset(DatSubTable(), select = -CATEGORY) %>% mutate(Sector =input$Sect_sel)  
          }
      }
      table$Sector <-c('Catcher Vessels','First Receivers and Shorebased Processors','Mothership vessels','Catcher-Processor vessels')[match(table$Sector, c('CV','FR','M','CP'))]
      
      # some wonky code to insert a timestamp. xtable has a more straightfoward approach but not supported with current RStudio version on the server
      if(input$LayoutSelect=="Metrics"){
        if(input$Ind_sel=="Economic"){
           if(input$Sect_sel=="CV"|input$Sect_sel=="FR"){
            if(input$CategorySelect == "Fisheries"){
             if(input$Sect_sel=="CV"){
                temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)","Sector")
              } else {
                temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure","Data summed across", "Number of processors","Value",  "Variance (MAD, SD)","Sector")
              }
            } else {
              if(input$Sect_sel=="CV"){
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Data summed across","Number of vessels", "Value", "Variance (MAD, SD)","Sector")
              } else {
                temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Economic measure", "Data summed across","Number of processors", "Value", "Variance (MAD, SD)","Sector")
              }
            }
          } else {
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)","Sector")
          }
        } #End Economic
        else {
          if(input$Sect_sel=="CV"|input$Sect_sel=="FR"){
            if(input$CategorySelect == "Fisheries"){
               if(input$Sect_sel=="CV"){
                 temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of vessels","Value",  "Variance (MAD, SD)","Sector")
              } else {
                temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of processors","Value",  "Variance (MAD, SD)","Sector")
              }
            } else{
              if(input$Sect_sel=="CV"){
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across","Number of vessels","Value",  "Variance (MAD, SD)","Sector")
              } else {
                temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across","Number of Processors","Value",  "Variance (MAD, SD)","Sector")
              }
            }}  else {
              temp <- data.frame("Year", "Summary Variable","Statistic", "Metric","Data summed across","Number of vessels","Value",  "Variance (MAD, SD)","Sector")
            }
        }
      } # End compare metrics
      #Begin Compare vessles
      else {
        if(input$Ind_sel=="Economic"){
          if(input$Sect_sel=="CV"&input$CategorySelect != "Fisheries"){
            temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Data summed across","Number of vessels", "Value", "Variance (MAD, SD)","Sector")
          }
          else if(input$Sect_sel=="FR"&input$CategorySelect != "Fisheries"){
            temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Economic measure", "Data summed across","Number of processors", "Value", "Variance (MAD, SD)","Sector")
          }else if(input$Sect_sel=="FR"&input$CategorySelect == "Fisheries"){
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure", "Data summed across","Number of processors", "Value", "Variance (MAD, SD)","Sector")
          } else {
            temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)","Sector")
          }
        } #end economic for non-metrics comparison
        else if(input$Ind_sel=="Social and Regional"){
          if(input$CategorySelect == "Fisheries"){
             if(input$socSelect=="Revenue per crew day"|input$socSelect=="Crew wage per day"){
              if(input$Sect_sel=="FR"){
                temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of processors","Value",  "Variance (MAD, SD)","Sector")
              } else {
                temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)","Sector")
              } 
            }else if(input$socSelect=="Share of landings by state"){
              temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of vessels", "Value", "Delivery location","Sector")
            } else {
              if(input$Sect_sel=="FR"){
                temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of processors","Value",  "Variance (MAD, SD)","Sector")
              }else {
                temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)","Sector")
            }}
          } # end fisheries
          else {
            if(input$socSelect=="Revenue per crew day"|input$socSelect=="Crew wage per day"){
              if(input$Sect_sel=="FR"){
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric", "Data summed across","Number of processors", "Value", "Variance (MAD, SD)","Sector")
              }else {
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric", "Data summed across","Number of vessels", "Value", "Variance (MAD, SD)","Sector")
              }
            } else if(input$socSelect=="Share of landings by state"){
              temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across","Number of vessels", "Value", "Delivery location","Sector")
            } else {
               if(input$Sect_sel=="FR"){
                 temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across", "Number of processors","Value",  "Variance (MAD, SD)","Sector")
              } else {
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)","Sector")
              }
            }
          }
        } #End social and regional
        
        else if(input$Ind_sel=="Demographic"){
          if(input$CategorySelect == "Fisheries"){
            if(input$demSelect=="Number of vessels"){
              temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of vessels","Sector")
            } else if(input$demSelect=="Number of processors"){
              temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of processors","Sector")
            }  else {
              if(input$demSelect=="Exponential Shannon Index"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Fishery participation"){
                if(input$Sect_sel=="CV"){
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Alaskan fisheries","Number of vessels","Value",  "Variance (MAD, SD)","Sector")
                } else if(input$Sect_sel=="FR"){                
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of processors","Value","Variance (MAD, SD)","Sector")
                } else {
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of vessels","Value",  "Variance (MAD, SD)","Sector")
                }
              }else if(input$demSelect=="Days at sea"){
                if(input$Sect_sel!="FR"){
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Alaskan fisheries","Number of vessels","Value",  "Variance (MAD, SD)","Sector")
                } else {
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of processors","Value",  "Variance (MAD, SD)","Sector")
                }}
              else {
                if(input$Sect_sel=="FR"){
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of processors","Value",  "Variance (MAD, SD)","Sector")
                } else {
                  temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)","Sector")
                }
              }}
          } # end fisheries
          else {
            if(input$demSelect=="Number of vessels"){
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across","Number of vessels","Sector")
              } else if(input$demSelect=="Number of Processors"){
                names(table) <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across","Number of processors","Sector")
            } else if(input$demSelect=="Exponential Shannon Index"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Fishery participation"|input$demSelect=="Days at sea"){
              if(input$Sect_sel=="FR"){
                temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)","Sector")
              } else {
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across", "Alaskan fisheries","Number of vessels","Value",  "Variance (MAD, SD)","Sector")
              }
            }else {
              if(input$Sect_sel=="FR"){
                temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed cross", "Number of processors",'Value',"Variance (MAD, SD)","Sector")
              }else {
                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)","Sector")
              }}
          } 
        }#End Dempgraphic
      }#end compare vessels 
      
  #    if(input$Ind_sel=="Economic"){
#        if(input$LayoutSelect=="Metrics"){
#            if(input$CategorySelect == "Fisheries"){
#                temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure","Fished for whiting","Number of vessels","Value", "Variance (MAD, SD)")
#            } else if(input$Sect_sel=="CV") {
#                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure","Number of vessels", "Value","Variance (MAD, SD)")
#            }  else {
#                temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Economic measure","Number of vessels", "Value","Variance (MAD, SD)")
#            }
#        } else {
#            if(input$CategorySelect == "Fisheries"){
#                temp <- data.frame("Year", "Summary Variable", "Statistic", "Economic measure","Data summed across", "Number of vessels","Value","Variance (MAD, SD)")
#            } else if(input$Sect_sel=="CV"){
#                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Economic measure", "Data summed across","Number of vessels", "Value", "Variance (MAD, SD)")
#            } else {
#                temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Economic measure", "Data summed across","Number of vessels", "Value", "Variance (MAD, SD)")
#            }
#        }
#      } #End economic
#      else if(input$Ind_sel!="Economic"){
#          if(input$CategorySelect == "Fisheries"){
#           table <- subset(DatSubTable(), select = -c(CATEGORY, CS))
#              if(input$LayoutSelect=="Metrics"){
#                    temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric", "Fished for whiting","Number of vessels","Value",  "Variance (MAD, SD)")
#              } else if(input$LayoutSelect!="Metrics"){
#               if(input$Sect_sel=="M"|input$Sect_sel=="CP"){
#                 if(input$demSelect=="Days at sea"){
#                temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Fished in Alaska", "Number of vessels","Value",  "Variance (MAD, SD)")
#                 } else if(input$demSelect=="Number of vessels"){
#                   temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of vessels")
#                } else {
#                   temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)")
#                 }
#                 }else {
#                  if(input$Ind_sel=="Social and Regional"){
#                    if(input$socSelect=="Revenue per crew day"|input$socSelect=="Crew wage per day"){
#                    temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)")
#                    } else if(input$socSelect=="Share of landings by state"){
#                    temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of vessels", "Value", "Delivery location")
#                    }else {
#                     temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)")
#                    } 
#                } else if(input$Ind_sel=="Demographic"){
#                    if(input$demSelect=="Number of vessels"|input$demSelect=="Number of processors"){
#                    temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Data summed across","Number of vessels")
#                    }  else if(input$demSelect=="Exponential Shannon Index"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Fishery participation"|input$demSelect=="Days at sea"){
#                    temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Fished for whiting", "Alaskan fisheries activities","Number of vessels","Value",  "Variance (MAD, SD)")
#                    } else {
#                    temp <- data.frame("Year", "Summary Variable", "Statistic", "Metric","Fished for whiting", "Number of vessels","Value",  "Variance (MAD, SD)")
#                }}
#              }}#End not metrics
#          }#End fisheries
#        else if(input$CategorySelect != "Fisheries"){
#          if(input$LayoutSelect=="Metrics"){
#                if(input$Sect_sel!="CV"){
#                    temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric", "Fished for whiting","Number of vessels","Value",  "Variance (MAD, SD)")
#                } else {
#                    temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric", "Fished for whiting","Number of vessels","Value",  "Variance (MAD, SD)")
#                }
#           } #end compare metrics
#          else {
#              if(input$Ind_sel=="Social and Regional"){
#                  if(input$socSelect=="Revenue per crew day"|input$socSelect=="Crew wage per day"){
#                  if(input$Sect_sel!="CV"){
#                      temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric", "Data summed across","Number of vessels", "Value", "Variance (MAD, SD)")
#                    } else {
#                      temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric", "Data summed across","Number of vessels", "Value", "Variance (MAD, SD)")
#                  }
#              } else if(input$socSelect=="Share of landings by state"){
#                  if(input$Sect_sel!="CV"){ 
#                    temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across","Number of vessels", "Value", "Delivery location")
#                  } else {
#                    temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across","Number of vessels", "Value", "Delivery location")
#                  }  
#              } else {
#                temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across","Number of vessels", "Value", "Delivery location")
#                
#              }} else if(input$Ind_sel=="Demographic"){
#                if(input$demSelect=="Number of vessels"|input$demSelect=='Number of processors'){
#                  if(input$Sect_sel!="CV"){ 
#                    temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across","Number of vessels")
#                  } else {
#                    temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across","Number of vessels")
#                  }
#          } else if(input$demSelect=="Exponential Shannon Index"|input$demSelect=="Proportion of revenue from CS fishery"|input$demSelect=="Fishery participation"|input$demSelect=="Days at sea"){
#                  if(input$Sect_sel!="CV"){ 
#                    temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Fished for whiting", "Alaskan fisheries activities","Number of vessels","Value",  "Variance (MAD, SD)")
#                  } else {
#                    temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Fished for whiting", "Alaskan fisheries activities","Number of vessels","Value",  "Variance (MAD, SD)")
#                  }
#           }else {
#                if(input$Sect_sel!="CV"){ 
#                  temp <- data.frame("Year", "Summary Variable","Production Category", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)")
#                } else {
#                  temp <- data.frame("Year", "Summary Variable","Fisheries Category", "Statistic", "Metric","Data summed across", "Number of vessels","Value",  "Variance (MAD, SD)")
#         }
#        }}
 #     }}}
        
      
      colnames(temp)=colnames(table)

table <- rbindCommonCols(temp, table) 
            if(input$LayoutSelect=="Metrics"){
              if(input$CategorySelect == "Fisheries"){
                names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                        format(Sys.Date(), format="%B %d %Y")),"", "", "", "","", "","","")
              } else {
                names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                        format(Sys.Date(), format="%B %d %Y")),"", "", "", "","", "","","","")
              }
            } else {
        if(input$CategorySelect == "Fisheries"){
         if(input$Sect_sel=="CP"|input$Sect_sel=="M"){
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "", "", "","","")
          }  #End CP or MS
          else {
          if(input$Ind_sel=="Demographic"&input$demSelect=="Number of vessels"|input$Ind_sel=="Demographic"&input$demSelect=="Number of processors"){
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "", "","","","")
        }  else if(input$Ind_sel=="Demographic"&input$demSelect=="Exponential Shannon Index"|input$Ind_sel=="Demographic"&input$demSelect=="Proportion of revenue from CS fishery"|input$Ind_sel=="Demographic"&input$demSelect=="Fishery participation"|input$Ind_sel=="Demographic"&input$demSelect=="Days at sea"){
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "", "", "","", "","","","")
        }else {
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "", "", "","", "","","")
        }
        } #End CP and MS
        }# End fisheries
      else {
        if(input$Ind_sel=="Demographic"&input$demSelect=="Number of vessels"|input$Ind_sel=="Demographic"&input$demSelect=="Number of processors"){
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "","","","", "","")
        } else if(input$Ind_sel=="Demographic"&input$demSelect=="Exponential Shannon Index"|input$Ind_sel=="Demographic"&input$demSelect=="Proportion of revenue from CS fishery"|input$Ind_sel=="Demographic"&input$demSelect=="Fishery participation"|input$Ind_sel=="Demographic"&input$demSelect=="Days at sea"){
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "","", "", "","", "","","")
        }else {
          names(table) <- c(paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
                                  format(Sys.Date(), format="%B %d %Y")),"", "","", "", "","", "","","")
        }
      }
            }
           write.csv(table, file)
   })

# render plot from  to pdf for download
output$dlFigure <- downloadHandler(
  filename = function() {'perfmetricsPlot.pdf'},
  content = function(file){
  #   if(!PermitPlot()) return()
   
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

