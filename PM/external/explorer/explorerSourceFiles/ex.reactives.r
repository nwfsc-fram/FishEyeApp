#This file handles the reactive expressions for data management and statistical operations.

# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file

DatMain <- reactive({ # data load moved to serverhead
  # data is loaded from serverHead.R load call
  if(input$Sect_sel=="CV"){
    dat <- CVperfmetrics
  } else if(input$Sect_sel=="M"){
    dat <- Mperfmetrics
  } else if(input$Sect_sel=="CP"){
    dat <- CPperfmetrics
  } else if(input$Sect_sel=="FR"){
    dat <- FRperfmetrics
  } 
})


DatVars <- reactive({
  # create a list of variable names used in the sidebar inputs
  dat <- DatMain()
  if(input$Sect_sel=="CV"){
    datVars <- with(dat, 
      list(
        YEAR = 2004:2014,
        SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"),
        CATEGORY = c("Fisheries","Homeport","State","Vessel length class"),
        FISHAK = unique(FISHAK),
        whitingv = c("All vessels","Non-whiting vessels","Whiting vessels"),
        STAT =  c("Average per vessel","Average per vessel/day","Average per vessel/metric-ton","Median per vessel","Median per vessel/day","Median per vessel/metric-ton","Fleet-wide total","Fleet-wide total/day","Fleet-wide total/metric-ton"),
        METRIC =  c("Number of vessels or processors"="Number of vessels","Vessel length","Fishery participation","Proportion of revenue from Catch Share fishery"="Proportion of revenue from CS fishery",
                  "Days at sea","Exponential Shannon Index","Gini coefficient","Herfindahl-Hirschman Index","Number of positions (captain and crew)"='Number of positions',"Crew wage per day","Revenue per crew day",
                  "Seasonality","Share of landings by state")
  ))
  } else if(input$Sect_sel=="FR"){
    datVars <- with(dat, 
                    list(
                      YEAR = 2004:2014,
                      SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"),
                      CATEGORY = c("Fisheries","State","Processor size"),
                      whitingv = c("All vessels","Non-whiting vessels","Whiting vessels"),
                      STAT =  c("Average per vessel","Average per vessel/metric-ton","Median per vessel","Median per vessel/metric-ton","Fleet-wide total","Fleet-wide total/metric-ton"),
                      METRIC =  c("Number of vessels or processors"="Number of processors","Number of species processed","Proportion of revenue from Catch Share fishery"="Proportion of revenue from CS fishery",
                                  "Exponential Shannon Index","Gini coefficient","Herfindahl-Hirschman Index","Number of workers"='Number of workers',"Hourly compensation", "Share of landings by state")
                    ))
  }else if(input$Sect_sel=="M"|input$Sect_sel=="CP"){
      datVars <- with(dat, 
           list(
                YEAR = 2004:2014,
                SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"),
                CATEGORY = "Fisheries",
                FISHAK = unique(FISHAK),
                whitingv = "Whiting vessels",
                STAT =  c("Average per vessel","Average per vessel/day","Average per vessel/metric-ton","Median per vessel","Median per vessel/day","Median per vessel/metric-ton","Fleet-wide total","Fleet-wide total/day","Fleet-wide total/metric-ton"),
                METRIC =  c("Number of vessels or processors"="Number of vessels","Vessel length",
                            "Proportion of revenue from Catch Share fishery"="Proportion of revenue from CS fishery","Days at sea","Gini coefficient",
                            "Herfindahl-Hirschman Index", "Number of positions (captain and crew)"='Number of positions',
                            "Crew wage per day", "Revenue per crew day", "Seasonality")
                    ))
  }
})



# Subset data for table
# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSubTable <- reactive({
    dat <- DatMain()      
#    dat <- dat[-c(which(colnames(dat)=="AK_FLAG"),which(colnames(dat)=="con_flag"),which(colnames(dat)=="repFISHAK"),which(colnames(dat)=="repwhitingv"))]
    
    #subsetting
    if(input$Sect_sel=="CV"|input$Sect_sel=="FR"){    
    datSub <- subset(dat, YEAR %in% input$YearSelect &  
                       CATEGORY == input$CategorySelect &
                       VARIABLE %in% input$VariableSelect &
                       whitingv %in% input$FishWhitingSelect)
    } else {
      datSub <- subset(dat, YEAR %in% input$YearSelect)
    } 
   
    if(input$Ind_sel=="Economic") {
         datSub <- subset(datSub,  SHORTDESCR %in% input$ShortdescrSelect & STAT == input$StatSelect)
    } 
    else if(input$Ind_sel=="Demographic") {
      if(input$LayoutSelect!="Metrics"){
        #        if(input$MetricSelect!="Number of vessels"&input$MetricSelect!="Seasonality"&input$MetricSelect!="Share of landings by state"&input$MetricSelect!="Gini coefficient"){
        #           if(input$LayoutSelect!="Metrics"){}
        if(input$Sect_sel=="FR"){
          datSub <- subset(datSub,  METRIC %in% input$demSelect & SUMSTAT == input$AVE_MED2) 
        }
        else if(input$demSelect=="Exponential Shannon Index"|input$Sect_sel=="CV" &input$demSelect=="Proportion of revenue from CS fishery"|input$Sect_sel=="CV" &input$demSelect=="Fishery participation"|input$demSelect=="Days at sea"){
          datSub <- subset(datSub,  METRIC%in%input$demSelect & SUMSTAT==input$AVE_MED2 &  FISHAK==input$FishAkSelect)
        } else {
          datSub <- subset(datSub,  METRIC %in% input$demSelect & SUMSTAT == input$AVE_MED2)
          }
      }  else {
        if(input$Sect_sel=="CV"){
          datSub <- subset(datSub,  METRIC %in% input$demSelect & SUMSTAT == input$AVE_MED2 & FISHAK!='FALSE')
        } else if(input$Sect_sel=="FR") {
          datSub <- subset(datSub,  METRIC %in% input$demSelect & SUMSTAT == input$AVE_MED2)
        } else {
          datSub <- subset(datSub,  METRIC %in% input$demSelect & SUMSTAT == input$AVE_MED2 & FISHAK!='TRUE')
        }
      }
    } else if(input$Ind_sel=="Social and Regional")  {
      if(input$LayoutSelect!="Metrics"){
        #        if(input$MetricSelect!="Number of vessels"&input$MetricSelect!="Seasonality"&input$MetricSelect!="Share of landings by state"&input$MetricSelect!="Gini coefficient"){
        if(input$socSelect!="Share of landings by state"){
          #           if(input$LayoutSelect!="Metrics"){}
          if(input$Sect_sel=="FR"){
            datSub <- subset(datSub,  METRIC %in% input$socSelect & SUMSTAT == input$AVE_MED2 & !is.na(input$socSelect)) 
          }
           else {
            datSub <- subset(datSub,  METRIC %in% input$socSelect & SUMSTAT == input$AVE_MED2 & !is.na(input$socSelect))
            }
        }   else {
          datSub <- subset(datSub,  METRIC %in% input$socSelect & !is.na(input$socSelect))
        }} else {
          if(input$Sect_sel=="CV"){
            datSub <- subset(datSub,  METRIC %in% input$socSelect & SUMSTAT == input$AVE_MED2 & !is.na(input$socSelect)& FISHAK!='FALSE')
          } else if(input$Sect_sel=="FR") {
            datSub <- subset(datSub,  METRIC %in% input$socSelect & SUMSTAT == input$AVE_MED2 & !is.na(input$socSelect))
          } else {
            datSub <- subset(datSub,  METRIC %in% input$socSelect & SUMSTAT == input$AVE_MED2 & !is.na(input$socSelect)& FISHAK!='TRUE')
          }
        }
    }
    print(datSub[1:2,])
 
#    if(input$Ind_sel!="Economic")  {
#      if(input$MetricSelect!="Number of vessels"&input$MetricSelect!="date"&input$MetricSelect!="landings"&input$MetricSelect!="GINI"){
#        datSub <- subset(datSub,  METRIC %in% input$MetricSelect & !is.na(input$MetricSelect) & SUMSTAT == input$AVE_MED2)}
#      else {
#        datSub <- subset(datSub,  METRIC %in% input$MetricSelect& !is.na(input$MetricSelect) )
#      }}
#    if(input$Ind_sel!="Economic")  {
#      if(input$MetricSelect!="Number of vessels"&input$MetricSelect!="Seasonality"&input$MetricSelect!="Share of landings by state"&input$MetricSelect!="Gini coefficient"){
#        if(input$MetricSelect=="Exponential Shannon Index"|input$Sect_sel=="CV" &input$MetricSelect=="Proportion of revenue from CS fishery"|input$MetricSelect=="Fishery participation"|input$MetricSelect=="Days at sea"){
#          datSub <- subset(datSub,  METRIC %in% input$MetricSelect & SUMSTAT == input$AVE_MED2 & !is.na(input$MetricSelect)& FISHAK==input$FishAkSelect)
#        } else {
#          datSub <- subset(datSub,  METRIC %in% input$MetricSelect & SUMSTAT == input$AVE_MED2 & !is.na(input$MetricSelect))}
#      }   else {
#        datSub <- subset(datSub,  METRIC %in% input$MetricSelect & !is.na(input$MetricSelect))
#      }}
    
 
    
   
     datSub$VALUE <- as.numeric(as.character(datSub$VALUE))
     datSub$VARIANCE <- as.numeric(as.character(datSub$VARIANCE))
     datSub$N <- as.numeric(datSub$N)
#     datSub$PCHANGE <- as.numeric(datSub$PCHANGE)
     
      if(input$Sect_sel=="CV" & input$CategorySelect != "Fisheries") {
        datSub <- subset(datSub, CS == input$inSelect)
      }
     datSub$VARIANCE <- ifelse(datSub$N<3, NA, datSub$VARIANCE)
     datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
#    datSub$N <- ifelse(datSub$N<3, NA, datSub$N)
#    datSub$N <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$N)
    datSub$VARIANCE <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$VARIANCE)
    #datSub$FISHAK <- ifelse(datSub$FISHAK=="TRUE", "Vessels included", "Vessels not included")
   # datSub$whitingv <- ifelse(datSub$whitingv=="TRUE", "Vessels included", "Vessels not included") 

           validate(
         need(sum(!is.na(as.numeric(datSub$VALUE)))!=0, 
              paste('Sorry, the selected statistic is not available for the selected metric. Try selecting a different statistic.
                  ')))
       
  
    if(input$Ind_sel=="Social and Regional"){
      if(input$socSelect[1]=="Share of landings by state"){
      datSub$VALUE <- datSub$VALUE*100
      datSub$VARIANCE <- datSub$VARIANCE*100
    }
      } else if(input$Ind_sel=="Demographic"){
           if(input$demSelect=="Number of vessels"){
             datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
             datSub$N <- ifelse(datSub$N<3, NA, datSub$N)
           }
      }      
   print(datSub[1,])            
if(input$LayoutSelect!="Metrics"){   
  if(input$Ind_sel=="Economic") {
      datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                        which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
      }  else if(input$Ind_sel=="Demographic") {
           if(input$demSelect[1]=="Exponential Shannon Index"|input$demSelect[1]=="Fishery participation"| input$demSelect[1]=="Days at sea"|input$Sect_sel=="CV"&input$demSelect[1]=="Proportion of revenue from CS fishery"
            ){
             if(input$Sect_sel!="FR"){
             datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
                                 which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="FISHAK"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
             datSub$FISHAK <- ifelse(datSub$FISHAK=="TRUE","Included","Not included")
             } else {
               datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
                                   which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
             }
             }
          else if(input$demSelect=="Number of vessels"){
             datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
                            which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"))]
            }else {
             datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
                                 which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
           }}# End Demographic 
      else if(input$Ind_sel=="Social and Regional") {
        if(input$socSelect=="Share of landings by state"){
          datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
                          which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="agid"))]
        }else {
          datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
                          which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
        }}
  
  
 } # Compare vessles
  else if(input$LayoutSelect=="Metrics"){
    if(input$Ind_sel=="Economic") {
      datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                        which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
    } else {
      datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
                      which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
  }}

 
       validate(
           need(dim(datSub)[1]>0, 
               paste('Sorry, this output could not be generated as no vessels matched your selections. Try selecting a different variable.
                  ')))
    return(datSub)
    
})


# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSub <- reactive({
  #  if(input$DodgeSelect == "Economic measures side-by-side"){
      dat <- DatMain()      
      if(input$Sect_sel=="CV"|input$Sect_sel=='FR'){    
        datSub <- subset(dat, YEAR %in% input$YearSelect &  
                            CATEGORY == input$CategorySelect &
                            VARIABLE %in% input$VariableSelect &
                            whitingv %in% input$FishWhitingSelect)
      } else {
        datSub <- subset(dat, YEAR %in% input$YearSelect)
      }

      if(input$Ind_sel=="Economic") {
                datSub <- subset(datSub,  SHORTDESCR %in% input$ShortdescrSelect & STAT == input$StatSelect) 
     } else if(input$Ind_sel=="Demographic")  {
        if(input$LayoutSelect!="Metrics"){
             if(input$Sect_sel=='FR'){
              datSub <- subset(datSub,  METRIC %in% input$demSelect & SUMSTAT == input$AVE_MED2)
            }
            else if(input$demSelect=="Exponential Shannon Index"|input$Sect_sel=="CV" &input$demSelect=="Proportion of revenue from CS fishery"|input$Sect_sel=="CV" &input$demSelect=="Fishery participation"|input$demSelect=="Days at sea"){
              datSub <- subset(datSub,  METRIC %in% input$demSelect & SUMSTAT==input$AVE_MED2& FISHAK==input$FishAkSelect)
            } else {
              datSub <- subset(datSub,  METRIC %in% input$demSelect & SUMSTAT == input$AVE_MED2 )}
            } else {
            if(input$Sect_sel=="CV"){
              datSub <- subset(datSub,  METRIC %in% input$demSelect & SUMSTAT == input$AVE_MED2 & FISHAK!='FALSE')
            } else if(input$Sect_sel=="FR") {
              datSub <- subset(datSub,  METRIC %in% input$demSelect & SUMSTAT == input$AVE_MED2)
            } else {
              datSub <- subset(datSub,  METRIC %in% input$demSelect & SUMSTAT == input$AVE_MED2 & FISHAK!='TRUE')
            }
          }
      } else if(input$Ind_sel=="Social and Regional")  {
        if(input$LayoutSelect!="Metrics"){
#        if(input$MetricSelect!="Number of vessels"&input$MetricSelect!="Seasonality"&input$MetricSelect!="Share of landings by state"&input$MetricSelect!="Gini coefficient"){
          if(input$socSelect!="Share of landings by state"){
 #           if(input$LayoutSelect!="Metrics"){}
            if(input$Sect_sel=='FR'){
              datSub <- subset(datSub,  METRIC %in% input$socSelect & SUMSTAT == input$AVE_MED2)
            }
             else {
            datSub <- subset(datSub,  METRIC %in% input$socSelect & SUMSTAT == input$AVE_MED2)
            }
          }   else {
          datSub <- subset(datSub,  METRIC %in% input$socSelect & !is.na(input$socSelect))
          }} else {
            if(input$Sect_sel=="CV"){
          datSub <- subset(datSub,  METRIC %in% input$socSelect & SUMSTAT == input$AVE_MED2 & FISHAK!='FALSE')
            }else if(input$Sect_sel=="FR") {
              datSub <- subset(datSub,  METRIC %in% input$socSelect & SUMSTAT == input$AVE_MED2)
            } else {
              datSub <- subset(datSub,  METRIC %in% input$socSelect & SUMSTAT == input$AVE_MED2 & FISHAK!='TRUE')
            }
                  }
      } #End Social and Regional
      
     
      validate(
        need(dim(datSub)[1]>0, 
             paste('Sorry, this plot could not be generated as no vessels matched your selections. Try selecting a different variable.
                  ')))
      
      if(input$Sect_sel=="CV" &input$CategorySelect != "Fisheries") {
        datSub <- subset(datSub, CS == input$inSelect)
      }

       
      datSub$N <- as.numeric(datSub$N)
      datSub$VALUE <- as.numeric(datSub$VALUE)
      datSub$VARIANCE <- as.numeric(datSub$VARIANCE)
    
       validate( 
        need(max(datSub$N, na.rm=T)>2, 
             paste('Sorry, this plot could not be generated as data has been suppressed to protect confidentiality. 
                  Try selecting "All fisheries" in the "Show data summed across these fisheries" button or a different variable.
                   ')))
              validate(
                need(sum(!is.na(as.numeric(datSub$VALUE)))!=0, 
                     paste('Sorry, the selected statistic is not available for the selected metric. Try selecting a different statistic.
                  ')))
              
      
      if(input$Ind_sel=="Economic"){
          datSub$VALUE <-datSub$VALUE/1000
          datSub$VARIANCE <- datSub$VARIANCE/1000
        } else if(input$Ind_sel=="Social and Regional") {
          if(input$socSelect=="Revenue per crew day"|input$socSelect=="Crew wage per day"){
          datSub$VALUE <- datSub$VALUE/1000
          datSub$VARIANCE <- datSub$VARIANCE/1000  
      } else if(input$socSelect=="Share of landings by state"){
          datSub$VALUE <- datSub$VALUE*100
          datSub$VARIANCE <- datSub$VARIANCE*100
      } else {
          datSub$VALUE <- datSub$VALUE
          datSub$VARIANCE <- datSub$VARIANCE
      }}else {
        datSub$VALUE <- datSub$VALUE
        datSub$VARIANCE <- datSub$VARIANCE
      }
              
      # order for plotting
#      datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
#        levels = factorOrder$shortdescr)
 
      if(input$Sect_sel=="CV"){
        if(input$CategorySelect == "Homeport"){
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
      } else if(input$CategorySelect == "State"){
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
      } else if(input$CategorySelect == "Fisheries"){
        
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries","At-sea Pacific whiting","Shoreside Pacific whiting",
                                                              "DTS trawl with trawl endorsement","Non-whiting midwater trawl","Non-whiting, non-DTS trawl with trawl endorsement",  "Groundfish fixed gear with trawl endorsement",
                                                              "All non-Catch Share fisheries combined"="All non-Catch Share fisheries", "Groundfish fixed gear with fixed gear endorsement","Crab","Shrimp","Other fisheries"))
        }} else if(input$Sect_sel=="FR"){
                if(input$CategorySelect == "State"){
                datSub$VARIABLE <- factor(datSub$VARIABLE, levels = c("Washington",'Oregon','California','Multi-state'))
                } else if(input$CategorySelect == "Fisheries"){
                datSub$VARIABLE <- factor(datSub$VARIABLE, levels = c("All fisheries","Pacific whiting","Non-whiting groundfish","Other fisheries"))
                } else if(input$CategorySelect == "Processor size") {
                  datSub$VARIABLE <- factor(datSub$VARIABLE, levels =c("Small","Medium","Large"))
                }
           } else {
        datSub$VARIABLE <- "At-sea Pacific whiting"
        }
              
      
      if(input$Ind_sel=="Social and Regional"){
        if(input$socSelect!="Share of landings by state"){
        if(length(input$VariableSelect)>1){
         datSub$star <- ifelse(is.na(datSub$VALUE)==T, "*", "")
         datSub$VARIANCE <- ifelse(is.na(datSub$VARIANCE)==T, 0, datSub$VARIANCE)
#        datSub$con_flag <- ifelse(datSub$con_flag==1, 0, datSub$con_flag)
      }
      else if(length(input$VariableSelect)==1){
        datSub$star <- ""
      }}} else { datSub$star <- ""}
         datSub$VARIANCE <- ifelse(datSub$N<3, NA, datSub$VARIANCE)
         datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
         

   if(input$LayoutSelect!="Metrics"){
        if(input$Sect_sel=="CV"){
            if(input$CategorySelect=="Fisheries"){
            datSub$sort <- ifelse(datSub$VARIABLE=="All fisheries", ".......All fisheries", as.character(datSub$VARIABLE))
            datSub$sort <- ifelse(datSub$VARIABLE=="All Catch Share fisheries", "......All Catch Share fisheries", as.character(datSub$sort))
            datSub$sort <- ifelse(datSub$VARIABLE=="All non-Catch Share fisheries", "..All non-Catch Share fisheries",  as.character(datSub$sort))
            datSub$sort <- ifelse(datSub$VARIABLE=="At-sea Pacific whiting", ".....At-sea Pacific whiting",  as.character(datSub$sort))
            datSub$sort <- ifelse(datSub$VARIABLE=="Shoreside Pacific whiting", ".....Shoreside Pacific whiting",  as.character(datSub$sort))
            datSub$sort <- ifelse(datSub$VARIABLE=="DTS trawl with trawl endorsement", "....DTS trawl with trawl endorsement",  as.character(datSub$sort))
            datSub$sort <- ifelse(datSub$VARIABLE=="Non-whiting midwater trawl", "....Non-whiting midwater trawl",  as.character(datSub$sort))
            datSub$sort <- ifelse(datSub$VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", "....Non-whiting, non-DTS trawl with trawl endorsement",  as.character(datSub$sort))
            datSub$sort <- ifelse(datSub$VARIABLE=="Groundfish fixed gear with trawl endorsement", "...Groundfish fixed gear with trawl endorsement",  as.character(datSub$sort))
            datSub$sort <- ifelse(datSub$VARIABLE=="Groundfish fixed gear with fixed gear endorsement", "..Groundfish fixed gear with fixed gear endorsement",  as.character(datSub$sort))
            datSub$sort <- ifelse(datSub$VARIABLE=="Crab", ".Crab",  as.character(datSub$sort))
            datSub$sort <- ifelse(datSub$VARIABLE=="Shrimp", ".Shrimp",  as.character(datSub$sort))
            } else if(input$CategorySelect == "Homeport") {
            datSub$sort <- ifelse(datSub$VARIABLE=="Puget Sound", ".....Puget Sound", as.character(datSub$VARIABLE))                
            datSub$sort <- ifelse(datSub$VARIABLE=="South and central WA coast", ".....South and central WA coast", as.character(datSub$sort)) 
            datSub$sort <- ifelse(datSub$VARIABLE=="Astoria", "....Astoria", as.character(datSub$sort))                    
            datSub$sort <- ifelse(datSub$VARIABLE=="Tillamook", "....Tillamook", as.character(datSub$sort))                  
            datSub$sort <- ifelse(datSub$VARIABLE=="Newport", "...Newport", as.character(datSub$sort))                   
            datSub$sort <- ifelse(datSub$VARIABLE=="Coos Bay","..Coos Bay", as.character(datSub$sort))                   
            datSub$sort <- ifelse(datSub$VARIABLE=="Brookings", ".Brookings", as.character(datSub$sort))                  
            datSub$sort <- ifelse(datSub$VARIABLE=="Crescent City", ".Crescent City", as.character(datSub$sort))              
            datSub$sort <- ifelse(datSub$VARIABLE=="Eureka", ".Eureka", as.character(datSub$sort))                     
            datSub$sort <- ifelse(datSub$VARIABLE=="Fort Bragg", ".Fort Bragg", as.character(datSub$sort))                
            datSub$sort <- ifelse(datSub$VARIABLE=="San Francisco", ".San Francisco", as.character(datSub$sort))              
            } else {
            datSub$sort <- datSub$VARIABLE 
            }
        }# End CV
        else if(input$Sect_sel=='FR'){
            if(input$CategorySelect=="Fisheries"){
              datSub$sort <- ifelse(datSub$VARIABLE=="All fisheries", ".All fisheries", as.character(datSub$VARIABLE))
              datSub$sort <- ifelse(datSub$VARIABLE=="Non-whiting groundfish", ".Non-whiting groundfish", as.character(datSub$sort))
            } else if(input$CategorySelect == "State"){
              datSub$sort <- ifelse(datSub$VARIABLE=="Washington", "..Washington", as.character(datSub$VARIABLE))
              datSub$sort <- ifelse(datSub$VARIABLE=="Oregon", ".Oregon", as.character(datSub$sort))
            } else {
              datSub$sort <- as.character(datSub$VARIABLE)
            }
        } #end FR
        else {datSub$sort <- "At-sea Pacific whiting"
        } #end MS and CP  
    } #end Metrics
      else {
        if(input$Ind_sel=="Economic"){
      datSub$sort <- ifelse(datSub$SHORTDESCR=="Revenue", "...Revenue", as.character(datSub$SHORTDESCR))
      datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable costs", "..Variable costs", as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$SHORTDESCR=="Fixed costs", ".Fixed costs", as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable Cost Net Revenue", ".Variable Cost Net Revenue",  as.character(datSub$sort))
       # }  else if(input$MetricSelect=="Share of landings by state"){
        #  datSub$sort <- ifelse(datSub$agid=="In Washington", "...In Washington", as.character(datSub$agid))
        #  datSub$sort <- ifelse(datSub$agid=="In Oregon", "..In Oregon", as.character(datSub$sort))
        #  datSub$sort <- ifelse(datSub$agid=="In California", ".In California", as.character(datSub$sort))
        #  datSub$sort <- ifelse(datSub$agid=="At sea", ".At sea",  as.character(datSub$sort))
        } else {
       datSub$sort <- as.character(datSub$METRIC)
     }}
        
      datSub$whitingv <- factor(datSub$whitingv, levels=c("Whiting vessels","Non-whiting vessels","All vessels"))
      
#      if(input$Ind_sel!="Economic"&input$MetricSelect=="Number of vessels"){
#        if(input$Ind_sel=="Demographic"&input$demSelect=="Number of vessels"){
#          datSub$star <- ifelse(datSub$N<3, "*", datSub$star)
#        datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
#        datSub$N <- ifelse(datSub$N<3, NA, datSub$N)
#      }
      datSub$flag <- max(datSub$flag, na.rm=T)
      datSub$metric_flag <- max(datSub$metric_flag, na.rm=T)
      datSub$conf <- ifelse(datSub$CS=="All fisheries", 0, datSub$conf)
 
      if(input$Ind_sel=="Demographic"&input$demSelect=="Number of vessels"){#    if(datSub$METRIC=="Number of vessels"){
        datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
        datSub$N <- ifelse(datSub$N<3, NA, datSub$N)
      }
   print(datSub)    
      return(datSub)
          
  #   } else return()
#   )
})




PermitPlot <- reactive({
 
  if(!(is.null(input$YearSelect) | is.null(input$CategorySelect) | 
       is.null(input$VariableSelect))){
    if(!(input$YearSelect[1]=="" | 
         input$CategorySelect[1] == "" | input$VariableSelect[1] == "")){      
      x <- TRUE
    } else {
      x <- FALSE
    }
  } else x <- FALSE
  x
 
})

#PermitMessage <- reactive({
#  if(!(is.null(input$YearSelect) | is.null(input$CategorySelect) | 
#       is.null(input$VariableSelect))){
#    if(any(grepl(2009, input$YearSelect))){
#      if(any(grepl("Groundfish fixed gear with trawl endorsement",input$VariableSelect))
#      ){      
#        x <- TRUE
#      } else {
#        x <- FALSE
#      }
#    } else x <- FALSE
#  } else x <- FALSE
#  x
#})

#Download buttons only shows up if PermitPlot()==T
output$download_Table <- renderUI({
  if(PermitPlot()) {
    tags$div(class="actbutton",downloadButton("dlTable", "Download Data Table",class = "btn btn-info"))
#    tags$div(actionButton("", "Download Data Table coming soon",class = "btn btn-info"))
    #    }
  }
})

output$download_figure <- renderUI({
  if(PermitPlot()){
    tags$div(class="actbutton",downloadButton("dlFigure", "Download Plot(s)",class = "btn btn-info"))
#    tags$div(actionButton("", "Download Plot(s) coming soon",class = "btn btn-info"))
  }
})

output$resetButton <- renderUI({
  if(PermitPlot()){
    tags$div(class="actbutton",actionButton("reset_input", HTML("<strong>Clear selections & <br> Return to Instructions</strong>"), class="btn btn-info"))
  }
})

#output$VCNRButton <- renderUI({

vars = reactiveValues(counter = 0.5)
output$DataButton <- renderUI({
  if(PermitPlot()){
    actionButton("data", label = label()) }
})

observe({
  if(!is.null(input$data)){
    input$data
    isolate({
      vars$counter <- vars$counter + .5
    })
  }
})

label <- reactive({
  if(!is.null(input$data)){
    if(vars$counter%%2 != 0) label <- "Show Data"
    else label <- "Show Plot(s)"
  }
})



vars2 = reactiveValues(counter = 0.5)
output$DataButton2 <- renderUI({
  if(PermitPlot()){
    actionButton("data2", label = label2())
  }
})

observe({
  if(!is.null(input$data2)){
    input$data2
    isolate({
      vars2$counter <- vars2$counter + .5
    })
  }
})

label2 <- reactive({
  if(!is.null(input$data2)){
    if(vars2$counter%%2 != 0) label2 <- "Show Data"
    else label2 <- "Show Plot(s)"
  }
})

