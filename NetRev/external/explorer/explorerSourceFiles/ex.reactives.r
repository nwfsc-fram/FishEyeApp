#This file handles the reactive expressions for data management and statistical operations.

# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file
DatMain <- reactive({ # data load moved to serverhead
  # data is loaded from serverHead.R load call
  if(input$Sect_sel=="CV"){
    dat <- netrevTable
    } else if(input$Sect_sel=="M"){
    dat <- MSnetrev
  } else if(input$Sect_sel=="CP"){
    dat <- CPnetrev
  } else if(input$Sect_sel=="FR"){
    dat <- FRnetrev
  } 
})

#DatMain <- reactive({ # data load moved to serverhead
  # data is loaded from serverHead.R load call
#  dat <- netrevTable
  #dat <- subset(dat, YEAR<2014)
#})

DatThirds <- reactive({
  if(input$Sect_sel=="CV"){
    dat <- netrevThirds
   } else if(input$Sect_sel=="FR"){
    dat <- FRthirds
   }
})


DatVars <- reactive({
  # create a list of variable names used in the sidebar inputs
  dat <- DatMain()
  if(input$Sect_sel=="CV"){
  datVars <- with(dat, 
                  list(YEAR = 2009:2015,
                       SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"),
                       CATEGORY = c("Fisheries","Homeport","State of homeport"="State","Vessel length class"),
                       FISHAK = unique(FISHAK),
                       whitingv = unique(whitingv),
                       STAT =  c("Average per vessel","Average per vessel/day","Average per vessel/metric-ton caught","Median per vessel","Median per vessel/day","Median per vessel/metric-ton caught","Fleet-wide total")#="Total"
                   ))
  } else if(input$Sect_sel=="FR"){
    
    datVars <- with(dat, 
                    list(YEAR =  2009:2015,
                         SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"),
                         CATEGORY = c("Production activities","Region","Processor size"),
                         FISHAK = unique(FISHAK),
                         STAT =  c("Average per processor","Average per processor/metric-ton produced","Median per processor","Median per processor/metric-ton produced","Industry-wide total")#="Total"
                    ))
  } else {
    datVars <- with(dat, 
                    list(YEAR =  2009:2015,
                         SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"),
                         CATEGORY = c("Fisheries"),
                         STAT =  c("Average per vessel","Average per vessel/day","Average per vessel/metric-ton produced","Median per vessel","Median per vessel/day","Median per vessel/metric-ton produced","Fleet-wide total")#="Total"
                    ))
  }
})


#Variable <- reactive({
#  dat <- DatMain()
#  if(input$CategorySelect == "Fisheries"){
#    variable = c("All Fisheries", "All Catch Share Fisheries","At-sea Pacific whiting","Shoreside Pacific whiting","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
#                 "Groundfish fixed gear with trawl endorsement","Groundfish fixed gear with fixed gear endorsement",
#                 "All Non-Catch Share Fisheries", "Crab","Shrimp","Other fisheries")
#  } else if(input$CategorySelect == "State"){
#    variable = factorOrder$state
#  } else if(input$CategorySelect == "Homeport"){
#    variable = factorOrder$port
#  } else {
#    variable = factorOrder$lengths
    #       subByCategory <- dat[dat$CATEGORY == input$CategorySelect,] 
#  }
#  return(variable)
#})


# Subset data for table
# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSubTable <- reactive({

  dat <- DatMain()      
  dat <- dat[-c(which(colnames(dat)=="con_flag"),which(colnames(dat)=="flag"))]
 
   if(input$Sect_sel=="FR") {
     dat$STAT <- ifelse(dat$STAT=="Fleet-wide total", "Industry-wide total", as.character(dat$STAT))
     dat$STAT <- ifelse(dat$STAT=="Average per vessel", "Average per processor", as.character(dat$STAT))
     dat$STAT <- ifelse(dat$STAT=="Average per vessel/metric-ton", "Average per processor/metric-ton", as.character(dat$STAT))
     dat$STAT <- ifelse(dat$STAT=="Median per vessel", "Median per processor", as.character(dat$STAT))
     dat$STAT <- ifelse(dat$STAT=="Median per vessel/metric-ton", "Median per processor/metric-ton", as.character(dat$STAT))
   }
  #subsetting
  datSub <- with(dat, dat[which(SHORTDESCR %in% input$ShortdescrSelect & 
                     CATEGORY %in% input$CategorySelect &
                     VARIABLE %in% input$VariableSelect &
                     STAT == input$StatSelect),])
  
  if(input$Sect_sel=="CV"){
    datSub <- with(datSub, datSub[which(YEAR %in% input$YearSelect &  
                                        FISHAK == input$FishAkSelect &
                                        whitingv == input$FishWhitingSelect),])
  datSub$FISHAK <- ifelse(datSub$AK_FLAG==0, datSub$FISHAK, datSub$repFISHAK)
  datSub$whitingv <- ifelse(datSub$AK_FLAG==0, as.character(datSub$whitingv), datSub$repwhitingv)
  datSub <- datSub[,-c(which(colnames(datSub)=="repFISHAK"), which(colnames(datSub)=="repwhitingv"),which(colnames(dat)=="AK_FLAG"))]
  } else if(input$Sect_sel=="FR") {
    datSub <- subset(datSub, ACS == input$ProductionSelect &
                             YEAR %in% input$YearSelect2) 
      } else{
    datSub <- subset(datSub, YEAR %in% input$YearSelect2) 
      }
  
  datSub$VALUE <- as.numeric(datSub$VALUE)
  datSub$VARIANCE <- as.numeric(datSub$VARIANCE)
  
  
  # order for plotting
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                              levels = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"))
  
  if(input$Sect_sel=='CV' & input$CategorySelect != "Fisheries" || input$Sect_sel=='FR' & input$CategorySelect != "Production activities") {
    datSub <- subset(datSub, CS == input$inSelect)
  }
  datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
#  datSub$N <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$N)
  datSub$VARIANCE <- ifelse(datSub$N<3, NA, datSub$VARIANCE)
  
  if(input$Sect_sel=="CV"){
  datSub$FISHAK <- ifelse(datSub$FISHAK=="TRUE", "Vessels included", "Vessels not included")
  datSub$whitingv <- ifelse(datSub$whitingv=="TRUE", "Vessels included", "Vessels not included") 
  datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                      which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="FISHAK"),which(colnames(datSub)=="whitingv"),
                      which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
  } else if(input$Sect_sel=='FR'){
    datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="ACS"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                        which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
  } 
  else {
    datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                        which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
  }
  
  validate(
    need(dim(datSub)[1]>0, #min(datSub$N)>2,
         paste('Sorry, this plot could not be generated as no vessels matched your selections. 
               Try clicking the box to include all vessels that fished in AK or include all vessels that fished for whiting. 
               ')))
  return(datSub)
  

})


# Subset data for table
# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatThirdsTable <- reactive({
  #   if(is.null(input$DataButton) || input$DataButton == 0) return()
  #   input$ShortdescrSelect
  #   isolate(  
  #  if(!is.null(DatMain())  ){
  dat <- DatThirds()      
  dat <- dat[,-which(colnames(dat)=="con_flag")]
   if(input$Sect_sel=="FR") {
    dat$STAT <- ifelse(dat$STAT=="Fleet-wide total", "Industry-wide total", as.character(dat$STAT))
    dat$STAT <- ifelse(dat$STAT=="Average per vessel", "Average per processor", as.character(dat$STAT))
    dat$STAT <- ifelse(dat$STAT=="Average per vessel/metric-ton", "Average per processor/metric-ton", as.character(dat$STAT))
    dat$STAT <- ifelse(dat$STAT=="Median per vessel", "Median per processor", as.character(dat$STAT))
    dat$STAT <- ifelse(dat$STAT=="Median per vessel/metric-ton", "Median per processor/metric-ton", as.character(dat$STAT))
  }
  
  #subsetting
  datSub <- subset(dat, SHORTDESCR %in% input$ShortdescrSelect & 
                     CATEGORY %in% input$CategorySelect &
                     VARIABLE %in% input$VariableSelect &
                     STAT == input$StatSelect)
  
  if(input$Sect_sel=="CV"){
  datSub <- subset(datSub, YEAR %in% input$YearSelect &  
                           FISHAK == input$FishAkSelect &
                           whitingv == input$FishWhitingSelect)
  datSub$FISHAK <- ifelse(datSub$AK_FLAG==1, datSub$repFISHAK, datSub$FISHAK)
  datSub$whitingv <- ifelse(datSub$AK_FLAG==1, datSub$repwhitingv, as.character(datSub$whitingv))
  datSub <- datSub[,-c(which(colnames(datSub)=="repFISHAK"), which(colnames(datSub)=="repwhitingv"),which(colnames(dat)=="AK_FLAG"))]
  } else if(input$Sect_sel=="FR") {
    datSub <- subset(datSub, ACS == input$ProductionSelect &
                       YEAR %in% input$YearSelect2) 
  }
  
  datSub$VALUE <- as.numeric(datSub$VALUE)
  datSub$VARIANCE <- as.numeric(datSub$VARIANCE)
 
  
  # order for plotting
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                              levels = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"))
  datSub$THIRDS <- ifelse(datSub$THIRDS=="Bottom third", "Lower third", as.character(datSub$THIRDS))
  
  if(input$Sect_sel=='CV' & input$CategorySelect != "Fisheries" || input$Sect_sel=='FR' & input$CategorySelect != "Production activities") {
    datSub <- subset(datSub, CS == input$inSelect)
  }
#  datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
#  datSub$N <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$N)
#  datSub$VARIANCE <- ifelse(datSub$N>2, NA, datSub$VARIANCE)
  
  if(input$Sect_sel=="CV"){
  datSub$FISHAK <- ifelse(datSub$FISHAK=="TRUE", "Vessels included", "Vessels not included")
  datSub$whitingv <- ifelse(datSub$whitingv=="TRUE", "Vessels included", "Vessels not included") 
  datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                      which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="THIRDS"),which(colnames(datSub)=="FISHAK"),which(colnames(datSub)=="whitingv"),
                      which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
  } else if(input$Sect_sel=='FR'){
    datSub <- datSub[,c(which(colnames(datSub)=="YEAR"), which(colnames(datSub)=="ACS"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                        which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="THIRDS"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
    
  } 
  
  validate(
    need(dim(datSub)[1]>0, #min(datSub$N)>2,
         'Sorry, this plot could not be generated as no vessels matched your selections. 
          Try clicking the box to include all vessels that fished in AK or include all vessels that fished for whiting.'),
    need(datSub$VARIABLE!='Large',
         'Sorry, this plot could not be generated as an insufficient number of processors matched your selections. 
          Try selecting a different processor size.'),
    need(input$VariableSelect!='Small',
         need(input$ProductionSelect!="Catch share processors",
            "Sorry, this plot could not be generated as an insufficient number of processors matched your selections. 
            Try selecting  'All processors' or selecting a different processor size. "))
    )
  
  print(datSub)[1:3,]
  return(datSub)
  
})

# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSub <- reactive({
  
    dat <- DatMain() 
    if(input$Sect_sel=="FR") {
      dat$STAT <- ifelse(dat$STAT=="Fleet-wide total", "Industry-wide total", as.character(dat$STAT))
      dat$STAT <- ifelse(dat$STAT=="Average per vessel", "Average per processor", as.character(dat$STAT))
      dat$STAT <- ifelse(dat$STAT=="Average per vessel/metric-ton", "Average per processor/metric-ton", as.character(dat$STAT))
      dat$STAT <- ifelse(dat$STAT=="Median per vessel", "Median per processor", as.character(dat$STAT))
      dat$STAT <- ifelse(dat$STAT=="Median per vessel/metric-ton", "Median per processor/metric-ton", as.character(dat$STAT))
    }
    
      datSub <- subset(dat, CATEGORY == input$CategorySelect &
                            VARIABLE %in% input$VariableSelect &
                            STAT == input$StatSelect)
      
      if(input$Sect_sel=="CV"){
        datSub <- subset(datSub,  YEAR %in% input$YearSelect & 
                                  FISHAK == input$FishAkSelect &
                                  whitingv == input$FishWhitingSelect )
      } else if(input$Sect_sel=="FR") {
        datSub <- subset(datSub, ACS == input$ProductionSelect &
                           YEAR %in% input$YearSelect2) 
      } else {
        datSub <- subset(datSub, YEAR %in% input$YearSelect2)  
      }

#subset the shortdescr data based on the type of plot chosen and then define the order for plotting
        if(input$DodgeSelect == "Economic measures side-by-side"){
          datSub <- subset(datSub, SHORTDESCR %in% input$ShortdescrSelect)
                datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, levels = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"))
          }
        if(input$DodgeSelect == "Composition of Total Cost Net Revenue"){
            datSub <- subset(datSub,  SHORTDESCR %in% c("Total Cost Net Revenue", "Fixed costs","Variable costs"))
                  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, levels=c("Variable costs","Fixed costs","Total Cost Net Revenue") )
          }
        if(input$DodgeSelect == "Composition of Variable Cost Net Revenue"){
            datSub <- subset(datSub,  SHORTDESCR %in% c("Variable Cost Net Revenue", "Variable costs"))  
                  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, levels=c("Variable costs","Variable Cost Net Revenue"))
          }

    
# for Homeport, state, and vessel length, subset the data by fisheries category (all fisheries, catch shares only, non-catch shares)
    if(input$Sect_sel=="CV" & input$CategorySelect != "Fisheries" || input$Sect_sel=="FR" & input$CategorySelect!="Production activities") {
        datSub <- subset(datSub, CS == input$inSelect)
    }
       
    datSub$VALUE <- as.numeric(datSub$VALUE)

    #Define levels - this is for the order when plotting
    if(input$CategorySelect == "Homeport"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
    } else if(input$CategorySelect == "State"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
    } else if(input$CategorySelect == "Fisheries"){
      if(input$Sect_sel=="CV"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = c("All fisheries","All catch share fisheries","All non-catch share fisheries",'Pacific whiting',"At-sea Pacific whiting",                      
                                                            "Shoreside Pacific whiting",'Groundfish with trawl gear',"DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
                                                            "Non-whiting midwater trawl","Groundfish fixed gear with trawl endorsement","Groundfish fixed gear with fixed gear endorsement",
                                                            "Crab","Shrimp"))
      }else{
        datSub$VARIABLE <- factor(datSub$VARIABLE)
      }
    }

    # This one fishery in this one year has too few observations to include. Rather than losing the ability to click the select all fisheries button, we have opted to 
    # remove it from the data set. An error message will occur if this is the only fishery selected.
 #   if(input$CategorySelect=="Fisheries" & length(input$VariableSelect)>1){
 #     datSub <- subset(datSub, datSub$VARIABLE!="Groundfish fixed gear with trawl endorsement"|datSub$YEAR!=2009)
 #   } 
#    if(length(input$VariableSelect)>1){
      datSub$VALUE <- ifelse(datSub$flag==1, 0, datSub$VALUE)
      datSub$star <- ifelse(datSub$flag==1, "*", "")
      datSub$con_flag <- ifelse(datSub$con_flag==1, 0, datSub$con_flag)
      datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
      datSub$VARIANCE <- ifelse(datSub$N<3, NA, datSub$VARIANCE)
      
#    }
#    else {
#      datSub$star <- ""
#    }
    
      
    # This is my quick solution to a ggplot issue with level and facet grid. The dots are removed in a function at the end of doPlot.
    #The URL below is a solution for similar problem with stacking and may work for this issue. I have not yet tried.
    #https://github.com/hadley/ggplot2/issues/1301  #use website for dealing with stacked bar plot order issue
#    if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
#      datSub$sort <- ifelse(datSub$VARIABLE=="All fisheries", "........All fisheries", 
#                             ifelse(datSub$VARIABLE=="All catch share fisheries", ".......All catch share fisheries", 
#                                    ifelse(datSub$VARIABLE=="All non-catch share fisheries", "..All non-catch share fisheries",  
#                                           ifelse(datSub$VARIABLE=="Pacific whiting", ".......Pacific whiting",  
#                                                  ifelse(datSub$VARIABLE=="At-sea Pacific whiting", "......At-sea Pacific whiting", 
#                                                         ifelse(datSub$VARIABLE=="Shoreside Pacific whiting", "......Shoreside Pacific whiting",  
#                                                                ifelse(datSub$VARIABLE=="Groundfish with trawl gear", ".....Groundfish with trawl gear",  
#                                                                       ifelse(datSub$VARIABLE=="DTS trawl with trawl endorsement", "....DTS trawl with trawl endorsement",  
#                                                                              ifelse(datSub$VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", "....Non-whiting, non-DTS trawl with trawl endorsement",  
#                                                                                     ifelse(datSub$VARIABLE=="Non-whiting midwater trawl","....Non-whiting midwater trawl" , 
#                                                                                            ifelse(datSub$VARIABLE=="Groundfish fixed gear with trawl endorsement", "...Groundfish fixed gear with trawl endorsement",  
#                                                                                                   ifelse(datSub$VARIABLE=="Groundfish fixed gear with fixed gear endorsement", "..Groundfish fixed gear with fixed gear endorsement", 
#                                                                                                          ifelse(datSub$VARIABLE=="Crab", ".Crab", 
#                                                                                                                 ifelse(datSub$VARIABLE=="Shrimp", ".Shrimp",  as.character(datSub$VARIABLE)
#                                                                                                                        ))))))))))))))
#} else if(input$CategorySelect == "Homeport") {
#  datSub$sort <- ifelse(datSub$VARIABLE=="Puget Sound", ".....Puget Sound",               
#                        ifelse(datSub$VARIABLE=="South and central WA coast", ".....South and central WA coast", 
#                               ifelse(datSub$VARIABLE=="Astoria", "....Astoria",                     
#                                      ifelse(datSub$VARIABLE=="Tillamook", "....Tillamook",                   
#                                             ifelse(datSub$VARIABLE=="Newport", "...Newport",                   
#                                                    ifelse(datSub$VARIABLE=="Coos Bay","..Coos Bay",                  
#                                                           ifelse(datSub$VARIABLE=="Brookings", ".Brookings",                  
#                                                                  ifelse(datSub$VARIABLE=="Crescent City", ".Crescent City",               
#                                                                         ifelse(datSub$VARIABLE=="Eureka", ".Eureka",                    
#                                                                                ifelse(datSub$VARIABLE=="Fort Bragg", ".Fort Brag",                 
#                                                                                       ifelse(datSub$VARIABLE=="San Francisco", ".San Francisco", as.character(datSub$VARIABLE)
#                                                                                       )))))))))))             
#}
#else {
#  datSub$sort <- datSub$VARIABLE 
#}
if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
        datSub$sort <- ifelse(datSub$VARIABLE=="All fisheries", 1, 
                              ifelse(datSub$VARIABLE=="All catch share fisheries", 2, 
                                     ifelse(datSub$VARIABLE=="Pacific whiting", 3,  
                                            ifelse(datSub$VARIABLE=="At-sea Pacific whiting", 4, 
                                                   ifelse(datSub$VARIABLE=="Shoreside Pacific whiting", 5,  
                                                          ifelse(datSub$VARIABLE=="Groundfish with trawl gear", 6,  
                                                                 ifelse(datSub$VARIABLE=="DTS trawl with trawl endorsement", 7,  
                                                                        ifelse(datSub$VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", 8,  
                                                                               ifelse(datSub$VARIABLE=="Non-whiting midwater trawl",9, 
                                                                                      ifelse(datSub$VARIABLE=="Groundfish fixed gear with trawl endorsement", 10,  
                                                                                              ifelse(datSub$VARIABLE=="All non-catch share fisheries", 11,  
                                                                                                    ifelse(datSub$VARIABLE=="Groundfish fixed gear with fixed gear endorsement", 12, 
                                                                                                           ifelse(datSub$VARIABLE=="Crab", 13, 
                                                                                                                  ifelse(datSub$VARIABLE=="Shrimp", 14,  15
                                                                                                                  ))))))))))))))
      } else if(input$CategorySelect == "Homeport") {
      datSub$sort <- ifelse(datSub$VARIABLE=="Puget Sound", 1,               
                            ifelse(datSub$VARIABLE=="South and central WA coast", 2, 
                                   ifelse(datSub$VARIABLE=="Astoria", 3,                     
                                          ifelse(datSub$VARIABLE=="Tillamook", 4,                   
                                          ifelse(datSub$VARIABLE=="Newport", 5,                   
                                                 ifelse(datSub$VARIABLE=="Coos Bay",6,                  
                                                        ifelse(datSub$VARIABLE=="Brookings", 7,                  
                                                               ifelse(datSub$VARIABLE=="Crescent City", 8,               
                                                                      ifelse(datSub$VARIABLE=="Eureka", 9,                    
                                                                             ifelse(datSub$VARIABLE=="Fort Bragg", 10,                 
                                                                                      ifelse(datSub$VARIABLE=="San Francisco", 11, 12
                                                                                             )))))))))))             
      } else if(input$CategorySelect == "Production activities") {
      datSub$sort <- ifelse(datSub$VARIABLE=="All production", 1,
                            ifelse(datSub$VARIABLE=="Groundfish production", 2, 
                                   ifelse(datSub$VARIABLE=="Pacific whiting production", 3,
                                          ifelse(datSub$VARIABLE=="Non-whiting groundfish production",4,5))))
      } else if(input$CategorySelect == 'Region') {
      datSub$sort <- ifelse(datSub$VARIABLE=='Washington and Oregon', 1,2)
    }
    else {
      datSub$sort <- datSub$VARIABLE 
    }
    

# Error messages for when the rule of 3 or 90/10 rules have been broken.    
#    validate(
#      need(max(datSub$con_flag)<1, 
#           paste('Sorry, this plot could not be generated as data from the following', datSub$CATEGORY[1],  '(s) and year(s) were suppressed:\n')),#grouping variable(s)
#      need(max(datSub$con_flag)<1,
#           paste(datSub$VARIABLE[which(datSub$con_flag==1&datSub$SHORTDESCR==datSub$SHORTDESCR[1])],
#                 datSub$YEAR[which(datSub$con_flag==1&datSub$SHORTDESCR==datSub$SHORTDESCR[1])])),
#      need(max(datSub$con_flag)<1, 
#           message=paste('
#Data are suppressed when there are not enough observations to protect confidentiality. 
#Although a plot could not be generated, the unsuppressed data from your selection can still be viewed and downloaded.  
#To view plots, try unclicking the indicated', datSub$CATEGORY[1], '(s) or year(s). If a', datSub$CATEGORY[1], 'or year is not provided, try clicking the box to include all vessels that fished in Alaska or fished for Pacific whiting. There are less vessels that fished solely off the West Coast than off the West Coast and in Alaska.
#                         ')))
    validate(
      need(dim(datSub)[1]>0, #min(datSub$N)>2,
           'Sorry, this plot could not be generated as no vessels matched your selections. 
                 Try clicking the box to include all vessels that fished in Alaska or include all vessels that fished for Pacific whiting. 
                 '))
    
#   datSub$VALUE <- ifelse(is.na(datSub$VALUE)==T, 0, datSub$VALUE) 
    return(datSub)
})



# create an additional subset for thirds plot
DatSubThirds <- reactive({
  dat <- DatThirds()
  if(input$Sect_sel=="FR") {
    dat$STAT <- ifelse(dat$STAT=="Fleet-wide total", "Industry-wide total", as.character(dat$STAT))
    dat$STAT <- ifelse(dat$STAT=="Average per vessel", "Average per processor", as.character(dat$STAT))
    dat$STAT <- ifelse(dat$STAT=="Average per vessel/metric-ton", "Average per processor/metric-ton", as.character(dat$STAT))
    dat$STAT <- ifelse(dat$STAT=="Median per vessel", "Median per processor", as.character(dat$STAT))
    dat$STAT <- ifelse(dat$STAT=="Median per vessel/metric-ton", "Median per processor/metric-ton", as.character(dat$STAT))
  }
  
  #subsetting
  datSub <- subset(dat, 
                     SHORTDESCR %in% input$ShortdescrSelect &
                     CATEGORY %in% input$CategorySelect &
                     VARIABLE %in% input$VariableSelect &
                     STAT == input$StatSelect
  )

  if(input$Sect_sel=="CV"){
    datSub <- subset(datSub, YEAR %in% input$YearSelect &
                          FISHAK == input$FishAkSelect &
                          whitingv == input$FishWhitingSelect)
  } else if(input$Sect_sel=="FR") {
    datSub <- subset(datSub, ACS == input$ProductionSelect &
                       YEAR %in% input$YearSelect2) 
  }
  print(datSub[1:2,])  
  if(input$Sect_sel=="CV" & input$CategorySelect != "Fisheries" || input$Sect_sel=="FR" & input$CategorySelect != 'Production activities') {
    datSub <- subset(datSub, CS == input$inSelect)
  }
  

  #datSub <- subset(datSub, is.na(datSub$N)==F)
  datSub$THIRDS <- factor(datSub$THIRDS,
                          levels = factorOrder$thirds)
  datSub$THIRDS <- ifelse(datSub$THIRDS=="Bottom third", "Lower third", as.character(datSub$THIRDS))
  
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                              levels =  c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"))
#  if(input$LayoutSelect=='Economic measures'){
#  datSub$sort <- ifelse(datSub$SHORTDESCR=="Revenue", "...Revenue", as.character(datSub$SHORTDESCR))
#  datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable costs", "..Variable costs", as.character(datSub$sort))
#  datSub$sort <- ifelse(datSub$SHORTDESCR=="Fixed costs", ".Fixed costs",  as.character(datSub$sort))
#  datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable Cost Net Revenue", ".Variable Cost Net Revenue",  as.character(datSub$sort))
#  } else {
#    if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
#      datSub$sort <- ifelse(datSub$VARIABLE=="All fisheries", "........All fisheries", 
#                            ifelse(datSub$VARIABLE=="All catch share fisheries", ".......All catch share fisheries", 
#                                   ifelse(datSub$VARIABLE=="All non-catch share fisheries", "..All non-catch share fisheries",  
#                                          ifelse(datSub$VARIABLE=="Pacific whiting", ".......Pacific whiting",  
#                                                 ifelse(datSub$VARIABLE=="At-sea Pacific whiting", "......At-sea Pacific whiting", 
#                                                        ifelse(datSub$VARIABLE=="Shoreside Pacific whiting", "......Shoreside Pacific whiting",  
#                                                               ifelse(datSub$VARIABLE=="Groundfish with trawl gear", ".....Groundfish with trawl gear",  
#                                                                      ifelse(datSub$VARIABLE=="DTS trawl with trawl endorsement", "....DTS trawl with trawl endorsement",  
#                                                                             ifelse(datSub$VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", "....Non-whiting, non-DTS trawl with trawl endorsement",  
#                                                                                    ifelse(datSub$VARIABLE=="Non-whiting midwater trawl","....Non-whiting midwater trawl" , 
#                                                                                           ifelse(datSub$VARIABLE=="Groundfish fixed gear with trawl endorsement", "...Groundfish fixed gear with trawl endorsement",  
#                                                                                                  ifelse(datSub$VARIABLE=="Groundfish fixed gear with fixed gear endorsement", "..Groundfish fixed gear with fixed gear endorsement", 
#                                                                                                         ifelse(datSub$VARIABLE=="Crab", ".Crab", 
#                                                                                                                ifelse(datSub$VARIABLE=="Shrimp", ".Shrimp",  as.character(datSub$VARIABLE)
#                                                                                                                ))))))))))))))
#    } else if(input$CategorySelect == "Homeport") {
#      datSub$sort <- ifelse(datSub$VARIABLE=="Puget Sound", ".....Puget Sound",               
#                            ifelse(datSub$VARIABLE=="South and central WA coast", ".....South and central WA coast", 
#                                   ifelse(datSub$VARIABLE=="Astoria", "....Astoria",                     
#                                          ifelse(datSub$VARIABLE=="Tillamook", "....Tillamook",                   
#                                                 ifelse(datSub$VARIABLE=="Newport", "...Newport",                   
#                                                        ifelse(datSub$VARIABLE=="Coos Bay","..Coos Bay",                  
#                                                               ifelse(datSub$VARIABLE=="Brookings", ".Brookings",                  
#                                                                      ifelse(datSub$VARIABLE=="Crescent City", ".Crescent City",               
#                                                                             ifelse(datSub$VARIABLE=="Eureka", ".Eureka",                    
#                                                                                    ifelse(datSub$VARIABLE=="Fort Bragg", ".Fort Brag",                 
#                                                                                           ifelse(datSub$VARIABLE=="San Francisco", ".San Francisco", as.character(datSub$VARIABLE)
#                                                                                           )))))))))))             
#    }
#    else {
#      datSub$sort <- datSub$VARIABLE 
#    }    
#  }
  if(input$LayoutSelect=='Economic measures'){
    datSub$sort <- ifelse(datSub$SHORTDESCR=="Revenue",1,
                          ifelse(datSub$SHORTDESCR=="Variable costs",2, 
                                  ifelse(datSub$SHORTDESCR=="Fixed costs", 3,  
                                          ifelse(datSub$SHORTDESCR=="Variable Cost Net Revenue", 4,  5))))
  } else {
    if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
      datSub$sort <- ifelse(datSub$VARIABLE=="All fisheries", 1, 
                            ifelse(datSub$VARIABLE=="All catch share fisheries", 2, 
                                   ifelse(datSub$VARIABLE=="Pacific whiting", 3,  
                                          ifelse(datSub$VARIABLE=="At-sea Pacific whiting", 4, 
                                                 ifelse(datSub$VARIABLE=="Shoreside Pacific whiting", 5,  
                                                        ifelse(datSub$VARIABLE=="Groundfish with trawl gear", 6,  
                                                               ifelse(datSub$VARIABLE=="DTS trawl with trawl endorsement", 7,  
                                                                      ifelse(datSub$VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", 8,  
                                                                             ifelse(datSub$VARIABLE=="Non-whiting midwater trawl",9, 
                                                                                    ifelse(datSub$VARIABLE=="All non-catch share fisheries", 10,  
                                                                                           ifelse(datSub$VARIABLE=="Groundfish fixed gear with trawl endorsement", 11,  
                                                                                                  ifelse(datSub$VARIABLE=="Groundfish fixed gear with fixed gear endorsement", 12, 
                                                                                                         ifelse(datSub$VARIABLE=="Crab", 13, 
                                                                                                                ifelse(datSub$VARIABLE=="Shrimp", 14,  15
                                                                                                                ))))))))))))))
    } else if(input$CategorySelect == "Homeport") {
      datSub$sort <- ifelse(datSub$VARIABLE=="Puget Sound", 1,               
                            ifelse(datSub$VARIABLE=="South and central WA coast", 2, 
                                   ifelse(datSub$VARIABLE=="Astoria", 3,                     
                                          ifelse(datSub$VARIABLE=="Tillamook", 4,                   
                                                 ifelse(datSub$VARIABLE=="Newport", 5,                   
                                                        ifelse(datSub$VARIABLE=="Coos Bay",6,                  
                                                               ifelse(datSub$VARIABLE=="Brookings", 7,                  
                                                                      ifelse(datSub$VARIABLE=="Crescent City", 8,               
                                                                             ifelse(datSub$VARIABLE=="Eureka", 9,                    
                                                                                    ifelse(datSub$VARIABLE=="Fort Bragg", 10,                 
                                                                                           ifelse(datSub$VARIABLE=="San Francisco", 11, 12
                                                                                           )))))))))))             
    } else if(input$CategorySelect == "Production activities") {
      datSub$sort <- ifelse(datSub$VARIABLE=="All production", 1,
                            ifelse(datSub$VARIABLE=="Groundfish production", 2, 
                                   ifelse(datSub$VARIABLE=="Pacific whiting production", 3,
                                          ifelse(datSub$VARIABLE=="Non-whiting groundfish production",4,5))))
    } else if(input$CategorySelect == 'Region') {
      datSub$sort <- ifelse(datSub$VARIABLE=='Washington and Oregon', 1,2)
    }
    else {
      datSub$sort <- datSub$VARIABLE 
    }
  }
    
 
  
  if(input$CategorySelect == "Homeport"){
    datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
  } else if(input$CategorySelect == "State"){
    datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
  } else if(input$CategorySelect == "Fisheries"){
    if(input$Sect_sel=="CV"){
    datSub$VARIABLE <- factor(datSub$VARIABLE, levels = c("All fisheries","All catch share fisheries","All non-catch share fisheries",'Pacific whiting',"At-sea Pacific whiting",                      
                                                            "Shoreside Pacific whiting",'Groundfish with trawl gear',"DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
                                                            "Non-whiting midwater trawl","Groundfish fixed gear with trawl endorsement","Groundfish fixed gear with fixed gear endorsement",
                                                            "Crab","Shrimp"))
    } else {
      datSub$VARIABLE <- factor(datSub$VARIABLE)
    }
    }else {
      datSub$VARIABLE <- factor(datSub$VARIABLE)
    }
#  datSub$VALUE <- ifelse(datSub$con_flag==1, "", datSub$VALUE)
#  datSub$star <- ifelse(datSub$con_flag==1, "*", "")
  datSub$con_flag <- max(datSub$con_flag)
  datSub <- subset(datSub, is.na(datSub$VALUE)==F)
  datSub$VALUE <- as.numeric(datSub$VALUE)
  datSub$VARIANCE <- as.numeric(datSub$VARIANCE)
#  datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
#  datSub$VARIANCE <- ifelse(is.na(datSub$VALUE)==T, NA, datSub$VARIANCE)
  
  
  validate(
    need(dim(datSub)[1]>0, 
         'Sorry, this plot could not be generated as no vessels matched your selections. 
          Try clicking the box to include all vessels that fished in Alaska or include all vessels that fished for Pacific whiting.'),
    need(datSub$VARIABLE!='Large',
         'Sorry, this plot could not be generated as an insufficient number of processors matched your selections. 
          Try selecting a different processor size.'),
    need(input$VariableSelect!='Small',
         need(input$ProductionSelect!="Catch share processors",
          "Sorry, this plot could not be generated as an insufficient number of processors matched your selections. 
          Try selecting  'All processors' or selecting a different processor size."))
  )
  
  
  return(datSub)
  
  # }
})



# Plotting/staging
PermitPlot <- reactive({
  if(!(is.null(input$YearSelect) | is.null(input$CategorySelect) | 
       is.null(input$VariableSelect) |
       is.null(input$StatSelect) | is.null(input$ShortdescrSelect))){
    if(!(input$YearSelect[1]=="" | 
         input$CategorySelect[1] == "" | input$StatSelect[1] == "" | 
         input$VariableSelect[1] == ""  | input$ShortdescrSelect[1] == "")){      
      x <- TRUE
    } else {
      x <- FALSE
    }
  } else x <- FALSE
  x
})

PermitMessage <- reactive({
  if(!(is.null(input$YearSelect) | is.null(input$CategorySelect) | 
       is.null(input$VariableSelect) |
       is.null(input$StatSelect) | is.null(input$ShortdescrSelect))){
    if(any(grepl(2009, input$YearSelect))){
      if(any(grepl("Groundfish fixed gear with trawl endorsement",input$VariableSelect))
      ){      
        x <- TRUE
      } else {
        x <- FALSE
      }
    } else x <- FALSE
  } else x <- FALSE
  x
})


#Download buttons only shows up if PermitPlot()==T
output$download_Table <- renderUI({
  if(PermitPlot()) {
    #    if(input$tabs=="Panel1"){
    tags$div(class="actbutton",downloadButton("dlTable", "Download Data Table",class = "btn btn-info"))
    #    }
  }
})

output$download_figure <- renderUI({
  if(PermitPlot()){# & input$tabs=="Visualize Data"){
    tags$div(class="actbutton",downloadButton("dlFigure", "Download Plot(s)",class = "btn btn-info"))
  }
})

output$resetButton <- renderUI({
  if(PermitPlot()){
    tags$div(class="actbutton",actionButton("reset_input", HTML("<strong>Clear selections & <br> Return to Instructions</strong>"), class="btn btn-info"))
  }
})

output$VCNRButton <- renderUI({
  if(PermitPlot()){
    if(input$DodgeSelect == "Composition of Total Cost Net Revenue"){
      HTML('<a class="btn btn-primary", href="TCNRGraphic.png" target="_blank" style="height:37px;position:absolute;bottom:170%;left:390%; background-color:RoyalBlue"> Explanation of this plot</a>')
    } else     if(input$DodgeSelect == "Composition of Variable Cost Net Revenue"){
      HTML('<a class="btn btn-primary", href="VCNRGraphic.png" target="_blank" style="height:37px;position:absolute;bottom:170%;left:390%; background-color:RoyalBlue"> Explanation of this plot</a>')
    }  
  }
})

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
    if(vars$counter%%2 != 0) label <- "Show Data Table"
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
    if(vars2$counter%%2 != 0) label2 <- "Show Data Table"
    else label2 <- "Show Plot(s)"
  }
})

#output$download_Thirds<- renderUI({
#  if(PermitPlot() & input$tabs=="Variability Analaysis"){
#    downloadButton("dlPlotThirds", "Download Figure",class = "btn btn-info")
#  }
#})

