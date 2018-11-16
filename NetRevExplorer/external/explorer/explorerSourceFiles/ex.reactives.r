#This file handles the reactive expressions for data management and statistical operations.
  # data is loaded from serverHead.R load call

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


DatThirds <- reactive({
  if(input$Sect_sel=="CV"){
    dat <- netrevThirds
   } else if(input$Sect_sel=="FR"){
    dat <- FRthirds
   }
})

# create a list of variable names used in the sidebar inputs
DatVars <- reactive({
  dat <- DatMain()
  if(input$Sect_sel=="CV"){
      datVars <- with(dat, 
                  list(YEAR = 2009:currentyear,
                       SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue",
                                      "Total Cost Net Revenue"),
                       CATEGORY = c("Fisheries","Homeport","State of homeport"="State","Vessel length class"),
                       FISHAK = unique(FISHAK),
                       whitingv = unique(whitingv),
                       STAT =  c("Mean per vessel","Mean per vessel/day","Mean per vessel/metric ton caught",
                                 "Median per vessel","Median per vessel/day","Median per vessel/metric ton caught",
                                 "Fleet-wide total")#="Total"
                   ))
  } else if(input$Sect_sel=="FR"){
      datVars <- with(dat, 
                    list(YEAR =  2009:currentyear,
                         SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue",
                                        "Total Cost Net Revenue"),
                         CATEGORY = c("Production activities","Region","Processor size"),
                         FISHAK = unique(FISHAK),
                         STAT =  c("Mean per processor","Mean per processor/metric ton produced",
                                   "Median per processor","Median per processor/metric ton produced",
                                   "Industry-wide total")#="Total"
                    ))
  } else {
    datVars <- with(dat, 
                    list(YEAR =  2009:currentyear,
                         SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue",
                                        "Total Cost Net Revenue"),
                         CATEGORY = c("Fisheries"),
                         STAT =  c("Mean per vessel","Mean per vessel/day","Mean per vessel/metric ton produced",
                                   "Median per vessel","Median per vessel/day",
                                   "Median per vessel/metric ton produced","Fleet-wide total")#="Total"
                    ))
  }
})


# Subset data for table
# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSubTable <- reactive({
  dat <- DatMain()      
  dat <- dat[-c(which(colnames(dat)=="con_flag"),which(colnames(dat)=="flag"))]

  dat$STAT <- ifelse(dat$STAT=="Average per vessel", "Mean per vessel", 
                        ifelse(dat$STAT=="Average per vessel/day", "Mean per vessel/day", 
                               ifelse(dat$STAT=="Average per vessel/metric ton caught",
                                                  "Mean per vessel/metric ton caught", 
                                      ifelse(dat$STAT=="Average per vessel/metric ton produced", 
                                                          "Mean per vessel/metric ton produced",
                                             ifelse(dat$STAT=="Average per processor", "Mean per processor",
                                                    ifelse(dat$STAT=="Average per processor/metric ton produced",
                                                           "Mean per processor/metric ton produced",
                                                                  as.character(dat$STAT)))))))

  dat$VARIABLE <- ifelse(dat$VARIABLE=='Small vessel (< 60 ft)', 'Small vessel (<= 60ft)', as.character(dat$VARIABLE))
  
  #subsetting
  datSub <- with(dat, dat[which(SHORTDESCR %in% input$ShortdescrSelect & 
                     CATEGORY %in% input$CategorySelect &
                     VARIABLE %in% input$VariableSelect &
                     STAT == input$StatSelect),])
  
  if(input$Sect_sel=="CV"){
      datSub <- with(datSub, datSub[which(YEAR %in% seq(input$YearSelect[1], input$YearSelect[2], 1) &  
                                        FISHAK == input$FishAkSelect &
                                        whitingv == input$FishWhitingSelect),])
      datSub$FISHAK <- ifelse(datSub$AK_FLAG==0, datSub$FISHAK, datSub$repFISHAK)
      datSub$whitingv <- ifelse(datSub$AK_FLAG==0, as.character(datSub$whitingv), datSub$repwhitingv)
      datSub <- datSub %>% subset(select=-c(repFISHAK,repwhitingv,AK_FLAG))
  } else if(input$Sect_sel=="FR") {
    datSub <- subset(datSub, ACS == input$ProductionSelect & YEAR %in% seq(input$YearSelect2[1], input$YearSelect2[2], 1)) 
      } else{
    datSub <- subset(datSub, YEAR %in% seq(input$YearSelect2[1], input$YearSelect2[2], 1)) 
      }
  
  datSub$VALUE <- as.numeric(datSub$VALUE)
  datSub$VARIANCE <- as.numeric(datSub$VARIANCE)
  
  
  # order for plotting
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, levels = c("Revenue","Variable costs","Fixed costs",
                                                            "Variable Cost Net Revenue","Total Cost Net Revenue"))
  
  if(input$Sect_sel=='CV' & input$CategorySelect != "Fisheries" || 
     input$Sect_sel=='FR' & input$CategorySelect != "Production activities") {
    datSub <- subset(datSub, CS == input$inSelect)
  }
  datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
#  datSub$N <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$N)
  datSub$VARIANCE <- ifelse(datSub$N<3, NA, datSub$VARIANCE)
  datSub$q25 <- round(as.numeric(datSub$q25),0)
  datSub$q75 <- round(as.numeric(datSub$q75),0)
  
  datSub$VARIANCE <- ifelse(grepl('Median',datSub$STAT), paste(datSub$q25, ',', datSub$q75), datSub$VARIANCE)
  
  if(input$Sect_sel=="CV"){
    datSub$FISHAK <- ifelse(datSub$FISHAK=="TRUE", "Vessels included", "Vessels not included")
    datSub$whitingv <- ifelse(datSub$whitingv=="TRUE", "Vessels included", "Vessels not included") 
    datSub <- datSub %>% subset(select=c(YEAR,VARIABLE,CATEGORY,CS,STAT,SHORTDESCR,FISHAK,whitingv,N,VALUE,VARIANCE)) 
  } else if(input$Sect_sel=='FR'){
    datSub <- datSub %>% subset(select=c(YEAR,ACS,VARIABLE,CATEGORY,CS,STAT,SHORTDESCR,N,VALUE,VARIANCE))
  } 
  else {
    datSub <- datSub %>% subset(select=c(YEAR,VARIABLE,CATEGORY,CS,STAT,SHORTDESCR,N,VALUE,VARIANCE))
  }
  
  validate(
    need(dim(datSub)[1]>0, #min(datSub$N)>2,
         paste('Sorry, this plot could not be generated as no vessels matched your selections. 
               Try clicking the box to include all vessels that fished in AK or 
                include all vessels that fished for whiting. 
               ')))
  return(datSub)
  

})


# Subset data for table
# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatThirdsTable <- reactive({
  dat <- DatThirds()      
  dat <- dat %>% subset(select=-con_flag)
  dat$STAT <- ifelse(dat$STAT=="Average per vessel", "Mean per vessel", 
                     ifelse(dat$STAT=="Average per vessel/day", "Mean per vessel/day", 
                            ifelse(dat$STAT=="Average per vessel/metric ton caught", 
                                              "Mean per vessel/metric ton caught", 
                                   ifelse(dat$STAT=="Average per vessel/metric ton produced",
                                                      "Mean per vessel/metric ton produced",
                                          ifelse(dat$STAT=="Average per processor", "Mean per processor",
                                                 ifelse(dat$STAT=="Average per processor/metric ton produced", 
                                                                  "Mean per processor/metric ton produced",
                                                                      as.character(dat$STAT)))))))
  
  dat$VARIABLE <- ifelse(dat$VARIABLE=='Small vessel (< 60 ft)', 'Small vessel (<= 60ft)', as.character(dat$VARIABLE))
  
  #subsetting
  datSub <- subset(dat, SHORTDESCR %in% input$ShortdescrSelect & 
                        CATEGORY %in% input$CategorySelect &
                        VARIABLE %in% input$VariableSelect &
                        STAT == input$StatSelect)
 
   if(input$Sect_sel=="CV"){
  datSub <- with(datSub, datSub[which(YEAR %in% seq(input$YearSelect[1], input$YearSelect[2], 1) &  
                                        FISHAK == input$FishAkSelect &
                                        whitingv == input$FishWhitingSelect),])
      datSub$FISHAK <- ifelse(datSub$AK_FLAG==0, as.character(datSub$FISHAK), datSub$repFISHAK)
      datSub$whitingv <- ifelse(datSub$AK_FLAG==0, as.character(datSub$whitingv), datSub$repwhitingv)
      datSub <- datSub %>% subset(select=-c(repFISHAK,repwhitingv,AK_FLAG))
  } else if(input$Sect_sel=="FR") {
      datSub <- subset(datSub, ACS == input$ProductionSelect &
                       YEAR %in% seq(input$YearSelect2[1], input$YearSelect2[2], 1)) 
  }
  
  
  
  datSub$VALUE <- as.numeric(datSub$VALUE)
  datSub$VARIANCE <- as.numeric(datSub$VARIANCE)
  datSub$q25 <- round(as.numeric(datSub$q25),0)
  datSub$q75 <- round(as.numeric(datSub$q75),0)
  
  datSub$VARIANCE <- 
    ifelse(grepl('Median',datSub$STAT), paste(datSub$q25, ',', datSub$q75), datSub$VARIANCE)
  
  # order for plotting
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                              levels = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue",
                                         "Total Cost Net Revenue"))
  datSub$THIRDS <- ifelse(datSub$THIRDS=="Bottom third", "Lower third", as.character(datSub$THIRDS))
  
  if(input$Sect_sel=='CV' & input$CategorySelect != "Fisheries" || 
     input$Sect_sel=='FR' & input$CategorySelect != "Production activities") {
        datSub <- subset(datSub, CS == input$inSelect)
  }

  if(input$Sect_sel=="CV"){
      datSub$FISHAK <- ifelse(datSub$FISHAK=="TRUE", "Vessels included", "Vessels not included")
      datSub$whitingv <- ifelse(datSub$whitingv=="TRUE", "Vessels included", "Vessels not included") 
      datSub <- datSub %>% subset(select=c(YEAR,VARIABLE,CATEGORY,CS,STAT,SHORTDESCR,THIRDS,FISHAK,whitingv,N,
                                    VALUE,VARIANCE))
  } else if(input$Sect_sel=='FR'){
      datSub <- datSub %>% subset(select=c(YEAR,ACS,VARIABLE,CATEGORY,CS,STAT,SHORTDESCR,THIRDS,N,VALUE,VARIANCE))
    
  } 
  
  validate(
    need(dim(datSub)[1]>0, #min(datSub$N)>2,
         'Sorry, this plot could not be generated as no vessels matched your selections. 
          Try clicking the box to include all vessels that fished in AK or 
          include all vessels that fished for whiting.'),
    need(datSub$VARIABLE!='Large',
         'Sorry, this plot could not be generated as an insufficient number of processors matched your selections. 
          Try selecting a different processor size.'),
    need(input$VariableSelect!='Small',
         need(input$ProductionSelect!="Catch share processors",
            "Sorry, this plot could not be generated as an insufficient number of processors matched your selections. 
            Try selecting  'All processors' or selecting a different processor size. "))
    )
  
  return(datSub)
})


# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSub <- reactive({
    dat <- DatMain() 
    dat$STAT <- ifelse(dat$STAT=="Average per vessel", "Mean per vessel", 
                       ifelse(dat$STAT=="Average per vessel/day", "Mean per vessel/day", 
                              ifelse(dat$STAT=="Average per vessel/metric ton caught", 
                                                "Mean per vessel/metric ton caught", 
                                     ifelse(dat$STAT=="Average per vessel/metric ton produced", 
                                                      "Mean per vessel/metric ton produced",
                                            ifelse(dat$STAT=="Average per processor", "Mean per processor",
                                                   ifelse(dat$STAT=="Average per processor/metric ton produced", 
                                                                    "Mean per processor/metric ton produced",
                                                                          as.character(dat$STAT)))))))
    
    dat$VARIABLE <- ifelse(dat$VARIABLE=='Small vessel (< 60 ft)', 'Small vessel (<= 60ft)', 
                                      as.character(dat$VARIABLE))
    
      datSub <- subset(dat, CATEGORY == input$CategorySelect &
                            VARIABLE %in% input$VariableSelect &
                            STAT == input$StatSelect)
      
      if(input$Sect_sel=="CV"){
        datSub <- subset(datSub,  YEAR %in% seq(input$YearSelect[1], input$YearSelect[2], 1) & 
                                  FISHAK == input$FishAkSelect &
                                  whitingv == input$FishWhitingSelect )
      } else if(input$Sect_sel=="FR") {
        datSub <- subset(datSub, ACS == input$ProductionSelect &
                                 YEAR %in% seq(input$YearSelect2[1], input$YearSelect2[2], 1)) 
      } else {
        datSub <- subset(datSub, YEAR %in% seq(input$YearSelect2[1], input$YearSelect2[2], 1))  
      }

#subset the shortdescr data based on the type of plot chosen and then define the order for plotting
        if(input$DodgeSelect == "Economic measures side-by-side"){
            datSub <- subset(datSub, SHORTDESCR %in% input$ShortdescrSelect)
            datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, levels = c("Revenue","Variable costs","Fixed costs",
                                                                      "Variable Cost Net Revenue",
                                                                      "Total Cost Net Revenue"))
          }
        if(input$DodgeSelect == "Composition of Total Cost Net Revenue"){
            datSub <- subset(datSub,  SHORTDESCR %in% c("Total Cost Net Revenue", "Fixed costs","Variable costs"))
            datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, levels=c("Variable costs","Fixed costs",
                                                                    "Total Cost Net Revenue") )
          }
        if(input$DodgeSelect == "Composition of Variable Cost Net Revenue"){
            datSub <- subset(datSub,  SHORTDESCR %in% c("Variable Cost Net Revenue", "Variable costs"))  
            datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, levels=c("Variable costs","Variable Cost Net Revenue"))
          }

    
# for Homeport, state, and vessel length, subset the data by fisheries category (all fisheries, catch shares only, non-catch shares)
    if(input$Sect_sel=="CV" & input$CategorySelect != "Fisheries" || 
       input$Sect_sel=="FR" & input$CategorySelect!="Production activities") {
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
      datSub$VARIABLE <- factor(datSub$VARIABLE, 
                               levels = c("All fisheries","All catch share fisheries","All non-catch share fisheries",
                                          'Pacific whiting',"At-sea Pacific whiting", "Shoreside Pacific whiting", 
                                          'Groundfish with trawl gear',"DTS trawl with trawl endorsement",
                                          "Non-whiting, non-DTS trawl with trawl endorsement",
                                          "Non-whiting midwater trawl","Groundfish fixed gear with trawl endorsement",
                                          "Groundfish fixed gear with fixed gear endorsement","Crab","Shrimp"))
      }else{
        datSub$VARIABLE <- factor(datSub$VARIABLE)
      }
    }

      datSub$VALUE <- ifelse(datSub$flag==1, 0, datSub$VALUE)
      datSub$star <- ifelse(datSub$flag==1, "*", "")
      datSub$con_flag <- ifelse(datSub$con_flag==1, 0, datSub$con_flag)
      datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
      datSub$VARIANCE <- ifelse(datSub$N<3, NA, datSub$VARIANCE)
      
      
if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
   datSub$sort <- with(datSub,
                    ifelse(VARIABLE=="All fisheries", 1, 
                      ifelse(VARIABLE=="All catch share fisheries", 2, 
                        ifelse(VARIABLE=="Pacific whiting", 3,  
                          ifelse(VARIABLE=="At-sea Pacific whiting", 4, 
                            ifelse(VARIABLE=="Shoreside Pacific whiting", 5,  
                              ifelse(VARIABLE=="Groundfish with trawl gear", 6,  
                                ifelse(VARIABLE=="DTS trawl with trawl endorsement", 7,  
                                  ifelse(VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", 8,
                                    ifelse(VARIABLE=="Non-whiting midwater trawl",9, 
                                      ifelse(VARIABLE=="Groundfish fixed gear with trawl endorsement", 10,
                                        ifelse(VARIABLE=="All non-catch share fisheries", 11,  
                                          ifelse(VARIABLE=="Groundfish fixed gear with fixed gear endorsement", 12, 
                                            ifelse(VARIABLE=="Crab", 13, 
                                              ifelse(VARIABLE=="Shrimp", 14,  15                                                           )))))))))))))))
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
    

    validate(
      need(dim(datSub)[1]>0, #min(datSub$N)>2,
           'Sorry, this plot could not be generated as no vessels matched your selections. 
                 Try clicking the box to include all vessels that fished in Alaska or 
                 include all vessels that fished for Pacific whiting. 
                 '))
    
    return(datSub)
})



# create an additional subset for thirds plot
DatSubThirds <- reactive({
  dat <- DatThirds()
  dat$STAT <- ifelse(dat$STAT=="Average per vessel", "Mean per vessel", 
                     ifelse(dat$STAT=="Average per vessel/day", "Mean per vessel/day", 
                            ifelse(dat$STAT=="Average per vessel/metric ton caught", 
                                              "Mean per vessel/metric ton caught", 
                                   ifelse(dat$STAT=="Average per vessel/metric ton produced", 
                                                  "Mean per vessel/metric ton produced",
                                          ifelse(dat$STAT=="Average per processor", "Mean per processor",
                                                 ifelse(dat$STAT=="Average per processor/metric ton produced", 
                                                                  "Mean per processor/metric ton produced",
                                                                      as.character(dat$STAT)))))))
  
  dat$VARIABLE <- ifelse(dat$VARIABLE=='Small vessel (< 60 ft)', 'Small vessel (<= 60ft)', as.character(dat$VARIABLE))

  #subsetting
  datSub <- subset(dat, CATEGORY %in% input$CategorySelect &
                     VARIABLE %in% input$VariableSelect &
                     SHORTDESCR %in% input$ShortdescrSelect & 
                     STAT == input$StatSelect)

  if(input$Sect_sel=="CV"){
    datSub <- subset(datSub, YEAR %in% seq(input$YearSelect[1], input$YearSelect[2], 1) &
                          FISHAK == input$FishAkSelect &
                          whitingv == input$FishWhitingSelect)
  } else {#if(input$Sect_sel=="FR") 
    datSub <- subset(datSub, ACS == input$ProductionSelect &
                       YEAR %in% seq(input$YearSelect2[1], input$YearSelect2[2], 1)) 
  }
  if(input$Sect_sel=="CV" & input$CategorySelect != "Fisheries"){
    datSub <- subset(datSub, CS == input$inSelect)
  }
  if(input$Sect_sel=="FR" & input$CategorySelect != 'Production activities') {
    datSub <- subset(datSub, CS == input$inSelect)
  }


  
  
  #datSub <- subset(datSub, is.na(datSub$N)==F)
  datSub$THIRDS <- factor(datSub$THIRDS,
                          levels = factorOrder$thirds)
  datSub$THIRDS <- ifelse(datSub$THIRDS=="Bottom third", "Lower third", as.character(datSub$THIRDS))
  
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                              levels =  c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue",
                                          "Total Cost Net Revenue"))
#  if(input$LayoutSelect=='Economic measures'){
#  datSub$sort <- ifelse(datSub$SHORTDESCR=="Revenue", "...Revenue", as.character(datSub$SHORTDESCR))
#  datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable costs", "..Variable costs", as.character(datSub$sort))
#  datSub$sort <- ifelse(datSub$SHORTDESCR=="Fixed costs", ".Fixed costs",  as.character(datSub$sort))
#  datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable Cost Net Revenue", ".Variable Cost Net Revenue",  as.character(datSub$sort))
#  } else {
#    if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
#      datSub$sort <- with(datSub,
#                      ifelse(VARIABLE=="All fisheries", "........All fisheries", 
#                       ifelse(VARIABLE=="All catch share fisheries", ".......All catch share fisheries", 
#                        ifelse(VARIABLE=="All non-catch share fisheries", "..All non-catch share fisheries", 
#                         ifelse(VARIABLE=="Pacific whiting", ".......Pacific whiting",  
#                          ifelse(VARIABLE=="At-sea Pacific whiting", "......At-sea Pacific whiting", 
#                           ifelse(VARIABLE=="Shoreside Pacific whiting", "......Shoreside Pacific whiting",  
#                            ifelse(VARIABLE=="Groundfish with trawl gear", ".....Groundfish with trawl gear",  
#                             ifelse(VARIABLE=="DTS trawl with trawl endorsement", "....DTS trawl with trawl endorsement",  
#                               ifelse(VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", "....Non-whiting, non-DTS trawl with trawl endorsement",  
#                                 ifelse(VARIABLE=="Non-whiting midwater trawl","....Non-whiting midwater trawl" , 
#                                  ifelse(VARIABLE=="Groundfish fixed gear with trawl endorsement", "...Groundfish fixed gear with trawl endorsement",  
#                                   ifelse(VARIABLE=="Groundfish fixed gear with fixed gear endorsement", "..Groundfish fixed gear with fixed gear endorsement", 
#                                     ifelse(VARIABLE=="Crab", ".Crab", 
#                                       ifelse(VARIABLE=="Shrimp", ".Shrimp",  as.character(VARIABLE)
#                           )))))))))))))))
#    } else if(input$CategorySelect == "Homeport") {
#      datSub$sort <- ifelse(datSub$VARIABLE=="Puget Sound", ".....Puget Sound",               
#                       ifelse(datSub$VARIABLE=="South and central WA coast", ".....South and central WA coast", 
#                         ifelse(datSub$VARIABLE=="Astoria", "....Astoria",                     
#                           ifelse(datSub$VARIABLE=="Tillamook", "....Tillamook",                   
#                             ifelse(datSub$VARIABLE=="Newport", "...Newport",                   
#                                ifelse(datSub$VARIABLE=="Coos Bay","..Coos Bay",                  
#                                  ifelse(datSub$VARIABLE=="Brookings", ".Brookings",                  
#                                     ifelse(datSub$VARIABLE=="Crescent City", ".Crescent City",               
#                                       ifelse(datSub$VARIABLE=="Eureka", ".Eureka",                    
#                                         ifelse(datSub$VARIABLE=="Fort Bragg", ".Fort Brag",                 
#                                           ifelse(datSub$VARIABLE=="San Francisco", ".San Francisco", 
#                                               as.character(datSub$VARIABLE))))))))))))             
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
      datSub$sort <- with(datSub,
                      ifelse(VARIABLE=="All fisheries", 1, 
                       ifelse(VARIABLE=="All catch share fisheries", 2, 
                         ifelse(VARIABLE=="Pacific whiting", 3,  
                           ifelse(VARIABLE=="At-sea Pacific whiting", 4, 
                             ifelse(VARIABLE=="Shoreside Pacific whiting", 5,  
                               ifelse(VARIABLE=="Groundfish with trawl gear", 6,  
                                 ifelse(VARIABLE=="DTS trawl with trawl endorsement", 7,  
                                   ifelse(VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", 8,  
                                     ifelse(VARIABLE=="Non-whiting midwater trawl",9, 
                                       ifelse(VARIABLE=="All non-catch share fisheries", 10,  
                                         ifelse(VARIABLE=="Groundfish fixed gear with trawl endorsement", 11,  
                                           ifelse(VARIABLE=="Groundfish fixed gear with fixed gear endorsement", 12, 
                                             ifelse(VARIABLE=="Crab", 13, 
                                               ifelse(VARIABLE=="Shrimp", 14,  15
                          )))))))))))))))
    } else if(input$CategorySelect == "Homeport") {
      datSub$sort <- ifelse(datSub$VARIABLE=="Puget Sound", 1,               
                        ifelse(datSub$VARIABLE=="South and central WA coast", 2, 
                           ifelse(datSub$VARIABLE=="Astoria", 3,                     
                              ifelse(datSub$VARIABLE=="Tillamook", 4,                   
                                 ifelse(datSub$VARIABLE=="Newport", 5,                   
                                    ifelse(datSub$VARIABLE=="Coos Bay",6,                  
                                       ifelse(datSub$VARIABLE=="Brookings", 7,                                                                                  ifelse(datSub$VARIABLE=="Crescent City", 8,               
                                             ifelse(datSub$VARIABLE=="Eureka", 9,                    
                                                 ifelse(datSub$VARIABLE=="Fort Bragg", 10,                                                                               ifelse(datSub$VARIABLE=="San Francisco", 11, 12)))))))))))             
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
    datSub$VARIABLE <- factor(datSub$VARIABLE, 
                              levels = c("All fisheries","All catch share fisheries","All non-catch share fisheries",
                                         'Pacific whiting',"At-sea Pacific whiting",  "Shoreside Pacific whiting",                                             'Groundfish with trawl gear',"DTS trawl with trawl endorsement",
                                         "Non-whiting, non-DTS trawl with trawl endorsement",
                                         "Non-whiting midwater trawl","Groundfish fixed gear with trawl endorsement",
                                         "Groundfish fixed gear with fixed gear endorsement","Crab","Shrimp"))
    } else {
      datSub$VARIABLE <- factor(datSub$VARIABLE)
    }
    }else {
      datSub$VARIABLE <- factor(datSub$VARIABLE)
    }
  #datSub$con_flag <- max(datSub$con_flag,na.rm=T)
 
  datSub <- subset(datSub, is.na(datSub$VALUE)==F)
  datSub$VALUE <- as.numeric(datSub$VALUE)
  datSub$VARIANCE <- as.numeric(datSub$VARIANCE)
    
  validate(
    need(dim(datSub)[1]>0, 
         'Sorry, this plot could not be generated as no vessels matched your selections. 
          Try clicking the box to include all vessels that fished in Alaska or 
         include all vessels that fished for Pacific whiting.'),
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
#Only show plots if no variables are empty
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
  if(PermitPlot()){
    tags$div(class="actbutton",downloadButton("dlFigure", "Download Plot(s)",class = "btn btn-info"))
  }
})

output$resetButton <- renderUI({
  if(PermitPlot()){
    tags$div(class="actbutton",actionButton("reset_input", 
                                     HTML("<strong>Clear selections & <br> Return to Instructions</strong>"), 
                                          class="btn btn-info"))
  }
})

output$VCNRButton <- renderUI({
  if(PermitPlot()){
    if(input$DodgeSelect == "Composition of Total Cost Net Revenue"){
      HTML('<a class="btn btn-primary", href="TCNRGraphic.png" target="_blank" style="height:37px;
                position:absolute;bottom:170%;left:390%; background-color:RoyalBlue"> Explanation of this plot</a>')
    } else if(input$DodgeSelect == "Composition of Variable Cost Net Revenue"){
      HTML('<a class="btn btn-primary", href="VCNRGraphic.png" target="_blank" style="height:37px;
           position:absolute;bottom:170%;left:390%; background-color:RoyalBlue"> Explanation of this plot</a>')
    }  
  }
})

#The following set of lines creates a counter that counts every time the show data/show plots button is pushed
#the button label changes on whether the count is odd or even
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

