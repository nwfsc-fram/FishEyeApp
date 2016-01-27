#This file handles the reactive expressions for data management and statistical operations.

# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file

DatMain <- reactive({ # data load moved to serverhead
  # data is loaded from serverHead.R load call
  dat <- netrevTable
  #dat <- subset(dat, YEAR<2014)
})

DatThirds <- reactive({
  dat <- netrevThirds
  #dat <- subset(dat, YEAR<2014)
})


DatVars <- reactive({
  # create a list of variable names used in the sidebar inputs
  dat <- DatMain()
  datVars <- with(dat, 
                  list(YEAR = unique(YEAR),
                       SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable cost net revenue","Total cost net revenue"),
                       CATEGORY = c("Fisheries","Homeport","State","Vessel length class"),
                       FISHAK = unique(FISHAK),
                       whitingv = unique(whitingv),
                       STAT =  c("Average per vessel","Average per vessel/day","Average per vessel/metric-ton","Median per vessel","Median per vessel/day","Median per vessel/metric-ton","Fleet-wide total")#="Total"
                       
                  ))
})


Variable <- reactive({
  dat <- DatMain()
  if(input$CategorySelect == "Fisheries"){
    variable = c("All Fisheries", "All Catch Share Fisheries","At-sea Pacific whiting","Shoreside Pacific whiting","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
                 "Groundfish fixed gear with trawl endorsement","Groundfish fixed gear with fixed gear endorsement",
                 "All Non-Catch Share Fisheries", "Crab","Shrimp","Other fisheries")
  } else if(input$CategorySelect == "State"){
    variable = factorOrder$state
  } else if(input$CategorySelect == "Homeport"){
    variable = factorOrder$port
  } else {
    variable = factorOrder$lengths
    #       subByCategory <- dat[dat$CATEGORY == input$CategorySelect,] 
  }
  return(variable)
})


# Subset data for table
# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSubTable <- reactive({
  #   if(is.null(input$DataButton) || input$DataButton == 0) return()
  #   input$ShortdescrSelect
  #   isolate(  
  #  if(!is.null(DatMain())  ){
  dat <- DatMain()      
  dat <- dat[-c(which(colnames(dat)=="AK_FLAG"),which(colnames(dat)=="con_flag"),which(colnames(dat)=="repFISHAK"),which(colnames(dat)=="repwhitingv"))]
  
  #subsetting
  datSub <- subset(dat, YEAR %in% input$YearSelect &  
                     SHORTDESCR %in% input$ShortdescrSelect & 
                     CATEGORY %in% input$CategorySelect &
                     VARIABLE %in% input$VariableSelect &
                     FISHAK == input$FishAkSelect &
                     whitingv == input$FishWhitingSelect &
                     STAT == input$StatSelect)
  
  datSub$VALUE <- as.numeric(datSub$VALUE)
  datSub$VARIANCE <- as.numeric(datSub$VARIANCE)
  
  # order for plotting
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                              levels = c("Revenue","Variable costs","Fixed costs","Variable cost net revenue","Total cost net revenue"))
  
  if(input$CategorySelect != "Fisheries") {
    datSub <- subset(datSub, CS == input$inSelect)
  }
  datSub$N <- ifelse(datSub$N<3, NA, datSub$N)
  datSub$N <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$N)
  datSub$VARIANCE <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$VARIANCE)
  datSub$FISHAK <- ifelse(datSub$FISHAK=="TRUE", "Vessels included", "Vessels not included")
  datSub$whitingv <- ifelse(datSub$whitingv=="TRUE", "Vessels included", "Vessels not included") 
  datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                      which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="FISHAK"),which(colnames(datSub)=="whitingv"),
                      which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
  validate(
    need(dim(datSub)[1]>0, #min(datSub$N)>2,
         paste('Sorry, this plot could not be generated as no vessels matched your selections. 
               Try clicking the box to include all vessels that fished in AK or include all vessels that fished for whiting. 
               ')))
  return(datSub)
  
  #  } else return()
  #   )
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
  dat <- dat[,-c(which(colnames(dat)=="AK_FLAG"),which(colnames(dat)=="con_flag"))]
  
  #subsetting
  datSub <- subset(dat, YEAR %in% input$YearSelect &  
                     SHORTDESCR %in% input$ShortdescrSelect & 
                     CATEGORY %in% input$CategorySelect &
                     VARIABLE %in% input$VariableSelect &
                     FISHAK == input$FishAkSelect &
                     whitingv == input$FishWhitingSelect &
                     STAT == input$StatSelect)
  
  datSub$VALUE <- as.numeric(datSub$VALUE)
  datSub$VARIANCE <- as.numeric(datSub$VARIANCE)
  
  # order for plotting
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                              levels = c("Revenue","Variable costs","Fixed costs","Variable cost net revenue","Total cost net revenue"))
  datSub$THIRDS <- ifelse(datSub$THIRDS=="Bottom third", "Lower third", as.character(datSub$THIRDS))
  
  if(input$CategorySelect != "Fisheries") {
    datSub <- subset(datSub, CS == input$inSelect)
  }
  datSub$N <- ifelse(datSub$N<3, NA, datSub$N)
  datSub$N <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$N)
  datSub$VARIANCE <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$VARIANCE)
  
  datSub$FISHAK <- ifelse(datSub$FISHAK=="TRUE", "Vessels included", "Vessels not included")
  datSub$whitingv <- ifelse(datSub$whitingv=="TRUE", "Vessels included", "Vessels not included") 
  datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                      which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="THIRDS"),which(colnames(datSub)=="FISHAK"),which(colnames(datSub)=="whitingv"),
                      which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
  
  validate(
    need(dim(datSub)[1]>0, #min(datSub$N)>2,
         paste('Sorry, this plot could not be generated as no vessels matched your selections. 
               Try clicking the box to include all vessels that fished in AK or include all vessels that fished for whiting. 
               ')))
  
  return(datSub)
  
})

# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSub <- reactive({
  #   if(is.null(input$DataButton) || input$DataButton == 0) return()
  #   input$ShortdescrSelect
  #   isolate(  
  if(input$DodgeSelect == "Economic measures side-by-side"){
    dat <- DatMain()      
    
    #       statSwitch <- switch(input$StatSelect,
    #         "Mean" = "mean",
    #         "Average" = "sum",
    #         "Average (per day)" = "mean per day",
    #         "Average (per metric ton)" = "mean per metric ton")
    
    #subsetting
    datSub <- subset(dat, YEAR %in% input$YearSelect &  
                       SHORTDESCR %in% input$ShortdescrSelect & 
                       CATEGORY %in% input$CategorySelect &
                       VARIABLE %in% input$VariableSelect &
                       FISHAK == input$FishAkSelect &
                       whitingv == input$FishWhitingSelect &
                       STAT == input$StatSelect)
    
    if(input$CategorySelect != "Fisheries") {
      datSub <- subset(datSub, CS == input$inSelect)
    }
    
    datSub$VALUE <- as.numeric(datSub$VALUE)
    
    # order for plotting
    datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                                levels = factorOrder$shortdescr)
    
    if(input$CategorySelect == "Homeport"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
    } else if(input$CategorySelect == "State"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
    } else if(input$CategorySelect == "Fisheries"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = c("All Fisheries",factorOrder$fisheries))
    }
    
    if(input$CategorySelect=="Fisheries" & length(input$VariableSelect)>1){
      datSub <- subset(datSub, datSub$VARIABLE!="Groundfish fixed gear with trawl endorsement"|datSub$YEAR!=2009)
    } 
    
    if(input$CategorySelect=="Fisheries"){
      datSub$sort <- ifelse(datSub$VARIABLE=="All Fisheries", ".....All Fisheries", as.character(datSub$VARIABLE))
      datSub$sort <- ifelse(datSub$VARIABLE=="All Catch Share Fisheries", "....All Catch Share Fisheries", as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$VARIABLE=="All Non-Catch Share Fisheries", "....All Non-Catch Share Fisheries",  as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$VARIABLE=="At-sea Pacific whiting", "....At-sea Pacific whiting",  as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$VARIABLE=="Shoreside Pacific whiting", "....Shoreside Pacific whiting",  as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$VARIABLE=="DTS trawl with trawl endorsement", "....DTS trawl with trawl endorsement",  as.character(datSub$sort))
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
      datSub$sort <- ifelse(datSub$VARIABLE=="Fort Bragg", ".Fort Brag", as.character(datSub$sort))                
      datSub$sort <- ifelse(datSub$VARIABLE=="San Francisco", ".San Francisco", as.character(datSub$sort))              
    }
    else {
      datSub$sort <- datSub$VARIABLE 
    }
    
    
    validate(
      need(max(datSub$con_flag)<1, #min(datSub$N)>2,
           paste('Sorry, this plot could not be generated as data from the following', datSub$CATEGORY[1],  '(s) and year(s) were suppressed:\n')),#grouping variable(s)
      need(max(datSub$con_flag)<1,
           paste(datSub$VARIABLE[which(datSub$con_flag==1&datSub$SHORTDESCR==datSub$SHORTDESCR[1])],
                 datSub$YEAR[which(datSub$con_flag==1&datSub$SHORTDESCR==datSub$SHORTDESCR[1])])),
      need(max(datSub$con_flag)<1, #need(min(datSub$N)>2,
           message=paste('
                         Data are suppressed when there are not enough observations to protect confidentiality. 
                         Try unclicking the indicated', datSub$CATEGORY[1], '(s) or year(s). If a', datSub$CATEGORY[1], 'or year is not provided, try clicking the box to include all vessels that fished in AK. There are less vessels that fished solely off the west coast than off the west coast and in Alaska.
                         ')))
    
    validate(
      need(dim(datSub)[1]>0, #min(datSub$N)>2,
           paste('Sorry, this plot could not be generated as no vessels matched your selections. 
                 Try clicking the box to include all vessels that fished in AK or include all vessels that fished for whiting. 
                 ')))
    
    #      validate(
    #        need(min(datSub$N)>2,
    #             message=paste('
    #             
    #Sorry, this plot could not be generated as data from \n', 
    #                           datSub$VARIABLE[which(datSub$N<3&datSub$SHORTDESCR==datSub$SHORTDESCR[1])],datSub$YEAR[which(datSub$N<3&datSub$SHORTDESCR==datSub$SHORTDESCR[1])],
    #            '\nwere suppressed to protect confidentiality. 
    #Data are suppressed when there are not enough observations to protect confidentiality. 
    #Try unclicking either the grouping variable (fishery, homeport, state, vessel length class) or year indicated on the first line of this message. 
    #If no grouping variable or year is provided, try clickng the box to include all vessels that fished in AK.
    #There are less vessels that fished solely off the west coast than off the west coast and in Alaska.
    #              '))
    #      ) 
    
    
    return(datSub)
  } else return()
  #   )
})


# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# This is only for when we choose the stacked plots
# build dcast formula using if controls and using the quoted method in dcast
DatSub2 <- reactive({
  #!is.null(DatMain()) & 
  if(input$DodgeSelect == "Composition of Total Cost Net Revenue"){
    dat <- DatMain()      
    
    
    #subsetting
    datSub2 <- subset(dat, YEAR %in% input$YearSelect &  
                        SHORTDESCR %in% c("Total cost net revenue","Fixed costs","Variable costs")   & 
                        CATEGORY %in% input$CategorySelect &
                        VARIABLE %in% input$VariableSelect &
                        FISHAK == input$FishAkSelect &
                        whitingv == input$FishWhitingSelect &
                        STAT == input$StatSelect)
    
    if(input$CategorySelect != "Fisheries") {
      datSub2 <- subset(datSub2, CS == input$inSelect)
    }
    
    # order for plotting
    datSub2$SHORTDESCR <- factor(datSub2$SHORTDESCR, 
                                 c("Total cost net revenue","Fixed costs","Variable costs") )
    
    #    
    if(input$CategorySelect == "Homeport"){
      datSub2$VARIABLE <- factor(datSub2$VARIABLE, levels = factorOrder$port)
      
    } else if(input$CategorySelect == "State"){
      datSub2$VARIABLE <- factor(datSub2$VARIABLE, levels = factorOrder$state)
    } else if(input$CategorySelect == "Fisheries"){
      datSub2$VARIABLE <- factor(datSub2$VARIABLE, levels = c("All Fisheries",factorOrder$fisheries))
    }
    
    if(input$CategorySelect=="Fisheries" & length(input$VariableSelect)>1){
      datSub2 <- subset(datSub2, datSub2$VARIABLE!="Groundfish fixed gear with trawl endorsement"|datSub2$YEAR!=2009)
    }
    
    if(input$CategorySelect=="Fisheries"){
      datSub2$sort <- ifelse(datSub2$VARIABLE=="All Fisheries", ".....All Fisheries", as.character(datSub2$VARIABLE))
      datSub2$sort <- ifelse(datSub2$VARIABLE=="All Catch Share Fisheries", "....All Catch Share Fisheries", as.character(datSub2$sort))
      datSub2$sort <- ifelse(datSub2$VARIABLE=="All Non-Catch Share Fisheries", "....All Non-Catch Share Fisheries",  as.character(datSub2$sort))
      datSub2$sort <- ifelse(datSub2$VARIABLE=="At-sea Pacific whiting", "....At-sea Pacific whiting",  as.character(datSub2$sort))
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Shoreside Pacific whiting", "....Shoreside Pacific whiting",  as.character(datSub2$sort))
      datSub2$sort <- ifelse(datSub2$VARIABLE=="DTS trawl with trawl endorsement", "....DTS trawl with trawl endorsement",  as.character(datSub2$sort))
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", "....Non-whiting, non-DTS trawl with trawl endorsement",  as.character(datSub2$sort))
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Groundfish fixed gear with trawl endorsement", "...Groundfish fixed gear with trawl endorsement",  as.character(datSub2$sort))
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Groundfish fixed gear with fixed gear endorsement", "..Groundfish fixed gear with fixed gear endorsement",  as.character(datSub2$sort))
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Crab", ".Crab",  as.character(datSub2$sort))
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Shrimp", ".Shrimp",  as.character(datSub2$sort))
    } else if(input$CategorySelect == "Homeport") {
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Puget Sound", ".....Puget Sound", as.character(datSub2$VARIABLE))                
      datSub2$sort <- ifelse(datSub2$VARIABLE=="South and central WA coast", ".....South and central WA coast", as.character(datSub2$sort)) 
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Astoria", "....Astoria", as.character(datSub2$sort))                    
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Tillamook", "....Tillamook", as.character(datSub2$sort))                  
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Newport", "...Newport", as.character(datSub2$sort))                   
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Coos Bay","..Coos Bay", as.character(datSub2$sort))                   
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Brookings", ".Brookings", as.character(datSub2$sort))                  
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Crescent City", ".Crescent City", as.character(datSub2$sort))              
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Eureka", ".Eureka", as.character(datSub2$sort))                     
      datSub2$sort <- ifelse(datSub2$VARIABLE=="Fort Bragg", ".Fort Brag", as.character(datSub2$sort))                
      datSub2$sort <- ifelse(datSub2$VARIABLE=="San Francisco", ".San Francisco", as.character(datSub2$sort))              
    }
    else {
      datSub2$sort <- datSub2$VARIABLE 
    }
    
    validate(
      need(max(datSub2$con_flag)<1,
           paste('Sorry, this plot could not be generated as data from the following', datSub2$CATEGORY[1],  '(s) and year(s) were suppressed:\n')),#grouping variable(s)
      need(max(datSub2$con_flag)<1,
           paste(datSub2$VARIABLE[which(datSub2$con_flag==1&datSub2$SHORTDESCR==datSub2$SHORTDESCR[1])],datSub2$YEAR[which(datSub2$con_flag==1&datSub2$SHORTDESCR==datSub2$SHORTDESCR[1])])),
      need(max(datSub2$con_flag)<1,
           message=paste('
                         Data are suppressed when there are not enough observations to protect confidentiality. 
                         Try unclicking the indicated', datSub2$CATEGORY[1], '(s) or year(s). If a', datSub2$CATEGORY[1], 'or year is not provided, try clicking the box to include all vessels that fished in AK. There are less vessels that fished solely off the west coast than off the west coast and in Alaska.
                         ')))
    
    validate(
      need(dim(datSub2)[1]>0, #min(datSub$N)>2,
           paste('Sorry, this plot could not be generated as no vessels matched your selections. 
                 Try clicking the box to include all vessels that fished in AK or include all vessels that fished for whiting. 
                 ')))
    return(datSub2)
    
  } 
  
})

DatSub3 <- reactive({
  
  if(input$DodgeSelect == "Composition of Variable Cost Net Revenue"){#!is.null(DatMain()) & 
    dat <- DatMain()      
    
    
    #subsetting
    datSub3 <- subset(dat, YEAR %in% input$YearSelect &  
                        SHORTDESCR %in% c("Variable cost net revenue","Variable costs")   & 
                        CATEGORY %in% input$CategorySelect &
                        VARIABLE %in% input$VariableSelect &
                        FISHAK == input$FishAkSelect &
                        whitingv == input$FishWhitingSelect &
                        STAT == input$StatSelect)
    
    if(input$CategorySelect != "Fisheries") {
      datSub3 <- subset(datSub3, CS == input$inSelect)
    }
    
    # order for plotting
    datSub3$SHORTDESCR <- factor(datSub3$SHORTDESCR, 
                                 levels =  c("Variable cost net revenue","Variable costs") )
    
    #    
    if(input$CategorySelect == "Homeport"){
      datSub3$VARIABLE <- factor(datSub3$VARIABLE, levels = factorOrder$port)
      
    } else if(input$CategorySelect == "State"){
      datSub3$VARIABLE <- factor(datSub3$VARIABLE, levels = factorOrder$state)
    } else if(input$CategorySelect == "Fisheries"){
      datSub3$VARIABLE <- factor(datSub3$VARIABLE, levels = c("All Fisheries",factorOrder$fisheries))
    }
    
    if(input$CategorySelect=="Fisheries" & length(input$VariableSelect)>1){
      datSub3 <- subset(datSub3, datSub3$VARIABLE!="Groundfish fixed gear with trawl endorsement"|datSub3$YEAR!=2009)
    }
    if(input$CategorySelect=="Fisheries"){
      datSub3$sort <- ifelse(datSub3$VARIABLE=="All Fisheries", ".....All Fisheries", as.character(datSub3$VARIABLE))
      datSub3$sort <- ifelse(datSub3$VARIABLE=="All Catch Share Fisheries", "....All Catch Share Fisheries", as.character(datSub3$sort))
      datSub3$sort <- ifelse(datSub3$VARIABLE=="All Non-Catch Share Fisheries", "....All Non-Catch Share Fisheries",  as.character(datSub3$sort))
      datSub3$sort <- ifelse(datSub3$VARIABLE=="At-sea Pacific whiting", "....At-sea Pacific whiting",  as.character(datSub3$sort))
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Shoreside Pacific whiting", "....Shoreside Pacific whiting",  as.character(datSub3$sort))
      datSub3$sort <- ifelse(datSub3$VARIABLE=="DTS trawl with trawl endorsement", "....DTS trawl with trawl endorsement",  as.character(datSub3$sort))
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", "....Non-whiting, non-DTS trawl with trawl endorsement",  as.character(datSub3$sort))
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Groundfish fixed gear with trawl endorsement", "...Groundfish fixed gear with trawl endorsement",  as.character(datSub3$sort))
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Groundfish fixed gear with fixed gear endorsement", "..Groundfish fixed gear with fixed gear endorsement",  as.character(datSub3$sort))
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Crab", ".Crab",  as.character(datSub3$sort))
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Shrimp", ".Shrimp",  as.character(datSub3$sort))
    } else if(input$CategorySelect == "Homeport") {
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Puget Sound", ".....Puget Sound", as.character(datSub3$VARIABLE))                
      datSub3$sort <- ifelse(datSub3$VARIABLE=="South and central WA coast", ".....South and central WA coast", as.character(datSub3$sort)) 
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Astoria", "....Astoria", as.character(datSub3$sort))                    
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Tillamook", "....Tillamook", as.character(datSub3$sort))                  
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Newport", "...Newport", as.character(datSub3$sort))                   
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Coos Bay","..Coos Bay", as.character(datSub3$sort))                   
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Brookings", ".Brookings", as.character(datSub3$sort))                  
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Crescent City", ".Crescent City", as.character(datSub3$sort))              
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Eureka", ".Eureka", as.character(datSub3$sort))                     
      datSub3$sort <- ifelse(datSub3$VARIABLE=="Fort Bragg", ".Fort Brag", as.character(datSub3$sort))                
      datSub3$sort <- ifelse(datSub3$VARIABLE=="San Francisco", ".San Francisco", as.character(datSub3$sort))              
    }
    else {
      datSub3$sort <- datSub3$VARIABLE 
    }
    
    
    validate(
      need(max(datSub3$con_flag)<1,
           paste('Sorry, this plot could not be generated as data from the following', datSub3$CATEGORY[1],  '(s) and year(s) were suppressed:\n')),#grouping variable(s)
      need(max(datSub3$con_flag)<1,
           paste(datSub3$VARIABLE[which(datSub3$con_flag==1&datSub3$SHORTDESCR==datSub3$SHORTDESCR[1])],datSub3$YEAR[which(datSub3$con_flag==1&datSub3$SHORTDESCR==datSub3$SHORTDESCR[1])])),
      need(max(datSub3$con_flag)<1,
           message=paste('
                         Data are suppressed when there are not enough observations to protect confidentiality. 
                         Try unclicking the indicated', datSub3$CATEGORY[1], '(s) or year(s). If a', datSub3$CATEGORY[1], 'or year is not provided, try clicking the box to include all vessels that fished in AK. There are less vessels that fished solely off the west coast than off the west coast and in Alaska.
                         ')))
    
    validate(
      need(dim(datSub3)[1]>0, #min(datSub$N)>2,
           paste('Sorry, this plot could not be generated as no vessels matched your selections. 
                 Try clicking the box to include all vessels that fished in AK or include all vessels that fished for whiting. 
                 ')))
    return(datSub3)
    
  } else return()
  
})

# create an additional subset for thirds plot...this is where OO would be handy
DatSubThirds <- reactive({
  #if(!is.null(DatThirds())){
  
  dat <- DatThirds()
  
  #subsetting
  datSub <- subset(dat, YEAR %in% input$YearSelect &
                     SHORTDESCR %in% input$ShortdescrSelect &
                     CATEGORY %in% input$CategorySelect &
                     VARIABLE %in% input$VariableSelect &
                     FISHAK == input$FishAkSelect &
                     whitingv == input$FishWhitingSelect &
                     STAT == input$StatSelect
  )
  #     
  #    datSub$flag <- 0 
  if(input$CategorySelect != "Fisheries") {
    datSub <- subset(datSub, CS == input$inSelect)
  }
  
  #datSub <- subset(datSub, is.na(datSub$N)==F)
  datSub$THIRDS <- factor(datSub$THIRDS,
                          levels = factorOrder$thirds)
  datSub$THIRDS <- ifelse(datSub$THIRDS=="Bottom third", "Lower third", as.character(datSub$THIRDS))
  
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                              levels =  factorOrder$shortdescr)
  datSub$sort <- ifelse(datSub$SHORTDESCR=="Revenue", "...Revenue", as.character(datSub$SHORTDESCR))
  datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable costs", "..Variable costs", as.character(datSub$sort))
  datSub$sort <- ifelse(datSub$SHORTDESCR=="Fixed costs", ".Fixed costs",  as.character(datSub$sort))
  datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable cost net revenue", ".Variable cost net revenue",  as.character(datSub$sort))
  
  
  if(input$CategorySelect == "Homeport"){
    datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
  } else if(input$CategorySelect == "State"){
    datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
  } else if(input$CategorySelect == "Fisheries"){
    datSub$VARIABLE <- factor(datSub$VARIABLE, levels = c("All Fisheries",factorOrder$fisheries))
  }
  
  
  
  validate(
    need(dim(datSub)[1]>0, 
         paste('Sorry, this plot could not be generated as no vessels matched your selections. 
               Try clicking the box to include all vessels that fished in AK or include all vessels that fished for whiting. 
               ')))
  
  
  
  validate(
    need(max(datSub$con_flag)<1,
         paste('Sorry, this plot could not be generated as data from the following', datSub$CATEGORY[1],  '(s) and year(s) were suppressed:\n')),#grouping variable(s)
    need(max(datSub$con_flag)<1,
         paste(datSub$VARIABLE[which(datSub$con_flag==1&datSub$SHORTDESCR==datSub$SHORTDESCR[1]&datSub$THIRDS==datSub$THIRDS[1])],
               datSub$YEAR[which(datSub$con_flag==1&datSub$SHORTDESCR==datSub$SHORTDESCR[1]&datSub$THIRDS==datSub$THIRDS[1])])),
    need(max(datSub$con_flag)<1,
         message=paste('
                       Data are suppressed when there are not enough observations to protect confidentiality. 
                       Try unclicking the indicated', datSub$CATEGORY[1], '(s) or year(s). If a', datSub$CATEGORY[1], 'or year is not provided, try clicking the box to include all vessels that fished in AK. There are less vessels that fished solely off the west coast than off the west coast and in Alaska.
                       ')))
  
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
      if(any(grepl("Groundfish fixed gear with trawl endorsement",input$VariableSelect)) #input$selectall2!=0 & input$selectall2%%2==1 input$YearSelect[1]==2009 &
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

