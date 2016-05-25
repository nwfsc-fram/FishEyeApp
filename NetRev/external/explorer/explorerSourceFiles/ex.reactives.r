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
                       SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"),
                       CATEGORY = c("Fisheries","Homeport","State","Vessel length class"),
                       FISHAK = unique(FISHAK),
                       whitingv = unique(whitingv),
                       STAT =  c("Average per vessel","Average per vessel/day","Average per vessel/metric-ton","Median per vessel","Median per vessel/day","Median per vessel/metric-ton","Fleet-wide total")#="Total"
                       
                  ))
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
  
  datSub$FISHAK <- ifelse(datSub$AK_FLAG==0, datSub$FISHAK, datSub$repFISHAK)
  datSub$whitingv <- ifelse(datSub$AK_FLAG==0, as.character(datSub$whitingv), datSub$repwhitingv)
  datSub <- datSub[,-c(which(colnames(datSub)=="repFISHAK"), which(colnames(datSub)=="repwhitingv"),which(colnames(dat)=="AK_FLAG"))]
  
  # order for plotting
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                              levels = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"))
  
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
 
  datSub$FISHAK <- ifelse(datSub$AK_FLAG==1, datSub$repFISHAK, datSub$FISHAK)
  datSub$whitingv <- ifelse(datSub$AK_FLAG==1, datSub$repwhitingv, as.character(datSub$whitingv))
  datSub <- datSub[,-c(which(colnames(datSub)=="repFISHAK"), which(colnames(datSub)=="repwhitingv"),which(colnames(dat)=="AK_FLAG"))]
  
  # order for plotting
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                              levels = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"))
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
  
    dat <- DatMain()      
      datSub <- subset(dat, YEAR %in% input$YearSelect &  
                         CATEGORY %in% input$CategorySelect &
                         VARIABLE %in% input$VariableSelect &
                         FISHAK == input$FishAkSelect &
                         whitingv == input$FishWhitingSelect &
                         STAT == input$StatSelect)

#subset the shortdescr data based on the type of plot chosen and then define the order for plotting
        if(input$DodgeSelect == "Economic measures side-by-side"){
          datSub <- subset(datSub, SHORTDESCR %in% input$ShortdescrSelect)
                datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, levels = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"))
          }
        if(input$DodgeSelect == "Composition of Total Cost Net Revenue"){
            datSub <- subset(datSub,  SHORTDESCR %in% c("Total Cost Net Revenue", "Fixed costs","Variable costs"))
                  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, levels=c("Total Cost Net Revenue","Fixed costs","Variable costs") )
          }
        if(input$DodgeSelect == "Composition of Variable Cost Net Revenue"){
            datSub <- subset(datSub,  SHORTDESCR %in% c("Variable Cost Net Revenue", "Variable costs"))  
                  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, levels=c("Variable Cost Net Revenue","Variable costs"))
          }
    
    
# for Homeport, state, and vessel length, subset the data by fisheries category (all fisheries, catch shares only, non-catch shares)
    if(input$CategorySelect != "Fisheries") {
        datSub <- subset(datSub, CS == input$inSelect)
    }
       
    datSub$VALUE <- as.numeric(datSub$VALUE)
#Define levels - this is for the order when plotting
    if(input$CategorySelect == "Homeport"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
    } else if(input$CategorySelect == "State"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
    } else if(input$CategorySelect == "Fisheries"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = c("All Fisheries","All Catch Share Fisheries","All Non-Catch Share Fisheries","At-sea Pacific whiting",                      
                                                            "Shoreside Pacific whiting","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
                                                            "Non-whiting midwater trawl","Groundfish fixed gear with trawl endorsement","Groundfish fixed gear with fixed gear endorsement",
                                                            "Crab","Shrimp"))
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
#    }
#    else {
#      datSub$star <- ""
#    }
    
      
    # This is my quick solution to a ggplot issue with level and facet grid. The dots are removed in a function at the end of doPlot.
    #The URL below is a solution for similar problem with stacking and may work for this issue. I have not yet tried.
    #https://github.com/hadley/ggplot2/issues/1301  #use website for dealing with stacked bar plot order issue
    if(input$CategorySelect=="Fisheries"){
      datSub$sort <- ifelse(datSub$VARIABLE=="All Fisheries", "......All Fisheries", as.character(datSub$VARIABLE))
      datSub$sort <- ifelse(datSub$VARIABLE=="All Catch Share Fisheries", ".....All Catch Share Fisheries", as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$VARIABLE=="All Non-Catch Share Fisheries", "..All Non-Catch Share Fisheries",  as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$VARIABLE=="At-sea Pacific whiting", ".....At-sea Pacific whiting",  as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$VARIABLE=="Shoreside Pacific whiting", ".....Shoreside Pacific whiting",  as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$VARIABLE=="DTS trawl with trawl endorsement", "....DTS trawl with trawl endorsement",  as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", "....Non-whiting, non-DTS trawl with trawl endorsement",  as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$VARIABLE=="Non-whiting midwater trawl","....Non-whiting midwater trawl" ,  as.character(datSub$sort))
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
           paste('Sorry, this plot could not be generated as no vessels matched your selections. 
                 Try clicking the box to include all vessels that fished in Alaska or include all vessels that fished for Pacific whiting. 
                 ')))
    
    
    return(datSub)
})



# create an additional subset for thirds plot
DatSubThirds <- reactive({
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

  if(input$CategorySelect != "Fisheries") {
    datSub <- subset(datSub, CS == input$inSelect)
  }
  
  #datSub <- subset(datSub, is.na(datSub$N)==F)
  datSub$THIRDS <- factor(datSub$THIRDS,
                          levels = factorOrder$thirds)
  datSub$THIRDS <- ifelse(datSub$THIRDS=="Bottom third", "Lower third", as.character(datSub$THIRDS))
  
  datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                              levels =  c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"))
  datSub$sort <- ifelse(datSub$SHORTDESCR=="Revenue", "...Revenue", as.character(datSub$SHORTDESCR))
  datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable costs", "..Variable costs", as.character(datSub$sort))
  datSub$sort <- ifelse(datSub$SHORTDESCR=="Fixed costs", ".Fixed costs",  as.character(datSub$sort))
  datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable Cost Net Revenue", ".Variable Cost Net Revenue",  as.character(datSub$sort))
  
  
  if(input$CategorySelect == "Homeport"){
    datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
  } else if(input$CategorySelect == "State"){
    datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
  } else if(input$CategorySelect == "Fisheries"){
    datSub$VARIABLE <- factor(datSub$VARIABLE, levels = c("All Fisheries","All Catch Share Fisheries","All Non-Catch Share Fisheries","At-sea Pacific whiting",                      
                                                            "Shoreside Pacific whiting","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
                                                            "Non-whiting midwater trawl","Groundfish fixed gear with trawl endorsement","Groundfish fixed gear with fixed gear endorsement",
                                                            "Crab","Shrimp"))
  }
  
#  datSub$VALUE <- ifelse(datSub$con_flag==1, "", datSub$VALUE)
#  datSub$star <- ifelse(datSub$con_flag==1, "*", "")
  datSub$con_flag <- max(datSub$con_flag)
  datSub <- subset(datSub, is.na(datSub$VALUE)==F)
  
  validate(
    need(dim(datSub)[1]>0, 
         paste('Sorry, this plot could not be generated as no vessels matched your selections. 
               Try clicking the box to include all vessels that fished in Alaska or include all vessels that fished for Pacific whiting. 
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
