#======================================
#
# this page  
#  1. calls the reactive expressions in ex.io.sidebar1
#  2. handles the reactive expressions for data management and statistical operations.
#  3. 
#======================================
currentyear<- 2015

# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file
DatMain <- reactive({ # data load moved to serverhead
  # data is loaded from serverHead.R load call
  if(input$Sect_sel=="CV"){
    dat <- CVcosts
  } else if(input$Sect_sel=="M"){
    dat <- MScosts
  } else if(input$Sect_sel=="CP"){
    dat <- CPcosts
  } else if(input$Sect_sel=="FR"){
    dat <- FRcosts
  } 
})



DatVars <- reactive({
  # create a list of variable names used in the sidebar inputs
  dat <- DatMain()
  if(input$Sect_sel=="CV"){
    datVars <- with(dat, 
                  list(YEAR = 2009:currentyear,
                       SHORTDESCR = c('All variable costs','Buyback fees','Captain','Cost recovery fees','Crew','Fuel','Observers', 'Other variable costs',
                                      'All fixed costs','Fishing gear','On-board equipment','Other fixed costs'),
                       CATEGORY = c("Fisheries","Homeport","State of homeport"="State","Vessel length class"),
                       FISHAK = '',#unique(FISHAK),
                       whitingv = c("All vessels","Non-whiting vessels","Whiting vessels"),
                       STAT =  c("Average per vessel","Average per vessel/day","Average per vessel/metric-ton caught","Median per vessel","Median per vessel/day","Median per vessel/metric-ton caught",
                                 "Fleet-wide total","Fleet-wide total/day","Fleet-wide total/metric-ton caught")
                   ))
  } else if(input$Sect_sel=="FR"){
    datVars <- with(dat, 
                    list(YEAR =  2009:currentyear,
                         SHORTDESCR = c("All variable costs",'Fish purchases','Freight','Labor','Monitoring','Off-site freezing & storage','Packing materials','Utilities','Other variable costs',
                                        "All fixed costs",'Buildings','Equipment','Other fixed costs'),
                         whitingv = c("All processors","Whiting processors","Non-whiting processors"),
                         CATEGORY = c("Production activities","Region","Processor size"),
                         STAT =  c("Average per processor","Average per processor/metric-ton of groundfish products produced"="Average per processor/metric-ton produced",
                                   "Median per processor", "Median per processor/metric-ton of groundfish products produced"="Median per processor/metric-ton produced",
                                   "Industry-wide total","Industry-wide total/metric-ton of groundfish products produced"="Industry-wide total/metric-ton produced")
                    ))
  } else if(input$Sect_sel=='M') {
    datVars <- with(dat, 
                    list(YEAR =  2009:currentyear,
                         SHORTDESCR = c("All variable costs","Fish purchases","Fuel","Non-processing crew","Observers","Processing crew","Other variable costs",
                                        "All fixed costs","Fishing gear","On-board equipment","Processing equipment",'Other fixed costs'),
                         whitingv = c("Whiting vessels"),
                         CATEGORY = c("Fisheries"),
                         STAT =  c("Average per vessel","Average per vessel/day","Average per vessel/metric-ton produced","Median per vessel","Median per vessel/day","Median per vessel/metric-ton produced",
                                   "Fleet-wide total","Fleet-wide total/day","Fleet-wide total/metric-ton produced")
                         
                    ))
  } else {
    datVars <- with(dat, 
                    list(YEAR =  2009:currentyear,
                         SHORTDESCR = c("All variable costs",'Cost recovery fees', "Fuel","Non-processing crew","Observers","Processing crew","Other variable costs",
                                        "All fixed costs","Fishing gear","On-board equipment","Processing equipment",'Other fixed costs'),
                         whitingv = c("Whiting vessels"),
                         CATEGORY = c("Fisheries"),
                         STAT =  c("Average per vessel","Average per vessel/day","Average per vessel/metric-ton produced","Median per vessel","Median per vessel/day","Median per vessel/metric-ton produced",
                                   "Fleet-wide total","Fleet-wide total/day","Fleet-wide total/metric-ton produced")
                    ))
  }
})




# Subset data for table
# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSubTable <- reactive({

  dat <- DatMain()      
#  dat <- dat[-c(which(colnames(dat)=="con_flag"),which(colnames(dat)=="flag"))]
 
  #subsetting
  datSub <- with(dat, dat[which(SHORTDESCR %in% input$ShortdescrSelect & 
                     CATEGORY %in% input$CategorySelect &
                     VARIABLE %in% input$VariableSelect &
                     STAT == input$StatSelect&
                     whitingv == input$FishWhitingSelect),])

 
    datSub$SHORTDESCR <- if(input$Sect_sel=="CV"){
                                                  factor(datSub$SHORTDESCR, levels=c('All variable costs','Buyback fees','Captain','Cost recovery fees',
                                                          'Crew','Fuel','Observers','Other variable costs',
                                                          'All fixed costs','Fishing gear','On-board equipment','Other fixed costs'))
                         } else if(input$Sect_sel=="M"){
                                                  factor(datSub$SHORTDESCR, levels=c("All variable costs","Fish purchases","Fuel","Non-processing crew","Observers","Processing crew","Other variable costs",
                                                                                     "All fixed costs","Fishing gear","On-board equipment","Processing equipment",'Other fixed costs'))
                         } else if(input$Sect_sel=='CP'){ 
                                                  factor(datSub$SHORTDESCR, levels=c("All variable costs",'Cost recovery fees', "Fish purchases","Fuel","Non-processing crew","Observers","Processing crew","Other variable costs",
                                                              "All fixed costs","Fishing gear","On-board equipment","Processing equipment",'Other fixed costs'))
                         } else {
                                                  factor(datSub$SHORTDESCR, levels=c("All variable costs",'Fish purchases','Freight','Labor','Monitoring','Off-site freezing & storage','Packing materials','Utilities','Other variable costs',
                                                                                     "All fixed costs",'Buildings','Equipment','Other fixed costs'))
                                                  }

  if(input$Sect_sel=="CV"){
    datSub <- with(datSub, datSub[which(YEAR %in% input$YearSelect #&  
                                        #FISHAK == input$FishAkSelect &
                                        #whitingv == input$FishWhitingSelect
                                        ),])
  #datSub$FISHAK <- ifelse(datSub$AK_FLAG==0, datSub$FISHAK, datSub$repFISHAK)
  #datSub$whitingv <- ifelse(datSub$AK_FLAG==0, as.character(datSub$whitingv), datSub$repwhitingv)
  datSub <- datSub[,-c(#which(colnames(datSub)=="repFISHAK"), which(colnames(datSub)=="repwhitingv"),
                        which(colnames(dat)=="AK_FLAG"))]
  } else if(input$Sect_sel=="FR") {
    datSub <- subset(datSub, whitingv == input$FishWhitingSelect & #ACS == input$ProductionSelect &
                             YEAR %in% input$YearSelect2) 
      } else{
    datSub <- subset(datSub, YEAR %in% input$YearSelect2 ) 
      }
  
  datSub$VALUE <- round(as.numeric(datSub$VALUE),0)
  datSub$VARIANCE <- round(as.numeric(datSub$VARIANCE),0)

  if(input$Sect_sel=='CV' & input$CategorySelect != "Fisheries" || input$Sect_sel=='FR' & input$CategorySelect != "Production activities") {
    datSub <- subset(datSub, CS == input$inSelect)
  }
  
  datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
#  datSub$N <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$N)
  datSub$VARIANCE <- ifelse(datSub$N<3, NA, datSub$VARIANCE)
  
  if(input$Sect_sel=="CV"){
  #datSub$FISHAK <- ifelse(datSub$FISHAK=="TRUE", "Vessels included", "Vessels not included")
  #datSub$whitingv <- ifelse(datSub$whitingv=="TRUE", "Vessels included", "Vessels not included") 
  datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                      which(colnames(datSub)=="SHORTDESCR"),#which(colnames(datSub)=="FISHAK"),
                      which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
  } else if(input$Sect_sel=='FR'){
    datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),#which(colnames(datSub)=="ACS"),
                        which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                        which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
  } 
  else {
    datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),#which(colnames(datSub)=="CS"),
                        which(colnames(datSub)=="STAT"),
                        which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
  }
  
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
   
      datSub <- subset(dat, SHORTDESCR %in% input$ShortdescrSelect &
                            CATEGORY == input$CategorySelect &
                            VARIABLE %in% input$VariableSelect &
                            STAT == input$StatSelect&
                            whitingv == input$FishWhitingSelect)
      
      
      if(input$Sect_sel=="CV"){
        if(input$PlotSelect=='Stacked bar'& input$ShortdescrSelect[2] %in% c('Buyback fees','Buildings','Cost recovery fees','Crew','Equipment',"Fish purchases",'Fishing gear','Freight','Fuel', 'Captain','Labor','Monitoring',"Non-processing crew",'Observers','On-board equipment','Other fixed costs','Other variable costs','Packing materials','Utilities',
                                                                        'Off-site freezing & storage',"Processing crew", "Processing equipment")|
           input$PlotSelect=='Stacked bar'&input$ShortdescrSelect[3] %in% c('Buyback fees','Buildings','Cost recovery fees','Crew','Equipment',"Fish purchases",'Fishing gear','Freight','Fuel', 'Captain','Labor','Monitoring',"Non-processing crew",'Observers','On-board equipment','Other fixed costs','Other variable costs','Packing materials','Utilities',
                                                                            'Off-site freezing & storage',"Processing crew", "Processing equipment")#'Buildings','Cost recovery fees','Crew','Equipment',"Fish purchases",'Fishing gear','Freight','Fuel', 
                                                                       # )
        ){
          datSub <- subset(datSub,  YEAR %in% input$YearSelect & !SHORTDESCR %in% c('All variable costs','All fixed costs'))                                                                 
        } else{
          datSub <- subset(datSub,  YEAR %in% input$YearSelect)
      }
                                  #FISHAK == input$FishAkSelect #&
                                  #whitingv == input$FishWhitingSelect
      } else if(input$Sect_sel=="FR") {
        if(input$PlotSelect=='Stacked bar'& input$ShortdescrSelect[2] %in% c('Buyback fees','Buildings','Cost recovery fees','Crew','Equipment',"Fish purchases",'Fishing gear','Freight','Fuel', 'Captain','Labor','Monitoring',"Non-processing crew",'Observers','On-board equipment','Other fixed costs','Other variable costs','Packing materials','Utilities',
                                                                             'Off-site freezing & storage',"Processing crew", "Processing equipment")|
           input$PlotSelect=='Stacked bar'&input$ShortdescrSelect[3] %in% c('Buyback fees','Buildings','Cost recovery fees','Crew','Equipment',"Fish purchases",'Fishing gear','Freight','Fuel', 'Captain','Labor','Monitoring',"Non-processing crew",'Observers','On-board equipment','Other fixed costs','Other variable costs','Packing materials','Utilities',
                                                                            'Off-site freezing & storage',"Processing crew", "Processing equipment")#'Buildings','Cost recovery fees','Crew','Equipment',"Fish purchases",'Fishing gear','Freight','Fuel', 
           # )
        ){
        datSub <- subset(datSub, whitingv == input$FishWhitingSelect & #ACS == input$ProductionSelect &
                           YEAR %in% input$YearSelect2& !SHORTDESCR %in% c('All variable costs','All fixed costs'))
        } else {
          datSub <- subset(datSub, whitingv == input$FishWhitingSelect & #ACS == input$ProductionSelect &
                             YEAR %in% input$YearSelect2)
        }
      } else {
        
        if(input$PlotSelect=='Stacked bar'& input$ShortdescrSelect[2] %in% c('Buyback fees','Buildings','Cost recovery fees','Crew','Equipment',"Fish purchases",'Fishing gear','Freight','Fuel', 'Captain','Labor','Monitoring',"Non-processing crew",'Observers','On-board equipment','Other fixed costs','Other variable costs','Packing materials','Utilities',
                                                                             'Off-site freezing & storage',"Processing crew", "Processing equipment")|
           input$PlotSelect=='Stacked bar'&input$ShortdescrSelect[3] %in% c('Buyback fees','Buildings','Cost recovery fees','Crew','Equipment',"Fish purchases",'Fishing gear','Freight','Fuel', 'Captain','Labor','Monitoring',"Non-processing crew",'Observers','On-board equipment','Other fixed costs','Other variable costs','Packing materials','Utilities',
                                                                            'Off-site freezing & storage',"Processing crew", "Processing equipment")#'Buildings','Cost recovery fees','Crew','Equipment',"Fish purchases",'Fishing gear','Freight','Fuel', 
           # )
        ){
        datSub <- subset(datSub, YEAR %in% input$YearSelect2 & !SHORTDESCR %in% c('All variable costs','All fixed costs'))  
        } else {
          datSub <- subset(datSub, YEAR %in% input$YearSelect2 )   
      }
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
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = c("All fisheries","All catch share fisheries","All non-catch share fisheries","Pacific whiting","At-sea Pacific whiting",                      
                                                            "Shoreside Pacific whiting","Groundfish with trawl gear","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
                                                            "Non-whiting midwater trawl","Groundfish fixed gear with trawl endorsement","Groundfish fixed gear with fixed gear endorsement",
                                                            "Crab","Shrimp"))
      }else{
        datSub$VARIABLE <- factor(datSub$VARIABLE)
      }
    }
    
    datSub$SHORTDESCR <- if(input$Sect_sel=="CV"){
      factor(datSub$SHORTDESCR, levels=c('All variable costs','Buyback fees','Captain','Cost recovery fees',
                                         'Crew','Fuel','Observers','Other variable costs',
                                         'All fixed costs','Fishing gear','On-board equipment','Other fixed costs'))
    } else if(input$Sect_sel=="M"){
      factor(datSub$SHORTDESCR, levels=c("All variable costs","Fish purchases","Fuel","Non-processing crew","Observers","Processing crew","Other variable costs",
                                         "All fixed costs","Fishing gear","On-board equipment","Processing equipment",'Other fixed costs'))
    } else if(input$Sect_sel=='CP'){ 
      factor(datSub$SHORTDESCR, levels=c("All variable costs",'Cost recovery fees', "Fish purchases","Fuel","Non-processing crew","Observers","Processing crew","Other variable costs",
                                         "All fixed costs","Fishing gear","On-board equipment","Processing equipment",'Other fixed costs'))
    } else {
      factor(datSub$SHORTDESCR, levels=c("All variable costs",'Fish purchases','Freight','Labor','Monitoring','Off-site freezing & storage','Packing materials','Utilities','Other variable costs',
                                         "All fixed costs",'Buildings','Equipment','Other fixed costs'))
    }
   

    
#      datSub$VALUE <- ifelse(datSub$flag==1, 0, datSub$VALUE)
#      datSub$star <- ifelse(datSub$flag==1, "*", "")
#      datSub$con_flag <- ifelse(datSub$con_flag==1, 0, datSub$con_flag)
      datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
      datSub$VARIANCE <- ifelse(datSub$N<3, NA, datSub$VARIANCE)
      

      
    # This is my quick solution to a ggplot issue with level and facet grid. The dots are removed in a function at the end of doPlot.
    #The URL below is a solution for similar problem with stacking and may work for this issue. I have not yet tried.
    #https://github.com/hadley/ggplot2/issues/1301  #use website for dealing with stacked bar plot order issue
    if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
      datSub$sort <- ifelse(datSub$VARIABLE=="All fisheries", ".......All fisheries", 
                            ifelse(datSub$VARIABLE=="All catch share fisheries", "......All catch share fisheries", 
                                   ifelse(datSub$VARIABLE=="All non-catch share fisheries", "..All non-catch share fisheries", 
                                          ifelse(datSub$VARIABLE=="Pacific whiting", "......Pacific whiting",
                                                ifelse(datSub$VARIABLE=="At-sea Pacific whiting", "......At-sea Pacific whiting", 
                                                      ifelse(datSub$VARIABLE=="Shoreside Pacific whiting", "......Shoreside Pacific whiting",  
                                                             ifelse(datSub$VARIABLE=="Groundfish with trawl gear", ".....Groundfish with trawl gear", 
                                                                    ifelse(datSub$VARIABLE=="DTS trawl with trawl endorsement", "....DTS trawl with trawl endorsement", 
                                                                        ifelse(datSub$VARIABLE=="Non-whiting, non-DTS trawl with trawl endorsement", "....Non-whiting, non-DTS trawl with trawl endorsement",
                                                                            ifelse(datSub$VARIABLE=="Non-whiting midwater trawl","....Non-whiting midwater trawl",  
                                                                                  ifelse(datSub$VARIABLE=="Groundfish fixed gear with trawl endorsement", "...Groundfish fixed gear with trawl endorsement",
                                                                                    ifelse(datSub$VARIABLE=="Groundfish fixed gear with fixed gear endorsement", "..Groundfish fixed gear with fixed gear endorsement",  
                                                                                           ifelse(datSub$VARIABLE=="Crab", ".Crab", 
                                                                                                  ifelse(datSub$VARIABLE=="Shrimp", ".Shrimp",  as.character(datSub$VARIABLE)
                                                                                                  ))))))))))))))
    } else if(input$CategorySelect == "Homeport") {
      datSub$sort <- ifelse(datSub$VARIABLE=="Puget Sound", ".....Puget Sound",              
                            ifelse(datSub$VARIABLE=="South and central WA coast", ".....South and central WA coast", 
                                    ifelse(datSub$VARIABLE=="Astoria", "....Astoria",               
                                           ifelse(datSub$VARIABLE=="Tillamook", "....Tillamook",           
                                                  ifelse(datSub$VARIABLE=="Newport", "...Newport",              
                                                         ifelse(datSub$VARIABLE=="Coos Bay","..Coos Bay",                 
                                                                ifelse(datSub$VARIABLE=="Brookings", ".Brookings",                
                                                                       ifelse(datSub$VARIABLE=="Crescent City", ".Crescent City",              
                                                                              ifelse(datSub$VARIABLE=="Eureka", ".Eureka",                     
                                                                                     ifelse(datSub$VARIABLE=="Fort Bragg", ".Fort Bragg",               
                                                                                            ifelse(datSub$VARIABLE=="San Francisco", ".San Francisco", as.character(datSub$VARIABLE))
                                                                                     ))))))))))
    } else if(input$CategorySelect == "Production activities") {
      datSub$sort <- ifelse(datSub$VARIABLE=='All production', '.All production',
                            ifelse(datSub$VARIABLE=='Groundfish production', '.Groundfish production',
                                   ifelse(datSub$VARIABLE=='Pacific whiting production', '.Pacific whiting production', as.character(datSub$VARIABLE))))
    }
    else {
      datSub$sort <- datSub$VARIABLE 
    }
 print(datSub[1:5,])
      
    validate(
      need(dim(datSub)[1]>0 & max(datSub$N)>2,
           'Sorry, this plot could not be generated as no vessels or processors matched your selections. 
                 If you have selected to show data summed across non-whiting vessels or whiting vessels, try selecting to include all vessels. 
                 '))
    
    
    return(datSub)
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

