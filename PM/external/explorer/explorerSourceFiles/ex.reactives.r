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
  } else{ #(input$Sect_sel=="FR")
    dat <- FRperfmetrics
  } 
})


DatVars <- reactive({
  # create a list of variable names used in the sidebar inputs
  dat <- DatMain()
  datVars <- with(dat, 
    list(
       YEAR = 2004:2014,
      SHORTDESCR = c("Revenue","Variable costs","Fixed costs","Variable Cost Net Revenue","Total Cost Net Revenue"),
       CATEGORY = c("Fisheries","Homeport","State","Vessel length class"),
      FISHAK = unique(FISHAK),
      whitingv = c("All vessels","Non-whiting vessels","Whiting vessels"),
      STAT =  c("Average per vessel","Average per vessel/day","Average per vessel/metric-ton","Median per vessel","Median per vessel/day","Median per vessel/metric-ton","Fleet-wide total","Fleet-wide total/day","Fleet-wide total/metric-ton"),
      METRIC =  c("Number of vessels or processors"="Number of vessels","Vessel length","Fishery participation","Exponential Shannon Index","Proportion of revenue from Catch Share fishery"="Proportion of revenue from CS fishery",
                  "Days at sea","Gini coefficient","Herfindahl-Hirschman Index","Number of positions (captain and crew)"='Number of positions',"Crew wage","Revenue per crew day",
                  "Date 50% of total catch landed"="Date 50 percent of total catch landed","Share of landings by state")

      
  ))
})



# Subset data for table
# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSubTable <- reactive({
    dat <- DatMain()      
#    dat <- dat[-c(which(colnames(dat)=="AK_FLAG"),which(colnames(dat)=="con_flag"),which(colnames(dat)=="repFISHAK"),which(colnames(dat)=="repwhitingv"))]
    
    #subsetting
    datSub <- subset(dat, YEAR %in% input$YearSelect &  
                       CATEGORY == input$CategorySelect &
                       VARIABLE %in% input$VariableSelect &
                       whitingv %in% input$FishWhitingSelect)
 
       if(input$Ind_sel=="Economic") {
         datSub <- subset(datSub,  SHORTDESCR %in% input$ShortdescrSelect &
                                    STAT == input$StatSelect)
    } 
#    if(input$Ind_sel!="Economic")  {
#      if(input$MetricSelect!="Number of vessels"&input$MetricSelect!="date"&input$MetricSelect!="landings"&input$MetricSelect!="GINI"){
#        datSub <- subset(datSub,  METRIC %in% input$MetricSelect & !is.na(input$MetricSelect) & SUMSTAT == input$AVE_MED2)}
#      else {
#        datSub <- subset(datSub,  METRIC %in% input$MetricSelect& !is.na(input$MetricSelect) )
#      }}
    if(input$Ind_sel!="Economic")  {
      if(input$MetricSelect!="Number of vessels"&input$MetricSelect!="Date 50 percent of total catch landed"&input$MetricSelect!="Share of landings by state"&input$MetricSelect!="Gini coefficient"){
        if(input$MetricSelect=="Exponential Shannon Index"|input$MetricSelect=="Proportion of revenue from CS fishery"|input$MetricSelect=="Fishery participation"|input$MetricSelect=="Days at sea"){
          datSub <- subset(datSub,  METRIC %in% input$MetricSelect & SUMSTAT == input$AVE_MED2 & !is.na(input$MetricSelect)& FISHAK==input$FishAkSelect)
        } else {
          datSub <- subset(datSub,  METRIC %in% input$MetricSelect & SUMSTAT == input$AVE_MED2 & !is.na(input$MetricSelect))}
      }   else {
        datSub <- subset(datSub,  METRIC %in% input$MetricSelect & !is.na(input$MetricSelect))
      }}
    
     datSub$VALUE <- as.numeric(datSub$VALUE)
     datSub$VARIANCE <- as.numeric(datSub$VARIANCE)
     datSub$N <- as.numeric(datSub$N)
     datSub$PCHANGE <- as.numeric(datSub$PCHANGE)
     
      if(input$CategorySelect != "Fisheries") {
        datSub <- subset(datSub, CS == input$inSelect)
      }
     datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
     datSub$N <- ifelse(datSub$N<3, NA, datSub$N)
#    datSub$N <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$N)
    datSub$VARIANCE <- ifelse(datSub$N>2&is.na(datSub$VALUE)==T, NA, datSub$VARIANCE)
    #datSub$FISHAK <- ifelse(datSub$FISHAK=="TRUE", "Vessels included", "Vessels not included")
   # datSub$whitingv <- ifelse(datSub$whitingv=="TRUE", "Vessels included", "Vessels not included") 

    if(input$MetricSelect=="Share of landings by state"){
      datSub$VALUE <- datSub$VALUE*100
      datSub$VARIANCE <- datSub$VARIANCE*100
        }
    if(input$Ind_sel=="Economic") {
      datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="STAT"),
                        which(colnames(datSub)=="SHORTDESCR"),which(colnames(datSub)=="whitingv"),
                        #which(colnames(datSub)=="FISHAK")
                        which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]

    } 
#    if(input$Ind_sel!="Economic") {
#      datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
#                        which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),
                        #which(colnames(datSub)=="FISHAK"),
#                        which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
#    }
    
   if(input$Ind_sel!="Economic") {
           if(input$MetricSelect=="Exponential Shannon Index"|input$MetricSelect=="Proportion of revenue from CS fishery"|input$MetricSelect=="Fishery participation"|input$MetricSelect=="Days at sea"){   
             datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
                                 which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="FISHAK"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
             datSub$FISHAK <- ifelse(datSub$FISHAK=="TRUE","Included","Not included")
           } else if(input$MetricSelect=="Number of vessels"){
             datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
                            which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"))]
           }else if(input$MetricSelect=="Share of landings by state"){
             datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
                                 which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="agid"))]
           }else {
             datSub <- datSub[,c(which(colnames(datSub)=="YEAR"),which(colnames(datSub)=="VARIABLE"),which(colnames(datSub)=="CATEGORY"),which(colnames(datSub)=="CS"),which(colnames(datSub)=="SUMSTAT"),
                                 which(colnames(datSub)=="METRIC"),which(colnames(datSub)=="whitingv"),which(colnames(datSub)=="N"),which(colnames(datSub)=="VALUE"),which(colnames(datSub)=="VARIANCE"))]
           }}
 
    
       validate(
           need(dim(datSub)[1]>0, 
               paste('Sorry, this plot could not be generated as no vessels matched your selections. Try selecting a different statistic.
                  ')))
    return(datSub)
    
})


# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSub <- reactive({
  #  if(input$DodgeSelect == "Economic measures side-by-side"){
      dat <- DatMain()      
      datSub <- subset(dat, YEAR %in% input$YearSelect &  
                            CATEGORY == input$CategorySelect &
                            VARIABLE %in% input$VariableSelect &
                            whitingv %in% input$FishWhitingSelect)
      
     if(input$Ind_sel=="Economic") {
                datSub <- subset(datSub,  SHORTDESCR %in% input$ShortdescrSelect &
                             STAT == input$StatSelect) 
        }

      
      if(input$Ind_sel!="Economic")  {
        if(input$MetricSelect!="Number of vessels"&input$MetricSelect!="Date 50 percent of total catch landed"&input$MetricSelect!="Share of landings by state"&input$MetricSelect!="Gini coefficient"){
          if(input$MetricSelect=="Exponential Shannon Index"|input$MetricSelect=="Proportion of revenue from CS fishery"|input$MetricSelect=="Fishery participation"|input$MetricSelect=="Days at sea"){
            datSub <- subset(datSub,  METRIC %in% input$MetricSelect & SUMSTAT == input$AVE_MED2 & !is.na(input$MetricSelect)& FISHAK==input$FishAkSelect)
         } else {
            datSub <- subset(datSub,  METRIC %in% input$MetricSelect & SUMSTAT == input$AVE_MED2 & !is.na(input$MetricSelect))}
          }   else {
          datSub <- subset(datSub,  METRIC %in% input$MetricSelect & !is.na(input$MetricSelect))
      }}
       
       
      validate(
        need(dim(datSub)[1]>0, 
             paste('Sorry, this plot could not be generated as no vessels matched your selections. Try selecting a different variable
                  ')))
      
      if(input$CategorySelect != "Fisheries") {
        datSub <- subset(datSub, CS == input$inSelect)
      }

       
      datSub$N <- as.numeric(datSub$N)
      datSub$VALUE <- as.numeric(datSub$VALUE)
      datSub$VARIANCE <- as.numeric(datSub$VARIANCE)

        validate( 
        need(max(datSub$N)>2, 
             paste('Sorry, this plot could not be generated as data has been suppressed to protect confidentiality. 
                  Try selecting to show data summed across all fisheries or a different variable.
                   ')))
      
      if(input$Ind_sel=="Economic"){
          datSub$VALUE <-datSub$VALUE/1000
          datSub$VARIANCE <- datSub$VARIANCE/1000
        } else if(input$Ind_sel!="Economic") {
          if(input$MetricSelect=="Revenue per crew day"||input$MetricSelect=="Crew wage"){
          datSub$VALUE <- datSub$VALUE/1000
          datSub$VARIANCE <- datSub$VARIANCE/1000  
      } else if(input$MetricSelect=="Share of landings by state"){
          datSub$VALUE <- datSub$VALUE*100
          datSub$VARIANCE <- datSub$VARIANCE*100
      } else {
          datSub$VALUE <- datSub$VALUE
          datSub$VARIANCE <- datSub$VARIANCE
        }}
      # order for plotting
#      datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
#        levels = factorOrder$shortdescr)
 
      if(input$CategorySelect == "Homeport"){
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
      } else if(input$CategorySelect == "State"){
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
      } else if(input$CategorySelect == "Fisheries"){
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = c("All fisheries","All Catch Share fisheries","All non-Catch Share fisheries","At-sea Pacific whiting","Shoreside Pacific whiting",
                                                              "DTS trawl with trawl endorsement","Non-whiting midwater trawl","Non-whiting, non-DTS trawl with trawl endorsement",  "Groundfish fixed gear with trawl endorsement",
                                                              "All non-Catch Share fisheries combined"="All non-Catch Share fisheries", "Groundfish fixed gear with fixed gear endorsement","Crab","Shrimp","Other fisheries"))
      }
      
      if(input$Ind_sel!="Economic"&input$MetricSelect!="Share of landings by state"){
        if(length(input$VariableSelect)>1){
         datSub$star <- ifelse(is.na(datSub$VALUE)==T, "*", "")
         datSub$VARIANCE <- ifelse(is.na(datSub$VARIANCE)==T, 0, datSub$VARIANCE)
         datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
#        datSub$con_flag <- ifelse(datSub$con_flag==1, 0, datSub$con_flag)
      }
      else if(length(input$VariableSelect)==1){
        datSub$star <- ""
      }} else
      { datSub$star <- ""}

      if(input$Ind_sel!="Economic"){
       if(input$CategorySelect=="Fisheries"){
        datSub$sort <- ifelse(datSub$VARIABLE=="All fisheries", ".....All fisheries", as.character(datSub$VARIABLE))
        datSub$sort <- ifelse(datSub$VARIABLE=="All Catch Share fisheries", "....All Catch Share fisheries", as.character(datSub$sort))
        datSub$sort <- ifelse(datSub$VARIABLE=="All non-Catch Share fisheries", "....All non-Catch Share fisheries",  as.character(datSub$sort))
        datSub$sort <- ifelse(datSub$VARIABLE=="At-sea Pacific whiting", "....At-sea Pacific whiting",  as.character(datSub$sort))
        datSub$sort <- ifelse(datSub$VARIABLE=="Shoreside Pacific whiting", "....Shoreside Pacific whiting",  as.character(datSub$sort))
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
        datSub$sort <- ifelse(datSub$VARIABLE=="Fort Bragg", ".Fort Brag", as.character(datSub$sort))                
        datSub$sort <- ifelse(datSub$VARIABLE=="San Francisco", ".San Francisco", as.character(datSub$sort))              
      }
      else {
        datSub$sort <- datSub$VARIABLE 
      }
      } else {
      datSub$sort <- ifelse(datSub$SHORTDESCR=="Revenue", "...Revenue", as.character(datSub$SHORTDESCR))
      datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable costs", "..Variable costs", as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$SHORTDESCR=="Fixed costs", ".Fixed costs", as.character(datSub$sort))
      datSub$sort <- ifelse(datSub$SHORTDESCR=="Variable Cost Net Revenue", ".Variable Cost Net Revenue",  as.character(datSub$sort))
      }  

      if(input$Ind_sel!="Economic"&input$MetricSelect=="Number of vessels"){
        datSub$star <- ifelse(datSub$N<3, "*", datSub$star)
        datSub$VALUE <- ifelse(datSub$N<3, NA, datSub$VALUE)
      }
      datSub$flag <- max(datSub$flag)
#      datSub$confWhiting <- ifelse(datSub$confWhiting=="",0,1)
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

PermitMessage <- reactive({
  if(!(is.null(input$YearSelect) | is.null(input$CategorySelect) | 
       is.null(input$VariableSelect))){
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

