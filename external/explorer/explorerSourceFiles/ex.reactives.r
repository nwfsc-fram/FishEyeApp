#This file handles the reactive expressions for data management and statistical operations.

# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file

DatMain <- reactive({ # data load moved to serverhead
  # data is loaded from serverHead.R load call
  dat <- netrevTable

})

DatThirds <- reactive({
  dat <- netrevThirds
})


DatVars <- reactive({
  # create a list of variable names used in the sidebar inputs
  dat <- DatMain()
  datVars <- with(dat, 
    list(YEAR = unique(YEAR),
      SHORTDESCR = factorOrder$shortdescr,
       CATEGORY = c("Fisheries","Homeport","State","Vessel length class"),
      FISHAK = unique(FISHAK),
      STAT =  c("Average per vessel","Average per vessel/day","Average per vessel/metric-ton","Summed over all vessels"="Total" ),
      CS = unique(CS)
  ))
})


Variable <- reactive({
  dat <- DatMain()
    if(input$CategorySelect == "Fisheries"){
      variable = c("All Catch Share Fisheries","At-sea Pacific whiting","Shoreside Pacific whiting","DTS trawl with trawl endorsement","Non-whiting, non-DTS trawl with trawl endorsement",
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
    
      
    #subsetting
    datSub <- subset(dat, YEAR %in% input$YearSelect &  
                       SHORTDESCR %in% input$ShortdescrSelect & 
                       CATEGORY %in% input$CategorySelect &
                       VARIABLE %in% input$VariableSelect &
                       FISHAK == input$FishAkSelect &
                       STAT == input$StatSelect)
    
     datSub$VALUE <- as.numeric(datSub$VALUE)
    
    # order for plotting
    datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                                levels = factorOrder$shortdescr)
  
      if(input$CategorySelect != "Fisheries") {
        datSub <- subset(datSub, CS == input$inSelect)
        }
 
    if(input$CategorySelect == "Homeport"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
    } else if(input$CategorySelect == "State"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
    } else if(input$CategorySelect == "Fisheries"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$fisheries)
    }
    
    return(datSub)
    
#  } else return()
  #   )
})


# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSub <- reactive({
#   if(is.null(input$DataButton) || input$DataButton == 0) return()
#   input$ShortdescrSelect
#   isolate(  
    if(input$DodgeSelect == "Compare economic measures side-by-side"){
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
        datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$fisheries)
      }
 
      validate(
        need(min(datSub$N)>2,
             '  
              Sorry, this figure could not be generated as data were suppressed to protect confidentiality. 
              Data are suppressed when there are not enough observations to protect confidentiality. 
              Often, this error can be removed by clicking on the "Include vessels that fished in AK" button.
              There are less vessels that fished solely off the west coast than off the west coast and in Alaska.
              If this does not resolve the issue, please try a different selection of your summary variable (fishery, vessel length, homeport, state) or year.')
      ) 
      
      
      return(datSub)
    } else return()
#   )
})


# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# This is only for when we choose the stacked plots
# build dcast formula using if controls and using the quoted method in dcast
DatSub2 <- reactive({
 #!is.null(DatMain()) & 
  if(input$DodgeSelect == "Derivation of total cost net revenue"){
    dat <- DatMain()      
    
 
    #subsetting
    datSub2 <- subset(dat, YEAR %in% input$YearSelect &  
                        SHORTDESCR %in% c("Total cost net revenue","Fixed costs","Variable costs")   & 
                        CATEGORY %in% input$CategorySelect &
                       VARIABLE %in% input$VariableSelect &
                        FISHAK == input$FishAkSelect &
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
      datSub2$VARIABLE <- factor(datSub2$VARIABLE, levels = factorOrder$fisheries)
    }
   
    validate(
      need(min(datSub2$N)>2,
           '  
              Sorry, this figure could not be generated as data were suppressed to protect confidentiality. 
              Data are suppressed when there are not enough observations to protect confidentiality. 
              Often, this error can be removed by clicking on the "Include vessels that fished in AK" button.
              There are less vessels that fished solely off the west coast than off the west coast and in Alaska.
              If this does not resolve the issue, please try a different selection of your summary variable (fishery, vessel length, homeport, state) or year.')
      ) 
    
    return(datSub2)

  } 

})

DatSub3 <- reactive({
  
  if(input$DodgeSelect == "Derivation of variable cost net revenue"){#!is.null(DatMain()) & 
    dat <- DatMain()      
    
    
    #subsetting
    datSub3 <- subset(dat, YEAR %in% input$YearSelect &  
                        SHORTDESCR %in% c("Variable cost net revenue","Variable costs")   & 
                        CATEGORY %in% input$CategorySelect &
                        VARIABLE %in% input$VariableSelect &
                        FISHAK == input$FishAkSelect &
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
      datSub3$VARIABLE <- factor(datSub3$VARIABLE, levels = factorOrder$fisheries)
    }
    
    validate(
      need(min(datSub3$N)>2,
           '  
              Sorry, this figure could not be generated as data were suppressed to protect confidentiality. 
              Data are suppressed when there are not enough observations to protect confidentiality. 
              Often, this error can be removed by clicking on the "Include vessels that fished in AK" button.
              There are less vessels that fished solely off the west coast than off the west coast and in Alaska.
              If this does not resolve the issue, please try a different selection of your summary variable (fishery, vessel length, homeport, state) or year.')
    ) 
    
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
                          STAT == input$StatSelect
                     )
#     
    if(input$CategorySelect != "Fisheries") {
      datSub <- subset(datSub, CS == input$inSelect)
    }
    
    datsub <- subset(datSub, is.na(datSub$N)==F)
    datSub$THIRDS <- factor(datSub$THIRDS,
                            levels = factorOrder$thirds)
    
    datSub$SHORTDESCR <- factor(datSub$SHORTDESCR, 
                                levels = factorOrder$shortdescr)
    
    if(input$CategorySelect == "Homeport"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$port)
    } else if(input$CategorySelect == "State"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$state)
    } else if(input$CategorySelect == "Fisheries"){
      datSub$VARIABLE <- factor(datSub$VARIABLE, levels = factorOrder$fisheries)
    }
   
    validate(
      need(min(datSub$N)>2,
           '  
              Sorry, this figure could not be generated as data were suppressed to protect confidentiality. 
              Data are suppressed when there are not enough observations to protect confidentiality. 
              Please try a different selection of your summary variable (fishery, vessel length, homeport, state) or year.')
    ) 
    
     validate(
       need(input$StatSelect != "Summed over all vessels", 
           '

            Sorry, the variability analysis does not support the "summed over all vessels" stastic.
            Please select a different statistic.')
     )
    return(datSub)
    
 # }
})


# Plotting/staging
PermitPlot <- reactive({
  if(!(is.null(input$YearSelect) | is.null(input$CategorySelect) | 
   is.null(input$VariableSelect) |is.null(input$FishAkSelect) | 
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
    downloadButton("dlTable", "Download Data Table",class = "btn btn-info")
  }
})

output$download_figure <- renderUI({
  if(PermitPlot()){# & input$tabs=="Visualize Data"){
    downloadButton("dlFigure", "Download Figure(s)",class = "btn btn-info")
  }
})

#output$download_Thirds<- renderUI({
#  if(PermitPlot() & input$tabs=="Variability Analaysis"){
#    downloadButton("dlPlotThirds", "Download Figure",class = "btn btn-info")
#  }
#})

