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
       CATEGORY = unique(CATEGORY),
      FISHAK = unique(FISHAK),
      STAT =  factorOrder$stat))
})


Variable <- reactive({
  dat <- DatMain()
    if(input$CategorySelect == "Homeport"){
      variable = factorOrder$port
    } else if(input$CategorySelect == "State"){
      variable = factorOrder$state
    } else if(input$CategorySelect == "Fisheries"){
      variable = factorOrder$fisheries
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
  if(!is.null(DatMain())  ){
    dat <- DatMain()      
    
    #       print("I am netrevTable data:")
    #       print(str(dat))
    
    #       statSwitch <- switch(input$StatSelect,
    #         "Mean" = "mean",
    #         "Average" = "sum",
    #         "Average (per day)" = "mean per day",
    #         "Average (per metric ton)" = "mean per metric ton")
    
    #       print(str(input$YearSelect))
    #       print(str(input$ShortdescrSelect))
    #       print(str(input$CategorySelect))
    #       print(str(input$VariableSelect))
    #       print(str(input$FishAkSelect))
    #       print(input$StatSelect) 
    
    #subsetting
    datSub <- subset(dat, YEAR %in% input$YearSelect &  
                       SHORTDESCR %in% input$ShortdescrSelect & 
                       CATEGORY %in% input$CategorySelect &
                       VARIABLE %in% input$VariableSelect &
                       FISHAK == input$FishAkSelect &
                       STAT == input$StatSelect)
    
    datSub$YEAR <- as.numeric(datSub$YEAR)
    datSub$VALUE <- as.numeric(datSub$VALUE)
    #       print("I am subset Table!")
    #       print(str(datSub))
    
    
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
    
    return(datSub)
    
  } else return()
  #   )
})


# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the quoted method in dcast
DatSub <- reactive({
#   if(is.null(input$DataButton) || input$DataButton == 0) return()
#   input$ShortdescrSelect
#   isolate(  
    if(!is.null(DatMain()) & input$DodgeSelect == "Compare economic measures side-by-side"
       ){
      dat <- DatMain()      
      
#       print("I am netrevTable data:")
#       print(str(dat))
      
#       statSwitch <- switch(input$StatSelect,
#         "Mean" = "mean",
#         "Average" = "sum",
#         "Average (per day)" = "mean per day",
#         "Average (per metric ton)" = "mean per metric ton")

#       print(str(input$YearSelect))
#       print(str(input$ShortdescrSelect))
#       print(str(input$CategorySelect))
#       print(str(input$VariableSelect))
#       print(str(input$FishAkSelect))
#       print(input$StatSelect) 

      #subsetting
      datSub <- subset(dat, YEAR %in% input$YearSelect &  
                            SHORTDESCR %in% input$ShortdescrSelect & 
                            CATEGORY %in% input$CategorySelect &
                            VARIABLE %in% input$VariableSelect &
                            FISHAK == input$FishAkSelect &
                            STAT == input$StatSelect)
      
      
#       print("I am subset Table!")
#       print(str(datSub))
      
      
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
      
      return(datSub)
      
    } else return()
#   )
})


# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# This is only for when we choose the stacked plots
# build dcast formula using if controls and using the quoted method in dcast
DatSub2 <- reactive({
 
  if(!is.null(DatMain()) & input$DodgeSelect == "Total cost revenue figure"){
    dat <- DatMain()      
    
 
    #subsetting
    datSub2 <- subset(dat, YEAR %in% input$YearSelect &  
                        SHORTDESCR %in% c("Total cost net revenue","Fixed costs","Variable costs")   & 
                        CATEGORY %in% input$CategorySelect &
                       VARIABLE %in% input$VariableSelect &
                        FISHAK == input$FishAkSelect &
                        STAT == input$StatSelect)
    
    
    # order for plotting
    datSub2$SHORTDESCR <- factor(datSub2$SHORTDESCR, 
                                 levels =  c("Variable costs","Fixed costs","Total cost net revenue") )
    
#    
    if(input$CategorySelect == "Homeport"){
      datSub2$VARIABLE <- factor(datSub2$VARIABLE, levels = factorOrder$port)
      
    } else if(input$CategorySelect == "State"){
      datSub2$VARIABLE <- factor(datSub2$VARIABLE, levels = factorOrder$state)
    } else if(input$CategorySelect == "Fisheries"){
      datSub2$VARIABLE <- factor(datSub2$VARIABLE, levels = factorOrder$fisheries)
    }
    
    return(datSub2)

  } else return()

})

DatSub3 <- reactive({
  
  if(!is.null(DatMain()) & input$DodgeSelect == "Variable cost revenue figure"){
    dat <- DatMain()      
    
    
    #subsetting
    datSub3 <- subset(dat, YEAR %in% input$YearSelect &  
                        SHORTDESCR %in% c("Variable cost net revenue","Variable costs")   & 
                        CATEGORY %in% input$CategorySelect &
                        VARIABLE %in% input$VariableSelect &
                        FISHAK == input$FishAkSelect &
                        STAT == input$StatSelect)
    
    
    # order for plotting
    datSub3$SHORTDESCR <- factor(datSub3$SHORTDESCR, 
                                 levels =  c("Variable costs","Variable cost net revenue") )
    
    #    
    if(input$CategorySelect == "Homeport"){
      datSub3$VARIABLE <- factor(datSub3$VARIABLE, levels = factorOrder$port)
      
    } else if(input$CategorySelect == "State"){
      datSub3$VARIABLE <- factor(datSub3$VARIABLE, levels = factorOrder$state)
    } else if(input$CategorySelect == "Fisheries"){
      datSub3$VARIABLE <- factor(datSub3$VARIABLE, levels = factorOrder$fisheries)
    }
    
    return(datSub3)
    
  } else return()
  
})

# create an additional subset for thirds plot...this is where OO would be handy
DatSubThirds <- reactive({
  if(!is.null(DatThirds())){
    
#     validate(
#       need(input$StatSelect == "Mean", 
#           'This feature currently supports only the "Mean" Summary Stastic')
#     )
#     
    validate(
      need(length(input$VariableSelect) == 1, 
        "This feature currently supports only one Summary Variable selection"))
    
    
    dat <- DatThirds()
    
#     print("I am netrevThirds data:")
#     print(str(dat))
        
#     statSwitch <- switch(input$StatSelect,
#                          "Average" = "mean",
#                          "Total" = "sum",
#                          "Average (per day)" = "mean per day",
#                          "Average (per metric ton)" = "mean per metric ton")
    
    #subsetting
    datSub <- subset(dat, YEAR %in% input$YearSelect &
                          SHORTDESCR %in% input$ShortdescrSelect &
                          CATEGORY %in% input$CategorySelect &
                          VARIABLE %in% input$VariableSelect &
                          FISHAK == input$FishAkSelect &
                          STAT == input$StatSelect
                     )
# 
#     print("I am subset thirds!")
#     print(str(datSub))
#     
#     print("THIRDS???")
#     print(unique(datSub$THIRDS))
    print(unique(factorOrder$thirds))

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
    
    return(datSub)
    
  }
})


# Plotting/staging
PermitPlot <- reactive({
  if(!(is.null(input$YearSelect) | is.null(input$CategorySelect) | 
    is.null(input$VariableSelect) | is.null(input$FishAkSelect) | 
    is.null(input$StatSelect) | is.null(input$ShortdescrSelect))){
    if(!(input$YearSelect[1]=="" | 
      input$CategorySelect[1] == "" | input$StatSelect[1] == "" | 
      input$VariableSelect[1] == "" | input$ShortdescrSelect[1] == "")){      
        x <- TRUE
    } else {
      x <- FALSE
    }
  } else x <- FALSE
  x
})
