#This file handles the reactive expressions for data management and statistical operations.

# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file

dat <- reactive({ #data load moved to serverhead
  input$dataButton
  isolate(
  if(!is.null(input$dat.name) && !is.null(input$topicSelect)){
  
    data <- switch(input$dat.name,
                   "Revenue" = "rev",
                   "Variable cost" = "varcost",
                   "Fixed cost" = "fixedcost",
                   "Variable cost net revenue" = "varnetrev",
                   "Total cost net revenue" = "totalnetrev")
    
    year <- "year"
    
    topic <- switch(input$topicSelect,
                    "Fisheries" = "fisheries",
                    "Homeport" = "homept",
                    "State" = "state",# gotta do this in plot.reactives aslo
                    "Vessel length class" = "vsslngclass",
#                     "Delivery port" = ifelse(input$placeUnit == "Port", "deliverypt", "deliveryst")
                    )
    
    selection <- ifelse(input$topicSelect == "Fisheries", input$fishery, 
                        ifelse(input$topicSelect == "Homeport", input$place,
                               ifelse(input$topicSelect == "State", input$place,
                                    ifelse(input$topicSelect == "Vessel length class", input$length)))) 
    
    stat <- switch(input$stat,
                   "Total" = "sum",
                   "Average" = "mean")
    
#     noak <- switch(input$removeAK,
#                    "All fishery operations" ="ak", 
#                    "West Coast only operations"= "noak")
    
    print("data.select:")
    print(paste(data, year, topic, stat, sep="."))    

    dat <- with(tabs.out, get(paste(data, year, topic, stat, sep=".")))
    
    print("dat:")
    print(dat)

    return(dat)

  } else return()
  )
})

dat.vars <- reactive({
  load("data/dat.vars.RData")
  print("datvars:")
  print(str(dat.vars))
  dat.vars
})

# #reactives for dataset specific parameters
# dat.measure.var <- reactive({
#   input$dataButton
#   isolate(
#     if(!is.null(dat())){
#       if(input$dat.name == "Cost"){
#         measurevar <- "DISCOST"
#       } else if(input$dat.name == "Revenue"){
#         measurevar <- "REV"
#       } else measurevar <- NULL
#       measurevar
#     }
#   )
# })

# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the qouted method in dcast
dat.sub <- reactive({
  input$dataButton
  isolate(
    if(!is.null(dat())){
        
        dat <- dat()
        
        #translatting input factor names to data.frame factor names
        selectVars <- c(switch(input$topicSelect,
                            "Fisheries" = input$fishery,
                            "Homeport" = input$place,
                            "State" = input$place,
                            "Vessel length class" = input$length))
        
        print("selectVars:")
        print(paste(selectVars, sep = " ")) #debugging
        
        #subsetting
          # selected topic vars
          dat.sub <- dat[dat[,1] %in% c(selectVars),]
          # selected years
          dat.sub <- dat.sub[dat.sub[,2] %in% c(input$years),]
          #re-ordering values
          dat.sub[,1] <- reorder(dat.sub[,1], dat.sub[,3])
          
         
            
        print("dat.sub:")
        print(dat.sub)
        
        return(dat.sub)
        
    } else return()
  )
})
