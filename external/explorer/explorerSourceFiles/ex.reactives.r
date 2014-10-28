#This file handles the reactive expressions for data management and statistical operations.

# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file

dat <- reactive({ #data load moved to serverhead
  input$dataButton
  isolate(
    if(permitPlot()){
    
    if (input$stat == "Total") {
      dat <- netrevTabsSum
    } else {
      dat <- netrevTabsMean
    }
      
#       year <- "year"
#       
#       data <- if (length(input$dat.name) < 2) {
#                 switch(input$dat.name,
#                      "Revenue" = "rev",
#                      "Variable cost" = "varcost",
#                      "Fixed cost" = "fixedcost",
#                      "Variable cost net revenue" = "varnetrev",
#                      "Total cost net revenue" = "totalnetrev")
#               } else {
#                 "netrev"
#               }
#       
#       stat <- switch(input$stat,
#                      "Total" = "sum",
#                      "Average" = "mean")
#       
#       topic <- switch(input$topicSelect,
#                       "Fisheries" = "fisheries",
#                       "Homeport" = "homept",
#                       "State" = "state",
#                       "Vessel length class" = "vsslngclass")
#       
#       
#       print("data.select:") # debugging to check on the table selection
#       print(paste(data, year, topic, stat, sep="."))  
#       
#       
#       selection <- ifelse(input$topicSelect == "Fisheries", input$fishery,
#                           ifelse(input$topicSelect == "Homeport", input$place,
#                                  ifelse(input$topicSelect == "State", input$place,
#                                         ifelse(input$topicSelect == "Vessel length class", input$length))))
#       
#       print("selection:")
#       print(selection)
#       print("input$topicSelect:")
#       print(input$fishery)
#       
#       # this if statement is a conditional to set up the table selection for multiple dat.name inputs
#       if(length(input$dat.name) < 2) {
#         
#         dat <- with(tabs.out, get(paste(data, year, topic, stat, sep=".")))
# 
#       } else {
#         
#         dat.go <- list()
#         
#         for(i in length(selection)) {
#           
#           dat.go[i] <- with(tabs.out, get(paste(data, year, topic, stat, sep=".")))
#           
#         }
#         
#         dat <- rbind(dat.go)
#       }
      
#       print("dat:")
#       print(str(dat))
      
      return(dat)
      
    } else return()
  )
})

dat.vars <- reactive({
  load("data/dat.vars.RData")
#   print("datvars:")
#   print(str(dat.vars))
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
  if(!is.null(input$dataButton) && input$dataButton > 0)
  isolate(
    if(permitPlot()){
      
      print(input$dataButton)
      
      dat <- dat()
      
      netrev.type <- vector(length=length(input$dat.name))
      
      for (i in 1:length(input$dat.name)) {
          netrev.type[i] <- switch(input$dat.name[i],
                                   "Revenue" = "REV",
                                   "Variable cost" = "VARCOST",
                                   "Fixed cost" = "FIXEDCOST",
                                   "Variable cost net revenue" = "VARNETREV",
                                   "Total cost net revenue" = "TOTALNETREV")         
                          
#         print(netrev.type[i])
        
      }
      
#       print(paste("**", netrev.type, "**", sep=" "))
                     
#       #translating input factor names to data.frame factor names
#       ifelse(input$topicSelect == "Fisheries", selection <- input$fishery,
#                          ifelse(input$topicSelect == "Homeport", selection <- input$place,
#                                ifelse(input$topicSelect == "State", selection <- input$place,
#                                      ifelse(input$topicSelect == "Vessel length class", selection <- input$length))))
      
        selection <- input$topics

#       print("selectVars:")
#       print(input$topics)
#       print(paste(selection, sep = " ")) #debugging
#       
#       print("input$fishery:")
#       print(input$fishery)

      
      #subsetting
      dat.sub <- subset(dat, SURVEY_YEAR %in% input$years & variable %in% netrev.type & category %in% input$topics)
      #re-ordering values
#       dat.sub[,5] <- reorder(dat.sub[,5], dat.sub[,3])
      
      # droping the "topic" var, I don't think we actually need it, ever
      dat.sub <- dat.sub[!names(dat.sub) %in% "topic"]
      
      print("dat.sub:")
      print(head(dat.sub))
      
      return(dat.sub)
      
    } else return()
  )
})

# Plotting/staging
permitPlot <- reactive({
  if(!(is.null(input$years) | is.null(input$dat.name) | is.null(input$topicSelect) | is.null(input$stat) | is.null(input$topics))){
    if(!(input$years[1]=="" | input$dat.name[1] == "" | input$topicSelect[1] == "" | input$stat[1] == "" | input$topics[1] == "")){
      x <- TRUE
    } else {
      x <- FALSE
    }
  } else x <- FALSE
  x
})
