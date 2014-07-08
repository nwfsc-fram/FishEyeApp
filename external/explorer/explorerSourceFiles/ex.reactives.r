#This file handles the reactive expressions for data management and statistical operations.

# creating the dat() reactive function that contains the user selected dataset
# The re-classification of data types can be transfered to the read-in file

dat <- reactive({ #
  if(!is.null(input$dat.name)){
    if(input$dat.name == "Disaggregated Cost"){
      load("data/fullcosts.RData")
      dat <- melt(fullcosts, measure.vars="DISCOST")
      print(names(dat))#debugging
    } else if(input$dat.name == "Revenue"){
      load("data/fullrev.RData")
      dat <- melt(fullrev, measure.vars=c("LBS", "REV", "MTS", "DAS"))
      print(names(dat)) #debugging
    }else dat <- NULL
    print(str(dat))
    dat
  }
})

#reactives for dataset specific parameters
dat.measure.var <- reactive({
  input$dataButton
  isolate(
    if(!is.null(dat())){
      if(input$dat.name == "Disaggregated Cost"){
        measurevar <- "DISCOST"
      } else if(input$dat.name == "Revenue"){
        measurevar <- "REV"
      } else measurevar <- NULL
      measurevar
    }
  )
})

# selecting plot variables, subsetting the data AND casting for individual level ID (fun.agg=sum)
# build dcast formula using if controls and using the qouted method in dcast
dat.sub <- reactive({
  input$dataButton
  isolate(
    if(!is.null(dat())){
      
      #subseting before variable selection and casting because all of the input variables will still be in the data
      
      subset.args <- function(){  #creating a subset string to be evaluated in subset arg of dcast
                       prime <- NULL
                       prime <- "SURVEY_YEAR %in% input$years & FISHERIES %in% input$fishery & VSSLNGCLASS %in% input$length" #this is our base subset list, these are common to all of the datasets                        
                       prime <- if(input$placeUnit == "Homeport"){ paste(prime, "& HOMEPT %in% input$place", sep=" ") } else paste(prime, "& STATE %in% input$place", sep=" ")  #adding the geographic location option                                                              
                       prime <- if(input$dat.name=="Disaggregated Cost"){ paste(prime, " & COSTTYPCAT %in% input$costtyp", sep = "") } else prime                    
                       print(prime) #debugging                       
                       prime
                     }

#       subset.args <- c(if(!is.null(input$years)) "SURVEY_YEAR %in% input$years",
#                        if(!is.null(input$fishery)) "FISHERIES %in% input$fishery",
#                        if(!is.null(input$length)) "VSSLNGCLASS %in% input$length",
#                        if(!is.null(input$costtyp)) "COSTTYP %in% input$costtyp",
#                        ifelse(input$placeUnit=="Homeport", "HOMEPT %in% input$place", "STATE %in% input$place"))
      
      # txt <- paste(subset.args()) #not necessary

      dat.sub <- subset(dat(), eval(parse(text=subset.args()))) # handling subsetting

#       print(paste("prime contents", subset.args, sep=": ")) #debugging
      
      formula.args <- c("VESSEL_ID", #this code puts together the left side of the dcast arg
                        if(!is.null(input$years)) "SURVEY_YEAR",
                        if(!is.null(input$fishery)) "FISHERIES",
                        if(!is.null(input$length)) "VSSLNGCLASS",
                        if(input$dat.name=="Disaggregated Cost") "COSTTYP",
                        ifelse(input$placeUnit=="Homeport", "HOMEPT", "STATE"))
      
      # print(paste("formula.args contents", formula.args, sep=": ")) # debugging
      
      d.cast1 <- dcast(dat.sub, list(c(formula.args), .(variable)), fun.aggregate=sum, na.rm=TRUE) #casting the dataframe by unique observation (vessel_ID) and the desired variables.
                       
      
      print(str(d.cast1)) #debugging
      
      # old code
#       if(input$placeUnit == "Homeport"){ d <- dcast(dat(), formula= VESSEL_ID + SURVEY_YEAR + FISHERIES + VSSLNGCLASS + HOMEPT + STATE ~ variable, 
#                                                     subset=.(variable == dat.measure.var() & SURVEY_YEAR %in% input$years & FISHERIES %in% input$fishery & VSSLNGCLASS %in% input$length & COSTTYP %in% input$costtyp & HOMEPT %in% input$place), fun.aggregate=sum, na.rm=T)      
#         }else if(input$placeUnit == "State"){ d <- dcast(dat(), formula= VESSEL_ID + SURVEY_YEAR + FISHERIES + VSSLNGCLASS + HOMEPT + STATE ~ variable, 
#                                                         subset=.(variable == dat.measure.var() & SURVEY_YEAR %in% input$years & FISHERIES %in% input$fishery & VSSLNGCLASS %in% input$length & COSTTYP %in% input$costtyp  & STATE %in% input$place), fun.aggregate=sum, na.rm=T)       
#       }

      d.melt <- melt(d.cast1, measure.vars=dat.measure.var()) #melting the dataframe after each cast is my new SOP for dealing with multiple cast/melt operations

      print(str(d.melt)) #debugging

      d.melt

    } else return()
  )
})

# these next two are only used in io.sidebar.r, they should be integrated elsewhere
group.choices <- reactive({
  if(!is.null(input$by.var)){
    if(input$by.var=="Survey year") {
      choices <- c("Fishery","Vessel length","Location")
    } else if(input$by.var=="Fishery") {
      choices <- c("Survey year","Vessel length","Location")
    }else choices <- NULL
    choices
  } else return()
})

facet.choices <- reactive({
  if(!is.null(input$group.var)){
    if(length(group.choices())>1) choices <- c("None", group.choices()[group.choices() != input$group.var]) else choices <- NUll
    choices  
  } else return()
})

# THis is the 'second' cast/melt operation. This takes the vessel lvl observations and melts them into the plot output
# reactives for cast options
# I am pretty sure you can generalize all of these switch operations...
byvar <- reactive({
  if(!is.null(input$by.var)){
    byvar <- switch(input$by.var,
                    "Survey year" = "SURVEY_YEAR",
                    "Fishery" = "FISHERIES")
  } else return()
})

groupvar <- reactive({
  if(!is.null(input$group.var)){
    groupvar <- switch(input$group.var,
                       "Survey year" = "SURVEY_YEAR",
                       "Fishery" = "FISHERIES",
                       "Vessel length" = "VSSLNGCLASS",
                       "Location" = if(input$placeUnit == "Homeport") "HOMEPT" else "STATE")
  } else return()
})

facetvar <- reactive({
  if(!is.null(input$facet.var)){
    facetvar <- switch(input$facet.var,
                       "Survey year" = "SURVEY_YEAR",
                       "Fishery" = "FISHERIES",
                       "Vessel length" = "VSSLNGCLASS",
                       "Location" = if(input$placeUnit == "Homeport") "HOMEPT" else "STATE") 
  } else return()
})

agg.method <- reactive({
  if(!is.null(input$stat)){
    agg.method <- switch(input$stat,
                         "sum" = sum,
                         "mean" = mean,
                         "N" = length)
  }else return()
})

# casting the data for plot/table output
dat.cast <- reactive({
  if(!is.null(input$by.var)){        
    if(input$facet.var == "None"){ d <- dcast(dat.sub(), list(c(byvar()[1], groupvar()[1]), .(variable)), fun.aggregate = agg.method())        
    } else                         d <- dcast(dat.sub(), list(c(byvar()[1], groupvar()[1], facetvar()[1]), .(variable)), fun.aggregate = agg.method())
    d    
  } else return()             
})          

#dataGo is for staging the data subsetting section. "Am I ready to hit the dataButton?"
dataGo <- reactive({
  if(!is.null(input$dataGo) && !is.null(dat())){
    x <- TRUE
  } else x <- FALSE
  #print(paste("dataGo=", x)) # debugging
  x
})


#plotGo  is for staging the plot section. "Am I ready to hit the plotButton?"
plotGo <- reactive({
  if(!is.null(dat.sub())){
    if(!is.null(dat.sub()) && input$dataButton > 0){
    x <- TRUE
    } else x <- FALSE
  #print(paste("plotGo=", x)) #debugging
  x
  } else x <- FALSE
})