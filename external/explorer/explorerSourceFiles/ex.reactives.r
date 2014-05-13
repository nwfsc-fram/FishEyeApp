# creating the dat() reactive function that contains the user selected dataset
dat <- reactive({
  if(!is.null(input$dat.name)){
    if(input$dat.name == "Catcher Vessel Cost Data"){
      load("data/fullcosts.RData")
      dat <- melt(fullcosts, measure.vars="DISCOST")
    }else dat <- NULL
    dat
  }
})

#reactives for dataset specific parameters
dat.measure.var <- reactive({
  if(!is.null(dat())){
    if(input$dat.name == "Catcher Vessel Cost Data"){
      measurevar <- "DISCOST"
    }
    measurevar
  }
})

#subsetting the data AND casting for individual level ID (fun.agg=sum)
dat.sub <- reactive({
  if(is.null(input$dataGo) || input$dataGo==0) return()
  isolate(
    if(!is.null(dat())){
      if(input$placeUnit == "Homeport"){ d <- dcast(dat(), formula= VESSEL_ID + SURVEY_YEAR + FISHERIES + VSSLNGCLASS + HOMEPT + STATE ~ variable, 
                                                    subset=.(variable == dat.measure.var() & SURVEY_YEAR %in% input$years & FISHERIES %in% input$fishery & VSSLNGCLASS %in% input$length & HOMEPT %in% input$place), fun.aggregate=sum, na.rm=T)      
        }else if(input$placeUnit == "State"){ d <- dcast(dat(), formula= VESSEL_ID + SURVEY_YEAR + FISHERIES + VSSLNGCLASS + HOMEPT + STATE ~ variable, 
                                                        subset=.(variable == dat.measure.var() & SURVEY_YEAR %in% input$years & FISHERIES %in% input$fishery & VSSLNGCLASS %in% input$length & STATE %in% input$place), fun.aggregate=sum, na.rm=T)       
      }
      melt(d, measure.vars= dat.measure.var())
    } else return()
  )
})

# dat.melt <- reactive({
#   if(!is.null(dat.sub())) melt(dat.sub(), measure.vars= dat.measure.var())
# })

# ggplot2 grouping, faceting, pooling
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

# Resphaping the data based on plot options
# reactives for cast options
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

# Reactives for plot output
# 
# ggplot.base <- reactive({
#   if(!is.null(dat.cast())){
#     plot.dat <- dat.cast()
#     ggplot(plot.dat, aes(x=byvar(), y=dat.measure.var(), fill=groupvar()) + geom_bar(stat="identity")) 
#   }
# })

#staging subsetting
dataGo <- reactive({
  if(!is.null(dat())){
    x <- TRUE
  } else x <- FALSE
  x
})