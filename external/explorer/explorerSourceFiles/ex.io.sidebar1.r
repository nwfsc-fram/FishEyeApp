#this page handles all of the reactive expressions for the dynamic user interface
#this file is getting a bit long, consider splitting into two parts

output$dat.name <- renderUI({ #data selection. there are two reactives that are dependent on these names in ex.reactives
  selectInput("dat.name", "Measured variable:", choices=c("", "Disaggregated Cost", "Revenue"), selected="")
})

output$subsetChoice <- renderUI({
  if(!is.null(dat())){
    checkboxInput("subsetChoice", "View subsetting options", value= TRUE)
  }
})

# Inputs variables will be pulled at this stage because we are not preloading data via .r files
output$years <- renderUI({
  if(!is.null(dat())){
    selectInput("years", "Years:", choices=c(levels(dat()$SURVEY_YEAR)), selected=c(levels(dat()$SURVEY_YEAR)), multiple=T)
  }
})

output$fishery <- renderUI({
  if(!is.null(dat())){
    selectInput("fishery", "Fisheries:", choices=c(levels(dat()$FISHERIES)),selected=c(levels(dat()$FISHERIES)), multiple=T)
  } 
})

#homeport#
output$placeUnit <- renderUI({
  if(!is.null(dat())){
    selectInput("placeUnit", "Geographic unit", choices=c("State", "Homeport"))
  }
})

output$place <- renderUI({
  if(!is.null(input$placeUnit)){
    if(input$placeUnit == "Homeport"){
      selectInput("place", "Geographic location:", choices=c(unique(dat()$HOMEPT)), selected=c(unique(dat()$HOMEPT)), multiple=TRUE)
        }else selectInput("place", "Geographic location:", choices=c(unique(dat()$STATE)), selected=c(unique(dat()$STATE)), multiple=TRUE)
        
  }else return()
})

# #delivery port#
# output$delivUnit <- renderUI({
#   if(!is.null(dat()){
#     selectInput("delivport")
#   }
# })


output$length <- renderUI({
  if(!is.null(dat())){
    selectInput("length", "Vessel length class:", choices=c(levels(dat()$VSSLNGCLASS)), selected=c(levels(dat()$VSSLNGCLASS)), multiple=TRUE)
  }
})

output$costtyp <- renderUI({
  if(!is.null(dat()) && input$dat.name=="Disaggregated Cost"){
    selectInput("costtyp", "Cost type:", choices=c(levels(dat()$COSTTYPCAT)), selected=c(levels(dat()$COSTTYPCAT)), multiple=TRUE)
  } else return(NULL)
})


########################################### Data subsetting action button ################################

output$dataButton <- renderUI({
  actionButton("dataButton",label=" Select Data", icon=icon("filter"))
})

########################################### begin wellPanel2, plot options ###############################

output$by.var <- renderUI({
  if(plotGo()){
    selectInput("by.var", "By:", choices=c("Survey year", "Fishery"))
  } else return()
})

output$group.var <- renderUI({
  if(plotGo()){
    selectInput("group.var", "Group/color:", group.choices())
  }
})

output$facet.var <- renderUI({
  if(plotGo()){
    selectInput("facet.var", "Facet:", c(facet.choices()), selected="None")
  }
})

output$stat <- renderUI({
  if(plotGo()){
    selectInput("stat", "Summary statistic:", choices= c("sum", "mean", "N"))
  }
})

output$plotType <- renderUI({
  if(plotGo()){
    selectInput("plotType", "Plot type:", choices= c("bar", "point", "line"))
  }
})

output$dodge <- renderUI({
  if(!is.null(input$plotType)){
    if(input$plotType=="bar"){
      radioButtons("dodge", "Position:", choices= c("stack", "dodge"))
    } else return()
  } else return()
})

output$groupMean <- renderUI({
  if(plotGo()){
    checkboxInput("groupMean", "Group mean CI", value=F)
  }
})

output$palette <- renderUI({
  if(plotGo()){
    selectInput("palette", "Palette:", choices=c("CB-friendly", "Brewer", "Hipster1"))
  }
})

############################################## Plotting action button #########################################

output$plotButton <- renderUI({
  if(!is.null(dat.sub())) actionButton("plotButton", label=" Plot Data", icon=icon("bar-chart-o"))
})