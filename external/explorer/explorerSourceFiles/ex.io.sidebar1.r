#this page handles all of the reactive expressions for the dynamic user interface

# Inputs variables will be pulled at this stage because we are not preloading data via .r files

#labeled "Years"
output$years <- renderUI({
#   if(!is.null(input$dat.name)){
    checkboxGroupInput("years", "Years:", choices=c(unique(dat.vars()$SURVEY_YEAR)), selected="")
#   }
})

# labeled "Topic"
output$dat.name <- renderUI({ #data selection. there are two reactives that are dependent on these names in ex.reactives
  checkboxGroupInput("dat.name", "Revenue/Cost type:", choices=c("Revenue", "Variable cost", "Fixed cost", "Variable cost net revenue", "Total cost net revenue"), 
              selected = "")
})

# labeled "Category
# these topic names are used all over the rest of the scripts, careful when you change them..
# in javascript control on ex.fluidPage, reactives in both dat.var and subsetting, plot.reactives
output$topicSelect <- renderUI({ 
#   if(!is.null(input$dat.name)){
    selectInput("topicSelect", "Summarize by:", 
                choices = c("", "Fisheries","Vessel length class", "Homeport", "State"),
                selected = "")
#   } else return()
})

# labeled summary satistic

output$stat <- renderUI({
#   if(!is.null(input$topicSelect)){
    selectInput("stat", "Summary statistc:", choices = c("", "Total", "Average"), selected = "", multiple = FALSE)
#   }
})

#################################

#the following are the choices from the specified category

#################################

output$fishery <- renderUI({
  if(!is.null(input$topicSelect)){      
      checkboxGroupInput("fishery", "", choices = c(unique(dat.vars()$FISHERIES)), selected="")
    }
})

output$topics <- renderUI({
  if(!is.null(input$topicSelect)){
#     if(input$dat.name != "Net Revenue"){
      if(input$topicSelect == "Homeport"){
        checkboxGroupInput("topics", "", choices=c(unique(dat.vars()$HOMEPT[!is.na(dat.vars()$HOMEPT)])), selected="")
      } else if(input$topicSelect == "State"){
        checkboxGroupInput("topics", "", choices=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])), selected="")
      } else if (input$topicSelect == "Fisheries") {
        checkboxGroupInput("topics", "", choices = c(unique(dat.vars()$FISHERIES)), selected="")
      } else if (input$topicSelect == "Vessel length class") {
        checkboxGroupInput("topics", "", choices=c(unique(dat.vars()$VSSLNGCLASS)), selected="")
      } else return()   
  } else return()
})
# 
# output$delivPort <- renderUI({
#   if(!is.null(input$placeUnit) && input$dat.name=="Revenue"){
#     if(input$placeUnit == "Port"){
#       checkboxGroupInput("delivPort", "", choices=c(unique(dat.vars()$DELIVERYPT[!is.na(dat.vars()$DELIVERYPT)])), selected=c(unique(dat.vars()$DELIVERYPT[!is.na(dat.vars()$DELIVERYPT)])))
#     }else checkboxGroupInput("delivPort", "", choices=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])), selected=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])))    
#   }else return()
# })


output$length <- renderUI({
  if(!is.null(input$topicSelect)){
#     if(input$dat.name != "Net Revenue"){
    checkboxGroupInput("length", "", choices=c(unique(dat.vars()$VSSLNGCLASS)), selected="")
#     } else {
#     selectInput("length", "", choices=c(unique(dat.vars()$VSSLNGCLASS)), multiple = F)  
#     }
  }
})

# output$costtyp <- renderUI({
#   if(!is.null(input$dat.name) && input$dat.name=="Cost"){
#     selectInput("costtyp", "", choices=c(unique(dat.vars()$COSTTYPCAT)), selected=c(unique(dat.vars()$COSTTYPCAT)))
#   } else return()
# })

####################################

# remove ak button to specify WC only
# taking this out for net rev stuff

####################################

# output$removeAK <- renderUI({
#   if(!is.null(input$topicSelect)){
#     if(input$dat.name == "Net Revenue") {
#       selectInput("removeAK", "", choices = c("West Coast only operations"))  
#     } else if(input$topicSelect == "Delivery port"){
#       selectInput("removeAK", "", choices = c("All fishery operations"))
#     } else if(input$dat.name == "Revenue"){
#       selectInput("removeAK", "", choices = c("All fishery operations"))  
#     } else selectInput("removeAK", "", choices = c("All fishery operations", "West Coast only operations"))
#   } else return()
# })


output$plotType <- renderUI({
    selectInput("plotType", "Plot type:", choices= c("", "Bar", "Point", "Line"), selected=c(""))
})

output$dodge <- renderUI({
  if(!is.null(input$plotType)) {
    if(input$plotType == "Bar") {
      radioButtons("dodge", "", choices= c("Grouped position", "Stacked position"))
    } else return()
  } else return()
})

########################################### Data subsetting action button ################################

output$dataButton <- renderUI({
  if(permitPlot()) {
    actionButton("dataButton", label=" Plot Data", icon=icon("bar-chart-o"))
  }
})

########################################### begin wellPanel2, plot options ###############################

# output$stat <- renderUI({
#   if(dat.sub()){
#     selectInput("stat", "Summary statistic:", choices= c("sum", "mean", "N"))
#   }
# })


# output$groupMean <- renderUI({
#   if(!is.null(dat.sub())){
#     checkboxInput("groupMean", "Group mean CI", value = F)
#   } 
# })
