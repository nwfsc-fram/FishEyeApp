#this page handles all of the reactive expressions for the dynamic user interface

# Inputs variables will be pulled at this stage because we are not preloading data via .r files

#labeled "Years"
output$years <- renderUI({
  if(!is.null(input$dat.name)){
    checkboxGroupInput("years", "Years:", choices=c(unique(dat.vars()$SURVEY_YEAR)), selected=c(unique(dat.vars()$SURVEY_YEAR)))
  }
})

# labeled "Topic"
output$dat.name <- renderUI({ #data selection. there are two reactives that are dependent on these names in ex.reactives
  selectInput("dat.name", "Topic:", choices=c("Cost", "Revenue", "Net Revenue"))
})

# labeled "Category
# these topic names are used all over the rest of the scripts, careful when you change them..
# in javascript control on ex.fluidPage, reactives in both dat.var and subsetting, plot.reactives
output$topicSelect <- renderUI({ 
  if(!is.null(input$dat.name)){
    if(input$dat.name == "Cost"){
    selectInput("topicSelect", "Category:", 
                choices = c("Fisheries","Vessel length class", "Homeport"),
                selected = c("Fisheries"))
    } else if(input$dat.name == "Revenue") {
      selectInput("topicSelect", "Category:", 
                  choices = c("Fisheries", "Vessel length class", "Homeport", "Delivery port"),
                  selected = c("Fisheries"))
    } else if (input$dat.name == "Net Revenue") {
      selectInput("topicSelect", "Category:",
                  choices = c("Fisheries", "Vessel length class", "Homeport"),
                  selected = c("Fisheries"))
    } else return()
  }
})

output$placeUnit <- renderUI({
  if(!is.null(input$topicSelect)){
#     if(input$topicSelect == "Delivery port"){
      selectInput("placeUnit", "", choices = c("State", "Port"))
#     } else if(input$topicSelect == "Homeport"){
#       selectInput("placeUnit", "", choices = c("State", "Port"))
    } else  return()
#   }
})

#################################

#the following are the choices from the specified category

#################################

output$fishery <- renderUI({
  if(!is.null(input$removeAK)){
    if(input$dat.name != "Net Revenue"){
      if(input$removeAK == "West Coast only operations"){
        checkboxGroupInput("fishery", "", choices = c(unique(dat.vars()$FISHERIES)[-1]), selected=c(unique(dat.vars()$FISHERIES)[-1]))
      } else {
        checkboxGroupInput("fishery", "", choices = c(unique(dat.vars()$FISHERIES)), selected=c(unique(dat.vars()$FISHERIES)))
      }
    } else {
      if(input$removeAK == "West Coast only operations"){
        selectInput("fishery", "", choices = c(unique(dat.vars()$FISHERIES)[-1]), multiple = F)
      } else {
        selectInput("fishery", "", choices = c(unique(dat.vars()$FISHERIES)), multiple = F)
      }
    }
  }
})

output$place <- renderUI({
  if(!is.null(input$placeUnit)){
    if(input$dat.name != "Net Revenue"){
      if(input$placeUnit == "Port"){
        checkboxGroupInput("place", "", choices=c(unique(dat.vars()$HOMEPT[!is.na(dat.vars()$HOMEPT)])), selected=c(unique(dat.vars()$HOMEPT[!is.na(dat.vars()$HOMEPT)])))
      } else checkboxGroupInput("place", "", choices=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])), selected=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])))
    } else {
      if(input$placeUnit == "Port"){
        selectInput("place", "", choices=c(unique(dat.vars()$HOMEPT[!is.na(dat.vars()$HOMEPT)])), multiple = F)
      } else selectInput("place", "", choices=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])), multiple = F)  
    }    
  }else return()
})

output$delivPort <- renderUI({
  if(!is.null(input$placeUnit) && input$dat.name=="Revenue"){
    if(input$placeUnit == "Port"){
      checkboxGroupInput("delivPort", "", choices=c(unique(dat.vars()$DELIVERYPT[!is.na(dat.vars()$DELIVERYPT)])), selected=c(unique(dat.vars()$DELIVERYPT[!is.na(dat.vars()$DELIVERYPT)])))
    }else checkboxGroupInput("delivPort", "", choices=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])), selected=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])))    
  }else return()
})


output$length <- renderUI({
  if(!is.null(input$dat.name)){
    if(input$dat.name != "Net Revenue"){
    checkboxGroupInput("length", "", choices=c(unique(dat.vars()$VSSLNGCLASS)), selected=c(unique(dat.vars()$VSSLNGCLASS)))
    } else {
    selectInput("length", "", choices=c(unique(dat.vars()$VSSLNGCLASS)), multiple = F)  
    }
  }
})

# output$costtyp <- renderUI({
#   if(!is.null(input$dat.name) && input$dat.name=="Cost"){
#     selectInput("costtyp", "", choices=c(unique(dat.vars()$COSTTYPCAT)), selected=c(unique(dat.vars()$COSTTYPCAT)))
#   } else return()
# })

####################################

#remove ak button to specify WC only

####################################

output$removeAK <- renderUI({
  if(!is.null(input$topicSelect)){
    if(input$dat.name == "Net Revenue") {
      selectInput("removeAK", "", choices = c("West Coast only operations"))  
    } else if(input$topicSelect == "Delivery port"){
      selectInput("removeAK", "", choices = c("All fishery operations"))
    } else if(input$dat.name == "Revenue"){
      selectInput("removeAK", "", choices = c("All fishery operations"))  
    } else selectInput("removeAK", "", choices = c("All fishery operations", "West Coast only operations"))
  } else return()
})

########################################### Data subsetting action button ################################

output$dataButton <- renderUI({
  actionButton("dataButton",label=" Plot Data", icon=icon("bar-chart-o"))
})

########################################### begin wellPanel2, plot options ###############################

# output$stat <- renderUI({
#   if(dat.sub()){
#     selectInput("stat", "Summary statistic:", choices= c("sum", "mean", "N"))
#   }
# })

output$plotType <- renderUI({
  if(!is.null(dat.sub())){
    selectInput("plotType", "Plot type:", choices= c("Bar", "Point", "Line"))
  } else return()
})

output$dodge <- renderUI({
  if(!is.null(input$plotType)){
    if(input$plotType=="Bar"){
      radioButtons("dodge", "Position:", choices= c("Dodge", "Stack"))
    } else return()
  } else return()
})

output$groupMean <- renderUI({
  if(!is.null(dat.sub())){
    checkboxInput("groupMean", "Group mean CI", value=F)
  } 
})

output$palette <- renderUI({
  if(!is.null(dat.sub())){
    selectInput("palette", "Palette:", choices=c("Default", "Color-blind friendly"), selected = "Default")
  }
})

############################################## Plotting action button #########################################

# output$plotButton <- renderUI({
#   if(!is.null(dat.sub())) actionButton("plotButton", label=" Plot Data", icon=icon("bar-chart-o"))
# })