#this page handles all of the reactive expressions for the dynamic user interface
#this file is getting a bit long, consider splitting into two parts

output$dat.name <- renderUI({ #data selection. there are two reactives that are dependent on these names in ex.reactives
  selectInput("dat.name", "Measured variable (y-axis):", choices=c("", "Cost", "Revenue"), selected="")
})

#depricated
# output$subsetChoice <- renderUI({
#   if(!is.null(dat())){
#     checkboxInput("subsetChoice", "View subsetting options", value= TRUE)
#   }
# })

# Inputs variables will be pulled at this stage because we are not preloading data via .r files
output$years <- renderUI({
  if(!is.null(input$dat.name)){
    selectInput("years", "Years (x-axis):", choices=c(levels(dat.vars()$SURVEY_YEAR)), selected=c(levels(dat.vars()$SURVEY_YEAR)), multiple=T)
  }
})

# 'Topic' or 'Variable' select
output$topicSelect <- renderUI({
  if(!is.null(input$dat.name)){
    if(input$dat.name == "Cost"){
    selectInput("topicSelect", "Variable of interest (Group/Color):", 
                choices = c("Fisheries","Vessel length class", "Home-port area", "Cost type" ),
                selected = c("Fisheries"))
    } else {
      selectInput("topicSelect", "Variable of interest (Group/Color):", 
                  choices = c("Fisheries", "Vessel length class", "Home-port area", "Delivery-port area"),
                  selected = c("Fisheries"))
    }
  }
})

# output$topicVars <- renderUI({
#   if(!is.null(dat())){
#     if(input$topicSelect == "Fisheries"){ 
#           checkboxGroupInput("topicVars", "", choices = c(levels(dat()$FISHERIES)),selected=c(levels(dat()$FISHERIES)))
#     } else if(input$topicSelect == "Home-port area"){ 
#           if(input$placeUnit == "State"){ checkboxGroupInput("topicVars", "", choices = c(unique(dat()$STATE[!is.na(dat()$STATE)])), selected = c(unique(dat()$STATE[!is.na(dat()$STATE)])))  
#           } else checkboxGroupInput("topicVars", "", choices = c(unique(dat()$HOMEPT[!is.na(dat()$HOMEPT)])), selected = c(unique(dat()$HOMEPT[!is.na(dat()$HOMEPT)])))
#     } else if(input$topicSelect == "Vessel length class"){
#           checkboxGroupInput("topicVars", "", choices = c(levels(dat()$VSSLNGCLASS)), selected = c(levels(dat()$VSSLNGCLASS)))
#     } else if(input$topicSelect == "Cost type"){
#           checkboxGroupInput("topicVars", "", choices = c(levels(dat()$COSTTYPCAT)), selected = c(levels(dat()$COSTTYPCAT)))
#     } else {
#           if(input$placeUnit == "State"){ checkboxGroupInput("topicVars", "", choices = c(unique(dat()$DELIVERYST[!is.na(dat()$DELIVERYST)])), selected = c(unique(dat()$DELIVERYST[!is.na(dat()$DELIVERYST)])))  
#           } else checkboxGroupInput("topicVars", "", choices = c(unique(dat()$DELIVERYPT[!is.na(dat()$DELIVERYPT)])), selected = c(unique(dat()$DELIVERYPT[!is.na(dat()$DELIVERYPT)])))  
#     }
#   } 
# })

#homeport#
output$placeUnit <- renderUI({
  if(!is.null(input$topicSelect)){
    if(input$topicSelect == "Delivery-port area"){
      selectInput("placeUnit", "Delivery-port unit:", choices = c("State", "Port"))
    } else if(input$topicSelect == "Home-port area"){
      selectInput("placeUnit", "Home-port unit:", choices = c("State", "Port"))
    } else  return()
  }
})

output$fishery <- renderUI({
  if(!is.null(input$dat.name)){
    checkboxGroupInput("fishery", "", choices = c(levels(dat.vars()$FISHERIES)),selected=c(levels(dat.vars()$FISHERIES)))
  } 
})



output$place <- renderUI({
  if(!is.null(input$placeUnit)){
    if(input$placeUnit == "Port"){
      checkboxGroupInput("place", "", choices=c(unique(dat.vars()$HOMEPT[!is.na(dat.vars()$HOMEPT)])), selected=c(unique(dat.vars()$HOMEPT[!is.na(dat.vars()$HOMEPT)])))
        }else checkboxGroupInput("place", "", choices=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])), selected=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])))
        
  }else return()
})
# 
# #delivery port#
# output$delivUnit <- renderUI({
#   if(!is.null(dat()) && input$dat.name=="Revenue"){
#     selectInput("delivUnit", "Delivery port unit", choices=c("State", "Port"))
#   }
# })

output$delivPort <- renderUI({
  if(!is.null(input$placeUnit) && input$dat.name=="Revenue"){
    if(input$placeUnit == "Port"){
      checkboxGroupInput("delivPort", "", choices=c(unique(dat.vars()$DELIVERYPT[!is.na(dat.vars()$DELIVERYPT)])), selected=c(unique(dat.vars()$DELIVERYPT[!is.na(dat.vars()$DELIVERYPT)])))
    }else checkboxGroupInput("delivPort", "", choices=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])), selected=c(unique(dat.vars()$STATE[!is.na(dat.vars()$STATE)])))    
  }else return()
})


output$length <- renderUI({
  if(!is.null(input$dat.name)){
    checkboxGroupInput("length", "", choices=c(levels(dat.vars()$VSSLNGCLASS)), selected=c(levels(dat.vars()$VSSLNGCLASS)))
  }
})

output$costtyp <- renderUI({
  if(!is.null(input$dat.name) && input$dat.name=="Cost"){
    checkboxGroupInput("costtyp", "", choices=c(levels(dat.vars()$COSTTYPCAT)), selected=c(levels(dat.vars()$COSTTYPCAT)))
  } else return()
})

output$removeAK <- renderUI({
  if(!is.null(input$topicSelect)){
    if(input$topicSelect != "Fisheries"){
      checkboxInput("removeAK", "West Coast fisheries only (exclude AK)", value = T)
    } else return()
  }
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

# output$plotButton <- renderUI({
#   if(!is.null(dat.sub())) actionButton("plotButton", label=" Plot Data", icon=icon("bar-chart-o"))
# })