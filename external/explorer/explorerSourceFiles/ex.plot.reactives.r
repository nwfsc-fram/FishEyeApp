# this file handles all of the reactives for the plot output. Everyhing is entered on the explorer.r file as ggplot elements

# moved these from ex.reactives page for references in the plot functions
# this just makes life easier if I change the order/names of vars later 

xvar <- reactive({
  input$dataButton
  isolate(
  if(!is.null(dat.sub())){
    byvar <- "SURVEY_YEAR"
  } else return()
)
})

yvar <- reactive({
  input$dataButton
  isolate(
  if(!is.null(dat.sub())){  
    if (input$dat.name == "Net Revenue"){
      yvar <- "value"
    } else {
      yvar <- switch(input$dat.name,
                     "Revenue" = "REV",
                     "Cost" = "DISCOST")
  }
  }
)
})

groupvar <- reactive({
  input$dataButton
  isolate(
  if(!is.null(dat.sub())){
    if(input$dat.name == "Net Revenue"){
      groupvar <- "variable"
    } else {
      groupvar <- switch(input$topicSelect,
                      "Fisheries" = "FISHERIES",
                      "Homeport" = ifelse(input$placeUnit == "Port" , "HOMEPT", "STATE"),
                      "Vessel length class" = "VSSLNGCLASS",
                      "Delivery port" = ifelse(input$placeUnit == "Port", "DELIVERYPT", "DELIVERYST"),
                      "Cost type" = "COSTTYPCAT")
    }
  }
)
})

plotBase <- reactive({
  input$dataButton
  isolate(
  if(!is.null(dat.sub())){
    ggplot(dat.sub(), aes_string(x=xvar(), y=yvar(), fill=groupvar()))
  }
)
})

plotGeom <- reactive({
  if(!is.null(input$plotType)){
    if(input$plotType=="Point"){
      geom_point(aes_string(color=groupvar(), size=5))
    } else if (input$plotType=="Line"){
      geom_line(aes_string(group=groupvar(),colour=groupvar()), size=2)
    } else {
      if(!is.null(input$dodge) && input$dodge =="Stack"){
        geom_bar(stat="identity", position="stack")
      } else geom_bar(stat = "identity", position="dodge") 
    }
  }
})

plotGroupMean <- reactive({
  if(!is.null(input$plotType)){
    if(input$groupMean == TRUE){
      geom_boxplot(aes_string(group=xvar(), fill=xvar(), alpha=.5))
    } 
  }
})

plotPalette <- reactive({
  input$dataButton
  isolate(
  if(!is.null(input$plotType)){
    if(input$dat.name != "Net Revenue"){
      
      if(input$palette=="Default"){
        if(input$plotType=="Bar"){
          scale_fill_brewer(type="qual", palette="Paired")
          } else scale_color_brewer(type="qual", palette = "Paired")
      } else return()
      
    } else {
      if(input$palette=="Default"){
        if(input$plotType=="Bar"){
          scale_fill_manual(values=pal.netrev)
        } else scale_color_manual(values=pal.netrev)
      } else return()
    }
    
  }
  )
})

plotLabels <- reactive({
  input$dataButton
  isolate(
    if(!is.null(input$plotType)){
    operations <- switch(input$removeAK,
                         "West Coast only operations" = "West Coast",
                         "All fishery operations" = "")
    
    selection <- ifelse(input$topicSelect == "Fisheries", input$fishery, 
                        ifelse(input$topicSelect == "Homeport", input$place, 
                               ifelse(input$topicSelect == "Vessel length class", input$length,
                                      ifelse(input$topicSelect == "Delivery port", input$delivPort))))
  
    labs(x= input$xvar, title=paste(operations, 
                                    "Average", input$dat.name, 
                                    ifelse(input$dat.name == "Net Revenue", "-","by"), 
                                    ifelse(input$dat.name == "Net Revenue", selection,input$topicSelect), sep=" "), 
                                    color= input$topicSelect, 
                                    fill= ifelse(input$dat.name == "Net Revenue", "", input$topicSelect))
  }
)
})

plotScales <- reactive({
  input$dataButton
  isolate(
  if(!is.null(input$plotType)){
    scale_y_continuous(paste(input$dat.name, "(thousands $)", sep=" "), labels= function(x) format(x/1e3))
  }
)
})

plotTheme <- reactive({
  if(!is.null(input$plotType)){
  theme(plot.title = element_text(size=rel(2), vjust=1, colour="grey25"), axis.title = element_text(size=rel(1.8), colour="grey25", vjust=1), legend.title = element_text(size=rel(1.2), colour="grey25", vjust=1))
  }
})
