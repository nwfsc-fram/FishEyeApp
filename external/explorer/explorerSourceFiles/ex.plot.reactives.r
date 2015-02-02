# this file handles all of the reactives for the plot output. Everyhing is entered on the explorer.r file as ggplot elements

# moved these from ex.reactives page for references in the plot functions
# this just makes life easier if I change the order/names of vars later 


xvar <- reactive({
  input$DataButton
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
        yvar <- "value"
    }
  )
})


groupvar <- reactive({
  input$dataButton
  isolate(
  if(!is.null(dat.sub())){
    if(length(input$dat.name) < 2){
      groupvar <- "category"
    } else {
      groupvar <- "variable"
    }
  }
)
})


plotFacet <- reactive({
  input$dataButton
  isolate(
    if(!is.null(input$plotType)) {
      if(length(input$dat.name) > 1) {
        facet_wrap(~ category, as.table = TRUE)
      } else NULL
    }
  )
})


plotBase <- reactive({
  input$dataButton
  isolate(
    if(!is.null(input$plotType)) {
      ggplot(dat.sub(), aes_string(x=xvar(), y=yvar(), fill=groupvar()))  
    }
  )
})


plotGeom <- reactive({ 
  input$dataButton
  isolate(
    if(!is.null(input$plotType)) {
      if(input$plotType=="Point"){
        geom_point(aes_string(color=groupvar(), size=5))
      } else if (input$plotType=="Line"){
        geom_line(aes_string(group=groupvar(),colour=groupvar()), size=2)
      } else {
        if(!is.null(input$dodge) && input$dodge =="Stacked position"){
          geom_bar(stat="identity", position="stack")
        } else geom_bar(stat = "identity", position="dodge") 
      }
    }
  )
})


plotPalette <- reactive({ # handles palette choices (palettes are defined in serverHead.R)
  input$dataButton
  isolate(
    if(length(input$dat.name) == 1) { # for single rev/cost selections the input$topicSelect is used for groups we use netRev palettes from EDC exec summaries
      if(input$topicSelect == "Fisheries") { # assinging hardcoded colors for each fishery
        if(input$plotType == "Bar") {
          scale_fill_manual(values = pal.fisheries) # for barcharts
        } else scale_colour_manual(values = pal.fisheries) # for other graphs
      } else { # for non fishery selections just use the the ggtheme's for now. Can be hardcoded later
        if(input$plotType == "Bar") {
          scale_fill_stata("s2color")
        } else scale_color_stata("s2color")
      }
    } else { # if dat.name is greater that one the netrev groups are displayed and netrev palettes are used
     if(input$plotType == "Bar") {
       scale_fill_manual(values = pal.netrev)
     } else scale_colour_manual(values = pal.netrev)
    }
  )
})


plotLabels <- reactive({
  input$dataButton
  isolate(
    if(!is.null(input$plotType)){
    operations <- "West Coast"
    
    selection <- input$topicSelect
                     
    netrevType <- ifelse(length(input$dat.name) > 1, "Net Revenue", input$dat.name)
    
    labs(x= input$xvar, title= "SAMPLE DATA FOR WEBSITE CONFIGURATION PURPOSES ONLY"
    )
  }
)
})


plotScales <- reactive({
  input$dataButton
  isolate(
  if(!is.null(input$plotType)){ # pause output until plotType is rendered
    if(input$stat == "Average") {
      scale_y_continuous(paste("(thousands $)", sep=" "), labels= function(x) format(x/1e3))
    } else {
      scale_y_continuous(paste("(millions $)", sep=" "), labels= function(x) format(x/1e6))
    }
  }
  )
})


plotTheme <- reactive({
  input$dataButton
  isolate(
    if(!is.null(input$plotType)){
      theme(plot.title = element_text(size=rel(2), vjust=1, colour="grey25"), axis.title = element_text(size=rel(1.8), colour="grey25", vjust=1), legend.title = element_text(size=rel(1.2), colour="grey25", vjust=1))
    }
  )
})


# assemble ggplot components in Voltron like fashion
plotOut <- reactive({
    if(!is.null(input$dataButton) && input$dataButton > 0) {
      plotBase() + plotGeom() + plotFacet() + plotPalette() + plotLabels() + plotScales() + plotTheme()
    }
})
