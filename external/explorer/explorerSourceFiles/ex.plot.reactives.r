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
    if(!is.null(dat.sub())) {
      if(length(input$dat.name) > 1) {
        facet_wrap(~ category, as.table = TRUE)
      } else NULL
    }
  )
})

plotBase <- reactive({
    if(!is.null(input$dataButton) && input$dataButton > 0) {
      if(input$plotType == "Bar") {
        ggplot(dat.sub(), aes_string(x=xvar(), y=yvar(), fill=groupvar()))
      } else {
        ggplot(dat.sub(), aes_string(x=xvar(), y=yvar(), color=groupvar()))
      }  
    }
})

plotGeom <- reactive({  
  if(!is.null(input$dataButton) && input$dataButton > 0 ) {
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
})
# 
# plotGroupMean <- reactive({
#   if(!is.null(input$groupMean)){
#     if(input$groupMean == TRUE){
#       geom_boxplot(aes_string(group=xvar(), fill=xvar(), alpha=.5))
#     } 
#   }
# })

plotPalette <- reactive({
  input$dataButton
  isolate(
    if(input$plotType == "Bar") {
      scale_fill_manual(values = pal.netrev)
    } else scale_colour_manual(values = pal.netrev)
  )
})

plotLabels <- reactive({
  input$dataButton
  isolate(
    if(!is.null(input$plotType)){
    operations <- "West Coast"
    
    selection <- input$topicSelect
                     
    netrevType <- "Net Revenue"
    
    labs(x= input$xvar, title=paste(operations, 
                                    input$stat, 
                                    netrevType,
                                    "by",
                                    selection, sep=" "), 
                                    color= input$topicSelect, 
                                    fill= ifelse(input$dat.name == "Net Revenue", "", input$topicSelect))
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
