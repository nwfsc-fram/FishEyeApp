# this file handles all of the reactives for the plot output. Everyhing is entered on the explorer.r file as ggplot elements

# moved these from ex.reactives page for references in the plot functions
# this just makes life easier if I change the order/names of vars later 

xvar <- reactive({
  if(!is.null(dat.sub())){
    byvar <- "SURVEY_YEAR"
  } else return()
})

yvar <- reactive({
  if(!is.null(dat.sub())){
    if (input$dat.name == "Revenue" & input$topicSelect == "Delivery-port area"){
      yvar <- "REVBYDPORT"    
    } else if (input$dat.name == "Net Revenue"){
      yvar <- "value"
    } else {
      yvar <- switch(input$dat.name,
                     "Revenue" = "REV",
                     "Cost" = "DISCOST")
  }
  }
})

groupvar <- reactive({
  if(!is.null(dat.sub())){
    if(input$dat.name == "Net Revenue"){
      groupvar <- "variable"
    } else {
      groupvar <- switch(input$topicSelect,
                      "Fisheries" = "FISHERIES",
                      "Home-port area" = "HOMEPT",
                      "Vessel length class" = "VSSLNGCLASS",
                      "Delivery-port area" = "DELIVERYPT",
                      "Cost type" = "COSTTYPCAT")
    }
  }
})

plotBase <- reactive({
  if(!is.null(dat.sub())){
    ggplot(dat.sub(), aes_string(x=xvar(), y=yvar(), fill=groupvar()))
  }
})

plotGeom <- reactive({
  if(!is.null(input$plotType)){
    if(input$plotType=="point"){
      geom_point(aes_string(color=groupvar(), size=4))
    } else if (input$plotType=="line"){
      geom_line(aes_string(group=groupvar(),colour=groupvar()), size=2)
    } else {
      if(!is.null(input$dodge) && input$dodge =="dodge"){
        geom_bar(stat="identity", position="dodge")
      } else geom_bar(stat="identity", postiion="stack") 
    }
  }
})

# plotFacet <- reactive({
#   if(!is.null(facetvar())){
#     if(input$facet.var != "None"){  
#       facet_grid(paste(".", "~", facetvar(), sep=""))
#     } else return ()
#   }
# })

plotGroupMean <- reactive({
  if(!is.null(input$plotType)){
    if(input$groupMean == TRUE){
      geom_boxplot(aes_string(group=xvar(), fill=xvar(), alpha=.5))
    }
  }
})

plotPalette <- reactive({
  if(!is.null(input$plotType)){
    if(input$palette=="Brewer"){
      if(input$plotType=="bar"){
        scale_fill_brewer(type="qual", palette="Paired")
        } else scale_color_brewer(type="qual", palette = "Paired")
    } else return() 
  }
})

plotLabels <- reactive({
  if(!is.null(input$plotType)){
    labs(x= input$xvar, title=paste(input$dat.name, "by", "Years", "and", input$topicSelect, sep=" "))
  }
})

plotScales <- reactive({
  if(!is.null(input$plotType)){
    scale_y_continuous(paste(input$dat.name, "(millions $)", sep=" "), labels= function(x)format(x/1e6))
  }
})

plotTheme <- reactive({
  if(!is.null(input$plotType)){
  theme(plot.title= element_text(size=rel(2), vjust=1), axis.title= element_text(size=rel(1.5)))
  }
})