plotBase <- reactive({
  if(!is.null(input$plotType)){
    ggplot(eval(dat.cast(), envir=parent.frame()), aes_string(x=byvar(), y=dat.measure.var(), fill=groupvar()))
  }
})

plotGeom <- reactive({
  if(!is.null(input$plotType)){
    if(input$plotType=="point"){
      geom_point(aes_string(colour=groupvar(), size=4))
    } else if (input$plotType=="line"){
      geom_line(aes_string(group=groupvar(), colour=groupvar(), fill=groupvar()), line=4)
    } else {
      if(input$dodge =="dodge"){
        geom_bar(stat="identity", position="dodge")
      } else geom_bar(stat="identity", postiion="stack") 
    }
  }
})

plotFacet <- reactive({
  if(!is.null(input$plotType)){
    if(input$facet.var != "None"){  
      facet_grid(paste(".", "~", facetvar(), sep=""))
    } else return ()
  }
})

plotGroupMean <- reactive({
  if(!is.null(input$plotType)){
    if(input$groupMean == TRUE){
      geom_boxplot(aes_string(group=byvar(), fill=byvar(), alpha=.5))
    }
  }
})

plotPalette <- reactive({
  if(!is.null(input$plotType)){
    if(input$palette=="l45"){
      if(input$plotType=="bar"){
        scale_fill_hue(l=45)
        } else scale_colour_hue(l-45)
    } else if(input$palette=="Hipster1"){
        if(input$plotType=="bar"){
        scale_fill_manual(values= wes.palette(5, "Cavalcanti"))
        } else scale_colour_manual(values= wes.palette(5, "Cavalcanti"))
    } else return() 
  }
})
