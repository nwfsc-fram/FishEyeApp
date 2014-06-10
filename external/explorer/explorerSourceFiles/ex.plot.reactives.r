# this file handles all of the reactives for the plot output. Everyhing is entered on the explorer.r file as ggplot elements

plotBase <- reactive({
  if(!is.null(input$plotType)){
    ggplot(eval(dat.cast(), envir=parent.frame()), aes_string(x=byvar(), y=dat.measure.var(), fill=groupvar()))
  }
})

plotGeom <- reactive({
  if(!is.null(input$plotType)){
    if(input$plotType=="point"){
      geom_point(aes_string(color=groupvar(), size=4))
    } else if (input$plotType=="line"){
      geom_line(aes_string(group=groupvar(),colour=groupvar()), size=2)
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
    if(input$palette=="Brewer"){
      if(input$plotType=="bar"){
        scale_fill_brewer(type="qual", palette="Paired")
        } else scale_color_brewer(type="qual", palette = "Paired")
    } else if(input$palette=="Hipster1"){
        if(input$plotType=="bar"){
        scale_fill_manual(values= wes.palette(5, "Cavalcanti"))
        } else scale_colour_manual(values= wes.palette(5, "Cavalcanti"))
    } else return() 
  }
})

plotLabels <- reactive({
  if(!is.null(input$plotType)){
    y.label <- switch(dat.measure.var(),
                      "DISCOST"="Disaggregated costs",
                      "REV"="Revenue")
    labs(x= input$by.var, title=paste(y.label, "by", input$by.var, ifelse(input$facet.var=="None", "",paste("and", input$facet.var, sep=" ")), sep=" "))
  }
})

plotScales <- reactive({
  if(!is.null(input$plotType)){
    y.label <- switch(dat.measure.var(),
                      "DISCOST"="Disaggregated costs",
                      "REV"="Revenue")
    
    scale_y_continuous(paste(y.label, "(millions $)", sep=" "), labels= function(x)format(x/1e6))
  }
})

plotTheme <- reactive({
  if(!is.null(input$plotType)){
  theme(plot.title= element_text(size=rel(2), vjust=1), axis.title= element_text(size=rel(1.5)))
  }
})