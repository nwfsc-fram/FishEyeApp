doPlot <- function(dat, x, y){
#   if(!is.null(DatSub()) & length(input$ShortdescrSelect) > 0){
    
    groupVar <- "SHORTDESCR"
    facetVar <- "VARIABLE"
    
    # Plot title construction
    main <- function(){
      stat <- switch(input$StatSelect,
      "Average" = "mean",
      "Total" = "sum")
      paste(stat, "West Coast Operations by", input$CategorySelect, sep=" ")
    }
      
    # base plot
    g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar))
    
    # define geom
    if(input$PlotSelect == "Bar"){
      if(!is.null(input$DodgeSelect)){
        if(input$DodgeSelect == "Grouped position"){
          g <- g + geom_bar(aes_string(fill = groupVar), stat="identity", 
            position="dodge")
        } else {
          g <- g + geom_bar(aes_string(fill = groupVar), stat = "identity", 
            position = "stack")
        }
      } else return()
    } else if(input$PlotSelect == "Point"){
      g <- g + geom_point(aes_string(colour = groupVar), size=4)
    } else {
      g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
    }
    
    # define facet
    g <- g + facet_wrap(~ VARIABLE, as.table = TRUE)
    
    # define scale
    if(input$PlotSelect == "Bar") {
      g <- g + scale_fill_manual(values = pal.netrev)
    } else {
      g <- g + scale_colour_manual(values = pal.netrev)
    }
    
    # defien x scale
#     g <- g +  scale_x_discrete(labels = c("2010" = "", "2011" = ""))
    
    # define solid line y=0
    g <- g + geom_hline(yintercept = 0)
    
    # define labels
    g <- g + labs(x = "Survey Year", y = "", title = main())
    
    # define theme
    g <- g + theme(
      plot.title = element_text(size=rel(2), vjust=1, colour="grey25"), 
      plot.title = element_text(family = "sans", face = "bold", vjust = 1),
      panel.background = element_rect(fill = "white"),
      panel.margin = unit(1.2, "lines"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", 
        size = 12, face = "bold", color = "black"),
      strip.background = element_rect(fill = "white"),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
        color = "black", face = "bold", size = 12),
      legend.title = element_blank())
    
    print(g)
    
  
#   } else plot(0,0,type="n", axes=F, xlab="", ylab="")
}