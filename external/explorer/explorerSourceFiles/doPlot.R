doPlot <- function(dat, x, y, type){
  if(PermitPlot()){
    
    # for(i in VARIABLE){
      #create list[i] for each variable
    #}
    
    # type = c("summary", "thirds")
    
    groupVar <- ifelse(type=="summary", "SHORTDESCR", "THIRDS")
    facetVar <- ifelse(type== "summary" , "VARIABLE", "SHORTDESCR")

    
    # Plot title construction
    main <- function(){
      if(type == "summary"){
        sprintf("%s West Coast Operations by %s", input$StatSelect, input$CategorySelect)
      } else {
        sprintf("
                %s West Coast Operations for %s", input$StatSelect, input$VariableSelect)
      }
    }
      
    
    # simple scaling for bar charsts based on number of inputs
    scale_bars <- function(){
#       a = length(input$ShortdesrSelect)
      b = length(input$YearSelect)
      
      if(b == 1){
        return(0.25)
      } else if(b == 2){
        return(0.375)
      } else if(b == 3){
        return(0.5)      
      } else{
        return(0.9)
      }
    }
    
    g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar))
    
    # define geom
    if(type == "summary"){
      
      if(input$PlotSelect == "Bar"){
        if(!is.null(input$DodgeSelect)){
          if(input$DodgeSelect == "Grouped position"){
            g <- g + geom_bar(aes_string(fill = groupVar), stat="identity", 
              position="dodge", width = scale_bars())
          } else {
            g <- g + geom_bar(aes_string(fill = groupVar), stat = "identity", 
              position = "stack", width = scale_bars())
          }
        } else return()
      } else if(input$PlotSelect == "Point"){
        g <- g + geom_point(aes_string(colour = groupVar), size=4)
      } else {
        g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
      }
      
    } else {
      if(length(input$YearSelect) > 1){
        g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
      } else {
        g <- g + geom_point(aes_string(colour = groupVar), size=4)
      }
    }
    
    # define facet
    if(type =="summary"){
      g <- g + facet_wrap(~ VARIABLE, as.table = TRUE)
    } else {
      g <- g + facet_wrap(~ SHORTDESCR)
    }
    
#     # create annotations for suppressed data
#     len <- length(levels(as.factor(dat$VARIABLE))
#     xyPosition <- data.frame(x=rep(as.numeric(inputinput$YearSelect[1]), len), y=rep(2))
#     g + 
#       
#     
    
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
    g <- g + labs(y = "Thousands ($)", title = main())
    
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
                                size = 16, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
        color = "grey25", face = "bold", size = 12),
      legend.title = element_blank())
    
    print(g)
      
   } else plot(0,0,type="n", axes=F, xlab="", ylab="")
}