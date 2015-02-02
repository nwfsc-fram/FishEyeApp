doPlot <- function(dat, x, y){
  if(!is.null(DatSub())){
    
    # base plot
    g <- ggplot(dat, 
      aes_string(x=x, y=y , group = "SHORTDESCR",
        colour="SHORTDESCR", fill="SHORTDESCR"))
    
    # define geom
    if(input$PlotSelect == "Bar"){
      if(!is.null(input$DodgeSelect) && 
        input$DodgeSelect == "Grouped position"){
        g <- g + geom_bar(stat="identity", position="stack")
      } else {
        g <- g + geom_bar(stat = "identity", position = "dodge")
      }  
    } else if(input$PlotSelect == "Point"){
      g <- g + geom_point(aes_string(size=5))
    } else {
      g <- g + geom_line(aes_string(size=2))
    }
    
    #define facet
    g <- g + facet_wrap(~ VARIABLE, as.table = TRUE)
    
    print(g)
    
  
  } else plot(0,0,type="n", axes=F, xlab="", ylab="")
}