doPlot <- function(dat, x, y, type){
  if(PermitPlot()){
    
#     print("And I am the input to the plot!!!")
#     print(head(dat))
#     print(str(dat))
    
    groupVar <- ifelse(type=="summary", "SHORTDESCR", "THIRDS")
    facetVar <- ifelse(type== "summary" , "VARIABLE", "SHORTDESCR")
   # length(table(as.numeric(DatSub()[,1])))
    
    # Plot title construction
    main <- function(){
         
      if(type == "summary"){
        if(input$PlotSelect!="Bar"){
          sprintf(paste("Summary Economic Measures for West Coast Catcher Vessels:", input$CategorySelect,
                        "\nStatistic: ", input$StatSelect))  
        } else {
        if(input$DodgeSelect == "Compare economic measures side-by-side"){
        sprintf(paste("Summary Economic Measures for West Coast Catcher Vessels:", input$CategorySelect,
                "\nStatistic: ", input$StatSelect))
        } else if(input$DodgeSelect == "Total cost revenue figure"){
          sprintf(paste("Total cost revenue for West Coast Catcher Vessels:", input$CategorySelect,
                  "\nStatistic: ", input$StatSelect))
        } else if(input$DodgeSelect == "Variable cost revenue figure"){
          sprintf(paste("Variable cost revenue for West Coast Catcher Vessels:", input$CategorySelect,
                  "\nStatistic: ", input$StatSelect))
        }}
        
      } else {
        sprintf(paste("Variability analysis of West Coast Catcher Vessels:",input$VariableSelect, 
                "\nStatistic: ", input$StatSelect))
      }
    }
      
    
    # simple scaling for bar charts based on number of inputs
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
   #   DatSub(), x = "YEAR", y = "VALUE/1000"
        # define geom
    if(type == "summary"){
      if(input$PlotSelect!="Bar"){
      if(input$PlotSelect == "Point"){
           if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
             if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
               
        g <- g + geom_point(aes_string(colour = groupVar), size=4)+  
                 geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey20", alpha=.025)+ 
                 geom_text(aes(x=0.5,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, size=4.25) + 
                 geom_text(aes(x=length(table(as.numeric(YEAR)))+.75,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=1, size=4.25)# +
                 #geom_text(aes(x=length(table(as.numeric(YEAR)))/1.6,y=(max(VALUE)/1000+max(VALUE)/4000),label="West Coast trawl catch shares program"),hjust=.5, size=4.5) 
             #    geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white") 
             } else {
        g <- g + geom_point(aes_string(colour = groupVar), size=4)+  
                geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey20", alpha=.025)+ 
                geom_text(aes(x=0.25,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"), size=4.2, hjust=0) + 
                geom_text(aes(x=length(table(as.numeric(YEAR)))+.5,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"), size=4.25, hjust=1) #+
               # geom_text(aes(x=length(table(as.numeric(YEAR)))/1.5,y=(max(VALUE)/1000+max(VALUE)/4000),label="West Coast trawl catch shares program"), size=4.5, hjust=.5) 
             #   geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white") 
               
             }} else {
               g <- g + geom_point(aes_string(colour = groupVar), size=4)    
             }
      } else {
        if(length(input$YearSelect)==1){
          g <- g + geom_point(aes_string(colour = groupVar), size=4)
        }
        if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
          if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
            g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+  
                geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), alpha=.025)+ 
                geom_text(aes(x=0.5,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, size=4.25) + 
                geom_text(aes(x=length(table(as.numeric(YEAR)))+.75,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=1, size=4.25)# +
                #geom_text(aes(x=2.5,y=(max(VALUE)/1000+max(VALUE)/4000),label="West Coast trawl catch shares program")) 
                #geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white") 
          } else {
            g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+ 
                geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), alpha=.025)+ 
                geom_text(aes(x=.25,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, size=4.25) + 
                geom_text(aes(x=length(table(as.numeric(YEAR)))+.5,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=1, size=4.25) #+
               # geom_text(aes(x=1.5,y=(max(VALUE)/1000+max(VALUE)/4000),label="West Coast trawl catch shares program")) 
               # geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white") 
          }} else { 
        g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
          }} # end if statement for line figure
      } 
      if(input$PlotSelect == "Bar"){
        if(!is.null(input$DodgeSelect)){
          if(input$DodgeSelect == "Compare economic measures side-by-side"){
            if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
              if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
                
               g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity",position="dodge", width = scale_bars()) +
                  geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey20", alpha=.02)+ 
                 geom_text(aes(x=0.5,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, size=4.25) + 
                 geom_text(aes(x=length(table(as.numeric(YEAR)))+.75,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=1, size=4.25)# +
                #geom_text(aes(x=2.5,y=(max(VALUE)/1000+max(VALUE)/4000),label="West Coast trawl catch shares program")) 
                  #geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white") #added this line to increase range of y-axis and fit the geom_text labels
     
                         } else  {
                g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="dodge", width = scale_bars()) + 
                        geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey20", alpha=.02)+ 
                        geom_text(aes(x=.25,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, size=4.25) + 
                        geom_text(aes(x=length(table(as.numeric(YEAR)))+.5,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=1, size=4.25) #+
                        #geom_text(aes(x=1.5,y=(max(VALUE)/1000+max(VALUE)/4000),label="West Coast trawl catch shares program")) 
                       # geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white") 
                
              }} # end if-else for adding dashed lines or not (pre- and post- catch shares)
             else {
               
            g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="dodge", width = scale_bars())
            }} #End if else for side-by-side comparion
          
          if(input$DodgeSelect == "Total cost revenue figure"){
      #      groupVar <- factor("SHORTDESCR", levels=c("Fixed costs","Variables costs","Total cost net revenue"))
            if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
              if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
                g <- ggplot(dat, aes_string(x = x, y = y ,group = groupVar, fill=groupVar, order=groupVar))+ # 
                       geom_bar(stat = "identity", position = "stack", width = scale_bars())  + 
                       geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey20", alpha=.04)+ 
                       geom_text(aes(x=.5,y=max(VALUE)/400, label="Pre-catch shares"),hjust=0, size=4.25) + 
                       geom_text(aes(x=length(table(as.numeric(YEAR)))+.75,y=max(VALUE)/400,label="Post-catch shares"),hjust=1, size=4.25)# +
                     #  geom_text(aes(x=2.5,y=max(VALUE)/350,label="West Coast trawl catch shares program")) 
                     #  geom_linerange(aes(ymin=Inf, ymax=Inf),col="white")   
              } else  {
                g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar, fill=groupVar, order=groupVar))+ 
                      geom_bar(stat = "identity", position = "stack", width = scale_bars())  + 
                      geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey20", alpha=.04)+ 
                      geom_text(aes(x=.25,y=max(VALUE)/400, label="Pre-catch shares"),hjust=0, size=4.25) + 
                      geom_text(aes(x=length(table(as.numeric(YEAR)))+.5,y=max(VALUE)/400,label="Post-catch shares"),hjust=1, size=4.25)# +
                    #  geom_text(aes(x=1.5,y=max(VALUE)/350,label="West Coast trawl catch shares program")) 
                    #  geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white")  
              }}
                else {
            g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar, fill=groupVar, order=groupVar))+ geom_bar(stat = "identity", 
                   position = "stack", width = scale_bars())
          }} #end if statement for total cost revenue figure
          
        if(input$DodgeSelect == "Variable cost revenue figure"){
          if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
            if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
              g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar, fill=groupVar, order=groupVar)) + 
                    geom_bar(aes_string(fill = groupVar), stat = "identity", position = "stack", width = scale_bars())+  
                    geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey20", alpha=.05)+ 
                    geom_text(aes(x=.5,y=max(VALUE)/400, label="Pre-catch shares"),hjust=0, size=4.25) + 
                    geom_text(aes(x=length(table(as.numeric(YEAR)))+.75,y=max(VALUE)/400,label="Post-catch shares"),hjust=1, size=4.25)# +
                    #geom_text(aes(x=2.5,y=max(VALUE)/350,label="West Coast trawl catch shares program")) 
                    #geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white")   
            } else  {
              g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar, fill=groupVar, order=groupVar)) + 
                    geom_bar(aes_string(fill = groupVar), stat = "identity", position = "stack", width = scale_bars())+ 
                    geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey20", alpha=.075)+ 
                    geom_text(aes(x=.25,y=max(VALUE)/400, label="Pre-catch shares"),hjust=0, size=4.25) + 
                    geom_text(aes(x=length(table(as.numeric(YEAR)))+.5,y=max(VALUE)/400,label="Post-catch shares"),hjust=1, size=4.25) #+
                    #geom_text(aes(x=1.5,y=max(VALUE)/350,label="West Coast trawl catch shares program")) 
                    #geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white")  
            }} 
              else {
             g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat = "identity", 
                    position = "stack", width = scale_bars())
          }} # end if statement for variable cost revenue figure

            } #else return()
  # Point and line plots 
         }  
      
    }# end Summary loop
    
    else { # Begin Variability analysis figure
      
      if(length(input$YearSelect) > 1){
        if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
          if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
            g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+  
                  geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf),fill="grey20", alpha=.04)+ 
                  geom_text(aes(x=.5,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, size=4.25) + 
                  geom_text(aes(x=length(table(as.numeric(YEAR)))+.75,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=1, size=4.25) #+
                #  geom_text(aes(x=2.5,y=(max(VALUE)/1000+max(VALUE)/4000),label="West Coast trawl catch shares program")) 
                 # geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white") 
            }  else {
            g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+ 
                  geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf),fill="grey20", alpha=.04)+ 
                  geom_text(aes(x=.25,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, size=4.25) + 
                  geom_text(aes(x=length(table(as.numeric(YEAR)))+.5,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=1, size=4.25) #+
                 # geom_text(aes(x=1.5,y=(max(VALUE)/1000+max(VALUE)/4000),label="West Coast trawl catch shares program")) 
                 # geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white") 
            }} else {
        g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
      }} else{
        g <- g + geom_point(aes_string(colour = groupVar), size=4)
      }
    } # end variability figure
    
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
    if(type == "summary") {
      g <- g + scale_fill_manual(values = pal.netrev, guide=guide_legend(reverse=T)) + 
        scale_colour_manual(values = pal.netrev, guide=guide_legend(reverse=T))
    } else {
      g <- g + scale_fill_manual(values = pal.thirds) + 
        scale_colour_manual(values = pal.thirds)
    }
    
    # defien x scale
#     g <- g +  scale_x_discrete(labels = c("2010" = "", "2011" = ""))
    
    # define solid line y=0
    g <- g + geom_hline(yintercept = 0)
    
    # define labels
    g <- g + labs(y = "Thousands ($)", title = main()) 
    
    # define theme
    g <- g + theme(
      plot.title = element_text(size=rel(1.75), vjust=1, colour="grey25"), 
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