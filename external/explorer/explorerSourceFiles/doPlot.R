doPlot <- function(dat, x, y, type){
  if(PermitPlot()){
    
#     print("And I am the input to the plot!!!")
#     print(head(dat))
#     print(str(dat))
    
    groupVar <- ifelse(type=="summary", "SHORTDESCR", "THIRDS")
    facetVar <- ifelse(type== "summary" , "VARIABLE", "SHORTDESCR")

## Change color palette to printer-friendly colors that are color-blind friendly. Want consistent colors with what Erin is using
      #  colourList <- c("#d73027","#fee090","#91bfdb","#fc8d59", "#4575b4")
    colourList <- c('Revenue'="#d73027",'Variable costs'="#fee090", 'Total cost net revenue'="#4575b4",'Variable cost net revenue'="#fc8d59",'Fixed costs'="#91bfdb")
    colourThirds <- c('Top third'="#253494",'Middle third'="#41b6c4",'Bottom third'="#a1dab4")
 
           # Plot title construction
    main <- function(){
         
      if(type == "summary"){
      #  if(input$PlotSelect!="Bar"){
      #    sprintf(paste("Summary Economic Measures for West Coast Catcher Vessels:", input$CategorySelect,
      #                  "\nStatistic: ", input$StatSelect))  
    #    } else {
        if(input$DodgeSelect == "Compare economic measures side-by-side"){
        sprintf(paste("Summary Economic Measures for West Coast Catcher Vessels:", input$CategorySelect,
                "\nStatistic: ", input$StatSelect))
        } else if(input$DodgeSelect == "Derivation of total cost revenue"){
          sprintf(paste("Derivation of total cost net revenue for West Coast Catcher Vessels:", input$CategorySelect,
                  "\nStatistic: ", input$StatSelect))
        } else if(input$DodgeSelect == "Derivation of variable cost revenue"){
          sprintf(paste("Derivation of variable cost net revenue for West Coast Catcher Vessels:", input$CategorySelect,
                  "\nStatistic: ", input$StatSelect))
        }#}
        
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
    
 scale_text <- function() {
   if(input$CategorySelect =="Fisheries" | input$CategorySelect == "Homeport") {
    b <- table(table(dat$VARIABLE)>1)[[1]]
    if(b == 10 | b ==9 ) {
      return(1)
    } else if(b == 8){
      return(1.2)
    } else if(b >= 5 & b < 8){
      return(1.3)
    } else if(b<5 | b == 12){
      return(1.65)
    } 
      if(input$CategorySelect =="Fisheries") {
          if(b ==11) {
          return(1.65)
        }
    } else {
      if(input$CategorySelect == "Homeport"){
          if(b==11) {
          return(1)
        }}
      }
   } else {
          return(1.2)
    }
 }   

 scale_text2 <- function() {
   
   b <- table(table(dat$SHORTDESCR)>1)[[1]]
   if(b == 2 | b ==5) {
     return(1.3)
   } else {
     return(1.2)
   } 
 }   
 
 
    g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment=environment())

        if(type == "summary"){    
     if(input$DodgeSelect == "Compare economic measures side-by-side"){
       if(input$PlotSelect!="Bar"){
      if(input$PlotSelect == "Point"){
           
        g <- g + geom_point(aes_string(colour=groupVar), size=4) 
          } else {
        g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
          }} # end if statement for line figure

      if(input$PlotSelect == "Bar"){
        #if(!is.null(input$DodgeSelect)){
          
            g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="dodge", width = scale_bars())
            } #End if else for side-by-side comparion
              # }#}     
          
      if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
      if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
        if(length(input$YearSelect[input$YearSelect>2010])==1){
          g <- g + geom_rect(aes_string(xmin=.1, xmax=2.35, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
            geom_text(aes(x=0.4,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text()) + 
            geom_text(aes(x=2.35,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"), hjust=0, 
                    fontface=1, family = "sans", color = "grey20", size=7/scale_text())  
        
        } else { 
          g <- g + geom_rect(aes_string(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
            geom_text(aes(x=0.3,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text()) + 
            geom_text(aes(x=2.65,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=0, 
                     fontface=1, family = "sans", color = "grey20", size=7/scale_text())  
         # +
      }} else {
        if(length(input$YearSelect[input$YearSelect>2010])==1){
            g <- g +  geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+
              geom_text(aes(x=0.25,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"), hjust=0, family="sans",color = "grey20", size=7/scale_text()) + 
               geom_text(aes(x=length(table(YEAR))+.5,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=1,  
                   family="sans",color = "grey20", size=7/scale_text()) 
        } else {
          g <- g +  geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+
            geom_text(aes(x=0.15,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"), hjust=0, family="sans",color = "grey20", size=7/scale_text()) + 
            geom_text(aes(x=1.8,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"), hjust=0, 
                      family="sans",color = "grey20", size=7/scale_text()) 
        }
       
      }} else {
        g <- g  
      }
        }}

    if(type == "summary"){    
              if(input$DodgeSelect != "Compare economic measures side-by-side"){          
              g <- ggplot(dat, aes_string(x = x, y = y ,group = groupVar, fill=groupVar, order=groupVar))+ # 
                  geom_bar(stat = "identity", position = "stack", width = scale_bars())
          
            if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
              if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
                if(length(input$YearSelect[input$YearSelect>2010])==1){
                  g <- g+  geom_rect(aes(xmin=.1, xmax=2.35, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                    geom_text(aes(x=.4,y=max(VALUE)/400, label="Pre-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text()) + 
                    geom_text(aes(x=2.35,y=max(VALUE)/400,label="Post-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text())# +
                  } else { 
                  g <- g+  geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                    geom_text(aes(x=.3,y=max(VALUE)/400, label="Pre-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text()) + 
                    geom_text(aes(x=2.65,y=max(VALUE)/400,label="Post-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text())# +
                  
                  
                } } else  {
                   if(length(input$YearSelect[input$YearSelect>2010])==1){
                  g <- g+ geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                    geom_text(aes(x=.25,y=max(VALUE)/400, label="Pre-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text()) + 
                    geom_text(aes(x=length(table(as.numeric(YEAR)))+.5,y=max(VALUE)/400,label="Post-catch shares"),color = "grey20", hjust=1, family="sans", size=7/scale_text())# +
                } else {
                  g <- g+ geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                    geom_text(aes(x=.15,y=max(VALUE)/400, label="Pre-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text()) + 
                    geom_text(aes(x=1.8,y=max(VALUE)/400,label="Post-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text())# +
                 }
              }} else {
              g <- g
            } #end if statement for text and greyed block
     }}#}}# end Summary loop for total and variable revenue figures
    
    if(type != "summary"){    # Begin Variability analysis figure
        if(length(input$YearSelect) > 1){
        if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
          if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
            if(length(input$YearSelect[input$YearSelect>2010])==1){
            g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+  
                  geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=.5,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text2()) + 
                  geom_text(aes(x=2.35,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text2()) #+
           } else {
              g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+  
               geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
               geom_text(aes(x=.5,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text2()) + 
               geom_text(aes(x=2.55,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text2()) #+
             
           } }  else {
             if(length(input$YearSelect[input$YearSelect>2010])==1){
            g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+ 
                  geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=.25,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text2()) + 
                  geom_text(aes(x=length(table(YEAR))+.5,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=1, family="sans",color = "grey20", size=7/scale_text2()) #+
            } else {
                g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+ 
                  geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=.25,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text2()) + 
                  geom_text(aes(x=1.8,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text2()) #+
            }
              }} else {
        g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
      }} else{
        g <- g + geom_point(aes_string(colour =groupVar), size=4)
      }
    } # end variability figure
      
    # define facet
    if(type =="summary"){
      g <- g + facet_wrap(~ VARIABLE, as.table = TRUE)
    } else {
      g <- g + facet_wrap(~ SHORTDESCR)
    }

    # define scale
    if(type == "summary") {
      g <- g + scale_fill_manual(values = colourList, guide=guide_legend(reverse=T)) + 
        scale_colour_manual(values = colourList, guide=guide_legend(reverse=T))
    } else {
      g <- g + scale_fill_manual(values = colourThirds) + 
        scale_colour_manual(values = colourThirds)
    }
    
    # define x scale
#     g <- g +  scale_x_discrete(labels = c("2010" = "", "2011" = ""))
    
    # define solid line y=0
    g <- g + geom_hline(yintercept = 0)
    
    # define labels
    if(type!="summary"){
    g <- g + labs(y = "Thousands ($)", x="Vessels are sorted annually into top, middle, and bottom earners based on variable cost net revenue", title = main()) 
    } else {
     g <- g + labs(y = "Thousands ($)", x="", title = main())       
    }
    
    # define theme
    g <- g + theme(
      plot.title = element_text(size=rel(1.75), vjust=1, colour="grey25"), 
      plot.title = element_text(family = "sans", face = "bold", vjust = 1),
      panel.background = element_rect(fill = "white"),
    #  panel.margin = unit(1.2, "lines"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", 
                                size = 18, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=rel(1.2),  face="italic", vjust=0, colour="grey25"),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
        color = "grey25", face = "bold", size = 12),
      legend.title = element_blank()
    #  text = element_text(family="sans", color = "red", size=rel(1.3))
    )
 
 ##function to wrapping facet labels
    strwrap_strip_text = function(p, pad=0.05) { 
      # get facet font attributes
      th = theme_get()
      if (length(p$theme) > 0L)
        th = th + p$theme
      
      require("grid")
      grobs <- ggplotGrob(p)
      
      # wrap strip x text
      if ((class(p$facet)[1] == "grid" && !is.null(names(p$facet$cols))) ||
          class(p$facet)[1] == "wrap")
      {
        ps = calc_element("strip.text.x", th)[["size"]]
        family = calc_element("strip.text.x", th)[["family"]]
        face = calc_element("strip.text.x", th)[["face"]]
        
        if (class(p$facet)[1] == "wrap") {
          nm = names(p$facet$facets)
        } else {
          nm = names(p$facet$cols)
        }
        
        # get number of facet columns
        levs = levels(factor(p$data[[nm]]))
        npanels = length(levs)
        if (class(p$facet)[1] == "wrap") {
          cols = n2mfrow(npanels)[1]
        } else {
          cols = npanels
        }
        
        # get plot width
        sum = sum(sapply(grobs$width, function(x) convertWidth(x, "in")))
        panels_width = par("din")[1] - sum  # inches
        # determine strwrap width
        panel_width = panels_width / cols
        mx_ind = which.max(nchar(levs))
        char_width = strwidth(levs[mx_ind], units="inches", cex=ps / par("ps"), 
                              family=family, font=gpar(fontface=face)$font) / 
          nchar(levs[mx_ind])
        width = floor((panel_width - pad)/ char_width)  # characters
        
        # wrap facet text
        p$data[[nm]] = unlist(lapply(strwrap(p$data[[nm]], width=width, 
                                             simplify=FALSE), paste, collapse="\n"))
      }
      
      if (class(p$facet)[1] == "grid" && !is.null(names(p$facet$rows))) {  
        ps = calc_element("strip.text.y", th)[["size"]]
        family = calc_element("strip.text.y", th)[["family"]]
        face = calc_element("strip.text.y", th)[["face"]]
        
        nm = names(p$facet$rows)
        
        # get number of facet columns
        levs = levels(factor(p$data[[nm]]))
        rows = length(levs)
        
        # get plot height
        sum = sum(sapply(grobs$height, function(x) convertWidth(x, "in")))
        panels_height = par("din")[2] - sum  # inches
        # determine strwrap width
        panels_height = panels_height / rows
        mx_ind = which.max(nchar(levs))
        char_height = strwidth(levs[mx_ind], units="inches", cex=ps / par("ps"), 
                               family=family, font=gpar(fontface=face)$font) / 
          nchar(levs[mx_ind])
        width = floor((panels_height - pad)/ char_height)  # characters
        
        # wrap facet text
        p$data[[nm]] = unlist(lapply(strwrap(p$data[[nm]], width=width, 
                                             simplify=FALSE), paste, collapse="\n"))
      }
      
      invisible(p)
    }   
    #print(g)
    g <- strwrap_strip_text(g) #use instead of print(g)
    print(g)
      
   } else plot(0,0,type="n", axes=F, xlab="", ylab="")
}