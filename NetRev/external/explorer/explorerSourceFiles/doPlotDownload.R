#This function is nearly identical to the doPlot function.  There have been some modifications to accomodate differences between optimizing for screen and optimizing for paper.

doPlotDownload <- function(dat, x, y, type){
  if(PermitPlot()){
    
    groupVar <- ifelse(type=="summary", "SHORTDESCR", "THIRDS")
    facetVar <- ifelse(type== "summary" , "VARIABLE", "SHORTDESCR")

    ## Change color palette to printer-friendly colors that are color-blind friendly. Want consistent colors with what Erin is using
    colourThirds <- c('Top third'="#253494",'Middle third'="#41b6c4",'Lower third'="#a1dab4")
    colourList <- c('Revenue'="#256D36",'Variable costs'="#fed25d",'Fixed costs'="#fca836",'Variable Cost Net Revenue'="#4B958D", 'Total Cost Net Revenue'="#4575B4")
    
    sect <- function(){
      if(input$Sect_sel == "CV"){
        return("Catcher Vessels")
      } else if(input$Sect_sel == "M"){
        return("Motherships")
      } else if(input$Sect_sel == "CP"){
        return("Catcher Processors")
      } else if (input$Sect_sel == "FR"){
        return("First Receivers")
      }}
    
    # Plot title construction
    plot.title <- function(){
      if(type == "summary"){
      if(input$DodgeSelect == "Economic measures side-by-side"){
        return(paste("Summary Economic Measures for West Coast ", sect()))
      } else if(input$DodgeSelect == "Composition of Total Cost Net Revenue"){
        return(paste("Composition of Total Cost Net Revenue for West Coast ", sect()))
      } else if(input$DodgeSelect == "Composition of Variable Cost Net Revenue"){
        return(paste("Composition of Variable Cost Net Revenue for West Coast ", sect()))
      }#}
    } else {
      return(paste("Variability Analysis of West Coast Catcher Vessels", sect()))
    }}
  
    gv <- function(){
      if(type == "summary"){
        if(input$CategorySelect=="Fisheries"){
          sprintf(paste("Group variable:", input$CategorySelect, " Statistic: ", input$StatSelect, " Fished in AK included:", input$FishAkSelect, " Fished for whiting included:", input$FishWhitingSelect))
        } else {
          sprintf(paste("Group variable:", input$CategorySelect, "Statistic: ", input$StatSelect, "Fished in AK included:", input$FishAkSelect, "Fished for whiting included:", input$FishWhitingSelect,"Summed across:", input$inSelect))
        }
      } else {
        if(input$CategorySelect=="Fisheries"){
          sprintf(paste(input$CategorySelect, ":", input$VariableSelect, " Statistic: ", input$StatSelect, "Fished in AK included:", input$FishAkSelect, " Fished for whiting included:", input$FishWhitingSelect))
        } else {
          sprintf(paste(input$CategorySelect, ":", input$VariableSelect, " Statistic: ", input$StatSelect, " Fished in AK included:", input$FishAkSelect, " Fished for whiting included:", input$FishWhitingSelect," Summed across:", input$inSelect))   
        }
      }
    }
    
    sv <- function(){
      if(type == "summary"){
        if(input$DodgeSelect == "Composition of Total Cost Net Revenue"){
          return("Total Cost Net Revenue = Revenue - Variable costs - Fixed costs")
        } else if(input$DodgeSelect == "Composition of Variable Cost Net Revenue"){
          return("Variable Cost Net Revenue = Revenue - Variable costs")
        } else {
          return()
        }
      } else {
        return()
      }
    }
    
    main <- function(){
      bquote(atop(.(plot.title()), atop(.(gv()), .(sv()))))
     }
    
    
    
    # simple scaling for bar charts based on number of inputs
    scale_bars <- function(){
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
        if(b == 10 | b ==9) {
          return(1.1)
        } else if(b == 8){
          return(1.3)
        } else if(b >= 5 & b < 8){
          return(1.5)
        } else if(b<5 | b==12){
          return(1.9)
        }   else {
          if(b==11) {
            return(1.1)
          }
        }
      } else {
        return(1.4)
      }
    }   
    
    scale_text2 <- function() {
      
      b <- table(table(dat$SHORTDESCR)>1)[[1]]
      if(b == 2 | b ==5) {
        return(1.7)
      } else {
        return(1.3)
      } 
    }   
    
    
    g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment=environment()) 
    
    if(type == "summary"){
      if(input$DodgeSelect == "Economic measures side-by-side"){
        if(input$PlotSelect!="Bar"){
          if(input$PlotSelect == "Point"){
            
            g <- g + geom_point(aes_string(colour = groupVar), size=4)   
          } else {
            g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
          }} # end if statement for line figure
        
        if(input$PlotSelect == "Bar"){
          g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="dodge", width = scale_bars())
        } #End if else for side-by-side comparion

        if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
          if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
            if(length(input$YearSelect[input$YearSelect>2010])==1){
              g <- g + geom_rect(aes_string(xmin=.1, xmax=2.35, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                geom_text(aes(x=0.4,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000, label="Pre-Catch shares", family="sans"),hjust=0, color="grey20", size=4.5/scale_text()) + 
                geom_text(aes(x=length(table(input$YearSelect[input$YearSelect<2011]))+length(table(input$YearSelect[input$YearSelect>2010]))/2,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000,label="Post-Catch shares", family="sans"),hjust=1, size=4.5/scale_text(), color="grey20")# +
              
            } else {
              g <- g + geom_rect(aes_string(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                geom_text(aes(x=0.3,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000, label="Pre-Catch shares", family="sans"),hjust=0, color="grey20", size=4.5/scale_text()) + 
                geom_text(aes(x=2.75,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000,label="Post-Catch shares", family="sans"),hjust=0, size=4.5/scale_text(), color="grey20")# +
              
            }} else  {
              if(length(input$YearSelect[input$YearSelect>2010])==1){
                g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=.25,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000, label="Pre-Catch shares", family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                  geom_text(aes(x=length(table(input$YearSelect[input$YearSelect<2011]))+length(table(input$YearSelect[input$YearSelect>2010]))/2,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000,label="Post-Catch shares", family="sans"),hjust=1, size=4.5/scale_text(), color="grey20") #+
              } else {
                g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=.15,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000, label="Pre-Catch shares", family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                  geom_text(aes(x=1.9,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000,label="Post-Catch shares", family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") #+
              }
            }} # end if-else for adding dashed lines or not (pre- and post- catch shares)
        else {
          g <- g
        }
      }#End standard plots
    
      if(input$DodgeSelect != "Economic measures side-by-side"){          
        dat <- dat[order(dat$SHORTDESCR),]
        g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar, fill=groupVar, order=groupVar)) + 
          geom_bar(aes_string(fill = groupVar), stat = "identity", position = "stack", width = scale_bars())
        #    }  
        
        if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
          if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
            if(length(input$YearSelect[input$YearSelect>2010])==1){
              g <- g + geom_rect(aes(xmin=.1, xmax=2.35, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                geom_text(aes(x=.4,y=max(VALUE,na.rm=T)/500, label="Pre-Catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                geom_text(aes(x=length(table(as.numeric(YEAR)))+.6,y=max(VALUE,na.rm=T)/500,label="Post-Catch shares",family="sans"),hjust=1, size=4.5/scale_text(), color="grey20")# +
            } else {
              g <- g + geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                geom_text(aes(x=.3,y=max(VALUE,na.rm=T)/500, label="Pre-Catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                geom_text(aes(x=2.75,y=max(VALUE,na.rm=T)/500,label="Post-Catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20")# +
            }
          } else  {
            if(length(input$YearSelect[input$YearSelect>2010])==1){  
              g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                geom_text(aes(x=.25,y=max(VALUE,na.rm=T)/500, label="Pre-Catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                geom_text(aes(x=length(table(as.numeric(YEAR)))+.6,y=max(VALUE,na.rm=T)/500,label="Post-Catch shares",family="sans"),hjust=1, size=4.5/scale_text(), color="grey20") #+
            } else {
              g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                geom_text(aes(x=.15,y=max(VALUE,na.rm=T)/500, label="Pre-Catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                geom_text(aes(x=1.9,y=max(VALUE,na.rm=T)/500,label="Post-Catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") #+
            }
          }}   else {
            g <- g 
          }
      }
      g <- g + geom_text(aes(label=star), colour="black", vjust=0, size=10)
      } # end if statement for variable cost revenue figure
    
    
    # Begin Variability analysis figure
    if(type!="summary"){
      if(length(input$YearSelect) > 1){
        if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
          if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
            if(length(input$YearSelect[input$YearSelect>2010])==1){
              g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+  
                geom_rect(aes(xmin=.2, xmax=2.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
                geom_text(aes(x=.5,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000, label="Pre-Catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") + 
                geom_text(aes(x=length(table(as.numeric(YEAR)))+.7,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000,label="Post-Catch shares"),hjust=1, size=4.5/scale_text2(), color="grey20",family="sans") #+
            } else {
              g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+  
                geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
                geom_text(aes(x=.25,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000, label="Pre-Catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") + 
                geom_text(aes(x=length(table(as.numeric(YEAR)))-1.5,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000,label="Post-Catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") #+
            }  }  else {
              if(length(input$YearSelect[input$YearSelect>2010])==1){              
                g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+ 
                  geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=.25,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000, label="Pre-Catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") + 
                  geom_text(aes(x=2.5,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000,label="Post-Catch shares"),hjust=1, size=4.5/scale_text2(), color="grey20",family="sans") #+
              } else {
                g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+ 
                  geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=.25,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000, label="Pre-Catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") + 
                  geom_text(aes(x=2.2,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000,label="Post-Catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") #+
              }
            }} else {
              g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
            }} else{
              g <- g + geom_point(aes_string(colour = groupVar), size=4)
            }
    } # end variability figure   
    
    # define facet
    if(type =="summary"){
      g <- g + facet_wrap(~ sort, as.table = TRUE)
    } else {
      g <- g + facet_wrap(~ sort)
    }
    

    # define scale
    if(type == "summary") {
      if(input$DodgeSelect == "Economic measures side-by-side"){
        g <- g + scale_fill_manual(values = colourList, guide=guide_legend(reverse=F)) + 
          scale_colour_manual(values = colourList, guide=guide_legend(reverse=F))}
      else {
        g <- g + scale_fill_manual(values = colourList, guide=guide_legend(reverse=T)) + 
          scale_colour_manual(values = colourList)}
      
    } else {
      g <- g + scale_fill_manual(values = colourThirds) + 
        scale_colour_manual(values = colourThirds)
    }
    
        # define solid line y=0
    g <- g + geom_hline(yintercept = 0)
    
    
    
    # define labels
    if(type!="summary"){
      if(max(dat$con_flag, na.rm=T)==1){
        if(max(dat$AK_FLAG, na.rm=T)==0){
          g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                                                                                             "\n  Vessels are grouped into three tiered categories: top, middle, and lower earners based on revenue. This is done for each year separately.
                      \nData has been suppressed for years that are not plotted as there are not enough observations to protect confidentiality."), title = main()) 
        } else {
          g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                          "\n Vessels are grouped into three tiered categories: top, middle, and lower earners based on revenue. This is done for each year separately.  
                          \nData has been suppressed for years that are not plotted as there are not enough observations to protect confidentiality.  
                          \nNOTE: In addition, your selection would reveal confidential data for years with sufficient observations.  The results shown may include both vessels that fished in Alaska and those that \nfished for Pacific whiting. See the confidentiality section under the ABOUT tab for more information."), title = main())        
        }} else {
          
          if(max(dat$AK_FLAG, na.rm=T)==0){
            g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),  
                          "\nVessels are grouped into three tiered categories: top, middle, and lower earners based on revenue. This is done for each year separately."), title = main()) 
          } else {
            g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                        "\n Vessels are grouped into three tiered categories: top, middle, and lower earners based on revenue. This is done for each year separately.  
                          \nNOTE: Unfortunately, your selection would reveal confidential data.  The results shown may include both vessels that fished in Alaska and those that \nfished for Pacific whiting. See the confidentiality section under the ABOUT tab for more information."), title = main())        
          }
        }
          } else {
            if(max(dat$flag)==1) {
              if(max(dat$AK_FLAG, na.rm=T)==0){
                g <- g + labs(y = paste("Thousands of 2014 $","(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                                                                                            "\n* Data has been suppressed for this selected",input$CategorySelect, "and year as there are not enough observations to protect confidentiality."), title = main())   
              }  else {
                g <- g + labs(y = paste("Thousands of 2014 $","(",input$StatSelect, ")"), x=
                                paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                          "\n* Data has been suppressed for this selected",input$CategorySelect, "and year as there are not enough observations to protect confidentiality.
                          \nNOTE: Unfortunately, your selection would reveal confidential data.  The results shown may include both vessels that fished in Alaska and those that \nfished for Pacific whiting. See the confidentiality section under the ABOUT tab for more information."), title = main())        
              }
            }
            else if(max(dat$AK_FLAG, na.rm=T)==1){
              g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                                                                                           "\nNOTE: Unfortunately, your selection would reveal confidential data.  The results shown may include both vessels that fished in Alaska and those that \nfished for Pacific whiting. See the confidentiality section under the ABOUT tab for more information."), title = main())       
            } else {
              g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y")), title = main())       
              
            }}   
            
     
    
    # define theme
    g <- g + theme(
      plot.title = element_text(size=rel(1), vjust=1, colour="grey25"), 
      plot.title = element_text(family = "sans", face = "bold", vjust = 1),
      plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      panel.background = element_rect(fill = "white"),
      panel.margin = unit(1.2, "lines"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", 
                                size = 11, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=rel(.7), face="italic", vjust=0, colour="grey25"),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 9.5),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
                                 color = "grey25", face = "bold", size = 12),
      legend.title = element_blank())
    
    ##function to wrapping facet labels
    strwrap_strip_text = function(p, pad=0.05) { 
      # get facet font attributes
      th = theme_get()
      if (length(p$theme) > 0L)
        th = th + p$theme
      
      require("grid")
      grobs <- ggplotGrob(p)
      
      # wrap strip x text
      ps = calc_element("strip.text.x", th)[["size"]]
      family = calc_element("strip.text.x", th)[["family"]]
      face = calc_element("strip.text.x", th)[["face"]]
      
      nm = names(p$facet$facets)
      
      # get number of facet columns
      levs = levels(factor(p$data[[nm]]))
      npanels = length(levs)
      cols = n2mfrow(npanels)[1]
      
      # get plot width
      sum = .5#sum(sapply(grobs$width, function(x) convertWidth(x, "in")))
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
      p$data[[nm]] = gsub("([.])", "\\ ", p$data[[nm]]) 
      invisible(p)
    }   
    #    print(g)
    g <- strwrap_strip_text(g) #use instead of print(g)
    print(g)
    
    } else plot(0,0,type="n", axes=F, xlab="", ylab="")
  }
