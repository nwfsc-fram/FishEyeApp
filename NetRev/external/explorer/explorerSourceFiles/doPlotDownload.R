#This function is nearly identical to the doPlot function.  There have been some modifications to accomodate differences between optimizing for screen and optimizing for paper.

doPlotDownload <- function(dat, x, y, type){
  if(PermitPlot()){
    
    yr <- function(){
      return(unique(dat$YEAR))
    }
    
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
          if(input$Sect_sel=="CV"){
            sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect, "    Fished in AK included:", input$FishAkSelect, "    Fished for whiting included:", input$FishWhitingSelect))
          } else {
            sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect))
          }} else if(input$CategorySelect=="Production activities"){
            sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect))
          } else {
            if(input$Sect_sel=="CV"){
              sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect, "    Fished in AK included:", input$FishAkSelect,"    Fished for whiting included:", input$FishWhitingSelect,"    Summed across:", input$inSelect))
            } else {
              sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect,   "Summed across:", input$inSelect))
            }}
      } else {
        if(input$CategorySelect=="Fisheries"){
          if(input$Sect_sel=="CV"){
            sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect, "    Fished in AK included:", input$FishAkSelect,"    Fished for whiting included:", input$FishWhitingSelect))
          } else{
            sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect))
          }} else if(input$CategorySelect=="Production activities"){
            sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect))
          } else {
            if(input$Sect_sel=="CV"){
              sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect, "    Fished in AK included:", input$FishAkSelect,"    Fished for whiting included:", input$FishWhitingSelect,"    Summed across:", input$inSelect))   
            } else {
              sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect,  "Summed across:", input$inSelect))   
            }}
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
        if(b<=8 | b==12){
          return(1.2)
        }   else {
            return(1.1)
        }
      } else {
        return(1.2)
      }
    }   
    
    scale_text2 <- function() {
      
      b <- table(table(dat$SHORTDESCR)>1)[[1]]
      if(b == 2 | b ==5) {
        return(1.6)
      } else {
        return(1.2)
      } 
    }   
    
    scale_geom_text <- function() {
      if(sum(dat$VALUE, na.rm=T)!=0) {
        return(max(dat$VALUE, na.rm=T))
      } else {
        return(0)
      }
    }
    g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment=environment()) 
    
    if(type == "summary"){
      if(input$DodgeSelect == "Economic measures side-by-side"){
        if(input$PlotSelect!="Bar"){
          if(input$PlotSelect == "Point"){
            
            g <- g + geom_point(aes_string(colour = groupVar), size=2)   
          } else {
            g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
          }} # end if statement for line figure
        
        if(input$PlotSelect == "Bar"){
          g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="dodge", width = scale_bars())
        } #End if else for side-by-side comparion

        if(length(yr())>1 & min(yr())<2011 & max(yr())>2010){
          if(yr()[1]==2009&yr()[2]==2010){
            if(length(yr())==3){
              g <- g + geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                geom_text(aes(x=0.4,y=scale_geom_text()/1000+scale_geom_text()/10000, label="Pre-catch shares"), family="serif",fontface="italic",hjust=0, color="grey40", size=4/scale_text()) + 
                geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1,y=scale_geom_text()/1000+scale_geom_text()/10000,label="Post-catch shares"), family="serif",fontface="italic",hjust=1, size=4/scale_text(), color="grey40")# +
              
            } else {
              g <- g + geom_rect(aes_string(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                geom_text(aes(x=0.3,y=scale_geom_text()/1000+scale_geom_text()/10000, label="Pre-catch shares"), family="serif",fontface="italic",hjust=0, color="grey40", size=4/scale_text()) + 
                geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1.75,y=scale_geom_text()/1000+scale_geom_text()/10000,label="Post-catch shares"),family="serif",fontface="italic",hjust=0, size=4/scale_text(), color="grey40")# +
              
            }} else  {
              if(table(yr()>2010)[[2]]==1){
                g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                  geom_text(aes(x=.25,y=scale_geom_text()/1000+scale_geom_text()/10000, label="Pre-catch shares"), family="serif",fontface="italic",hjust=0, size=4/scale_text(), color="grey40") + 
                  geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/.75,y=scale_geom_text()/1000+scale_geom_text()/10000,label="Post-catch shares"), family="serif",fontface="italic",hjust=1, size=4/scale_text(), color="grey40") #+
              } else {
                g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                  geom_text(aes(x=.15,y=scale_geom_text()/1000+scale_geom_text()/10000, label="Pre-catch shares"), family="serif",fontface="italic",hjust=0, size=4/scale_text(), color="grey40") + 
                  geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1.75,y=scale_geom_text()/1000+scale_geom_text()/10000,label="Post-catch shares"), family="serif",fontface="italic",hjust=0, size=4/scale_text(), color="grey40") #+
              }
            }} # end if-else for adding dashed lines or not (pre- and post- catch shares)
        else {
          g <- g
        }
      }#End standard plots
    
      if(input$DodgeSelect != "Economic measures side-by-side"){          
        #dat <- dat[order(dat$SHORTDESCR),]
       #g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar, fill=groupVar, order=groupVar)) + 
        #  geom_bar(aes_string(fill = groupVar), stat = "identity", position = "stack", width = scale_bars())
        #    }  
        g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="stack", width = scale_bars())
        
        if(length(yr())>1 & min(yr())<2011 & max(yr())>2010){
          if(yr()[1]==2009&yr()[2]==2010){
            if(table(yr()>2010)[[2]]==1){
              g <- g + geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                geom_text(aes(x=.4,y=scale_geom_text()/500, label="Pre-catch shares"),family="serif",fontface="italic",hjust=0, size=4/scale_text(), color="grey40") + 
                geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1,y=scale_geom_text()/500,label="Post-catch shares"),family="serif",fontface="italic",hjust=1, size=4/scale_text(), color="grey40")# +
            } else {
              g <- g + geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                geom_text(aes(x=.3,y=scale_geom_text()/500, label="Pre-catch shares"),family="serif",fontface="italic", hjust=0, size=4/scale_text(), color="grey40") + 
                geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1.5,y=scale_geom_text()/500,label="Post-catch shares"),family="serif",fontface="italic",hjust=0, size=4/scale_text(), color="grey40")# +
            }
          } else  {
            if(table(yr()>2010)[[2]]==1){  
              g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                geom_text(aes(x=.25,y=scale_geom_text()/500, label="Pre-catch shares"),family="serif",fontface="italic",hjust=0, size=4/scale_text(), color="grey40") + 
                geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1,y=scale_geom_text()/500,label="Post-catch shares"),family="serif",fontface="italic",hjust=1, size=4/scale_text(), color="grey40") #+
            } else {
              g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                geom_text(aes(x=.15,y=scale_geom_text()/500, label="Pre-catch shares"),family="serif",fontface="italic",hjust=0, size=4/scale_text(), color="grey40") + 
                geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1.5,y=scale_geom_text()/500,label="Post-catch shares"),family="serif",fontface="italic",hjust=0, size=4/scale_text(), color="grey40") #+
            }
          }}   else {
            g <- g 
          }
      }
      g <- g + geom_text(aes(label=star), colour="black", vjust=0, size=10)
      } # end if statement for variable cost revenue figure
    
    
    # Begin Variability analysis figure
    if(type!="summary"){
      if(length(yr()) > 1){
        if(length(yr())>1 & min(yr())<2011 & max(yr())>2010){
          if(yr()[1]==2009&yr()[2]==2010){
            if(table(yr()>2010)[[2]]==1){
              g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+  
                geom_rect(aes(xmin=.2, xmax=2.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(yr()))+ 
                geom_text(aes(x=.5,y=scale_geom_text()/1000+scale_geom_text()/10000, label="Pre-catch shares"),hjust=0, size=4/scale_text2(), color="grey40",family="serif", fontface="italic") + 
                geom_text(aes(x=length(yr())+.7,y=scale_geom_text()/1000+scale_geom_text()/10000,label="Post-catch shares"),hjust=1, size=4/scale_text2(), color="grey40",family="serif", fontface="italic") #+
            } else {
              g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+  
                geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(yr()))+ 
                geom_text(aes(x=.25,y=scale_geom_text()/1000+scale_geom_text()/10000, label="Pre-catch shares"),hjust=0, size=4/scale_text2(), color="grey40",family="serif", fontface="italic") + 
                geom_text(aes(x=length(yr())-1.5,y=scale_geom_text()/1000+scale_geom_text()/10000,label="Post-catch shares"),hjust=0, size=4/scale_text2(), color="grey40",family="serif", fontface="italic") #+
            }  }  else {
              if(table(yr()>2010)[[2]]==1){              
                g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+ 
                  geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(yr()))+ 
                  geom_text(aes(x=.25,y=scale_geom_text()/1000+scale_geom_text()/10000, label="Pre-catch shares"),hjust=0, size=4/scale_text2(), color="grey40",family="serif", fontface="italic") + 
                  geom_text(aes(x=length(yr())+.2,y=scale_geom_text()/1000+scale_geom_text()/10000,label="Post-catch shares"),hjust=1, size=4/scale_text2(), color="grey40",family="serif", fontface="italic") #+
              } else {
                g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+ 
                  geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(yr()))+ 
                  geom_text(aes(x=.25,y=scale_geom_text()/1000+scale_geom_text()/10000, label="Pre-catch shares"),hjust=0, size=4/scale_text2(), color="grey40",family="serif", fontface="italic") + 
                  geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1.5,y=scale_geom_text()/1000+scale_geom_text()/10000,label="Post-catch shares"),hjust=0, size=4/scale_text2(), color="grey40",family="serif", fontface="italic") #+
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
    
    
    ylab <- function(){
        paste("Thousands of 2015 $", "(",input$StatSelect, ")")
    }
    
    source_lab <- function(){
      paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"))
    }
    supp_obs <- function(){
      "\nData has been suppressed for years that are not plotted as there are not enough observations to protect confidentiality."
    }
    conf_mess <- function(){
      if(input$Sect_sel=="CV"){
        "\nNOTE: Your selection would reveal confidential data for years with sufficient observations.  The results shown may include both vessels that fished in Alaska and those that \nfished for Pacific whiting. See the confidentiality section under the ABOUT tab for more information."
      } else if(input$Sect_sel=="FR"){
        "\nNOTE: Your selection would reveal confidential data for years with sufficient observations. When this happens, we show results for catch share processors. See the confidentiality section under the ABOUT tab for more information."
      } else {
        ""
      }
    }
    thirds_mess <- function(){
      "\n  Vessels are grouped into three tiered categories: top, middle, and lower earners based on revenue. This is done for each year separately."
    }
    suff_flag <- function(){
      paste("\n* Data has been suppressed for this selected",input$CategorySelect, "and year as there are not enough observations to protect confidentiality.")
    }
      # define labels
    xlab <- function(){
    if(type!="summary"){
      if(max(dat$con_flag, na.rm=T)==1){
        if(max(dat$AK_FLAG, na.rm=T)==0){
          paste(source_lab(),thirds_mess(),supp_obs())
        } else {
          paste(source_lab(), thirds_mess(),supp_obs(), conf_mess())
        }} else {
          if(max(dat$AK_FLAG, na.rm=T)==0){
            paste(source_lab(),thirds_mess()) 
          } else {
            paste(source_lab(),thirds_mess(), conf_mess())
          }
        }
          }
      
      else {
            if(max(dat$flag)==1) {
              if(max(dat$AK_FLAG, na.rm=T)==0){
                paste(source_lab(),suff_flag())
              }  else {
                paste(source_lab(),suff_flag(),conf_mess())
              }
            }
            else if(max(dat$AK_FLAG, na.rm=T)==1){
              paste(source_lab(),conf_mess())
            } else {
              source_lab()      
            }}
    }
            
    g <- g + labs(y=ylab(), x=xlab(), title=main())
    
    g$data[[names(g$facet$facets)]] = unlist(gsub("([.])", "\\ ", g$data[[names(g$facet$facets)]])) 
    
    
    # define theme
    g <- g + theme(
      plot.title = element_text(size=rel(1.2), vjust=1, colour="grey25"), 
      plot.title = element_text(family = "sans", face = "bold", vjust = 1),
      plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      panel.background = element_rect(fill = "white"),
      panel.margin = unit(1.1, "lines"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", 
                                size = 13, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=rel(.7), face="italic", vjust=0, colour="grey25"),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 11),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
                                 color = "grey25", face = "bold", size = 10),
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
#    g <- strwrap_strip_text(g) #use instead of print(g)
    print(g)
    
    } else plot(0,0,type="n", axes=F, xlab="", ylab="")
  }
