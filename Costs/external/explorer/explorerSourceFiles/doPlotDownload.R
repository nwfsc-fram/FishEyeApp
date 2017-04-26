#This function is nearly identical to the doPlot function.  There have been some modifications to accomodate differences between optimizing for screen and optimizing for paper.

doPlotDownload <- function(dat, x, y){
  if(PermitPlot()){
  
    currentyear <- 2015
      
    yr <- function(){
      if(input$Sect_sel=="CV"){
        return(input$YearSelect)  
      } else {
        return(input$YearSelect2)
      }
    }
    
    groupVar <- "SHORTDESCR"
    facetVar <- "VARIABLE"

    ## Change color palette to printer-friendly colors that are color-blind friendly. Want consistent colors with what Erin is using
    colourList <- c('All variable costs'='#590014',
                    'Buyback fees'="#8f0007","Fish purchases"="#8f0007","Processing crew"="#8f0007",#'Freight'="#8f0007", #a50026
                    'Captain'="#d73027","Non-processing crew"="#d73027",'Freight'='#d73027',#''='#d73027',
                    'Cost recovery fees'='#FE8181','Labor'='#FE8181',
                    'Fuel'="#fdae61",'Off-site freezing & storage'="#fdae61",
                    
                    'Crew'="#ff6d33",'Monitoring'="#ff6d33",#
                    'Observers'="#cc7e00",#cc7e00",#fee090
                    #FE8181",#fcc212",#e3d165",
                    'Packing materials'="#e3d165",  
                    'Utilities'='#d8bc03',
                    'Other variable costs'="#fffb05",
                    
                    'All fixed costs'="#023858",
                    'Equipment'="#045a8d","On-board equipment"="#045a8d",
                    'Buildings'='#2605ff','Fishing gear'="#2605ff",
                    #'Repair and maintenance'='#3690c0',
                    "Processing equipment"="#3690c0",
                    'Other fixed costs'="#74a9cf"
    )
    
    thresh <- function(){
      if(input$PlotSelect != "Stacked bar"){
        return(max(dat$VALUE,na.rm=T)/1000+max(dat$VALUE,na.rm=T)/10000)
      } else {
        return(
          max(data.frame(
            dat %>% group_by(VARIABLE, YEAR, SHORTDESCR) %>% 
              summarise(VALUE=max(VALUE, na.rm=T)) %>% 
              group_by(VARIABLE,YEAR) %>% 
              summarise(VALUE=sum(VALUE, na.rm=T))
          )[,'VALUE'])/900
        )
      }
    }
    
    sect <- function(){
      if(input$Sect_sel == "CV"){
        return("Catcher Vessels")
      } else if(input$Sect_sel == "M"){
        return("Motherships")
      } else if(input$Sect_sel == "CP"){
        return("Catcher-Processors")
      } else if (input$Sect_sel == "FR"){
        return("First Receivers")
      }}
    
    # Plot title construction
    plot.title <- function(){
       return(paste("Summary Cost Measures for West Coast ", sect()))
     }
  
    gv <- function(){
     if(input$CategorySelect=="Fisheries"|input$CategorySelect=="Production activities"){
          if(input$Sect_sel=="CV"){
            sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect,"  Summed across:", input$FishWhitingSelect))
          } else {
            sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect,"  Summed across:", input$FishWhitingSelect))
          }} else if(input$CategorySelect=="Production activities"){
            sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect, "  Summed across:", input$inSelect,' and ', input$FishWhitingSelect))
          } else {
            if(input$Sect_sel=="CV"){
              sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect, "  Summed across:", input$inSelect,' and ', input$FishWhitingSelect))
            } else {
              sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect, "  Summed across:", input$inSelect,' and ', input$FishWhitingSelect))
            }}
    }
    
    
    main <- function(){
      bquote(atop(.(plot.title()), atop(.(gv()))))
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
    
    
    g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment=environment()) 
    
    
    if(input$PlotSelect=="Line"){
      g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
    } # end if statement for line figure
    else if(input$PlotSelect == "Bar"){
      g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="dodge", width = scale_bars())
    } #End if else for side-by-side comparion
    else {
      g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="stack", width = scale_bars())
    }

    if(length(yr())>1 & min(yr())<2011 & max(yr())>2010){
      g <- g + geom_rect(aes_string(xmin=-Inf, xmax=table(yr()<2011)[[2]]+.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))
      g <- g + geom_text(aes(x=(table(yr()<2011)[[2]])/4,y=thresh(), label="Pre-Catch shares"), family="serif",fontface="italic", hjust=0,color = "grey40", size=4/scale_text()) 
      g <- g + geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1.5,y=thresh(),label="Post-Catch shares"),hjust=0, 
                         family = "serif", fontface="italic", color = "grey40", size=4/scale_text())  
    } else {
      g <- g  
    } # end 
    
     # define facet
      g <- g + facet_wrap(~ sort, as.table = TRUE)
    

    # define scale
      g <- g + scale_fill_manual(values = colourList, guide=guide_legend(reverse=F)) + 
          scale_colour_manual(values = colourList, guide=guide_legend(reverse=F))
        
     
        # define solid line y=0
    g <- g + geom_hline(yintercept = 0)
    
    
    ylab <- function(){
        paste("Thousands of", currentyear, "$", "(",input$StatSelect, ")")
        }
    
    source_lab <- function(){
      paste("Sourced from the FISHEyE application (https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/Costs/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"))
    }
    conf_mess <- function(){
      if(input$Sect_sel=="CV"){
        "\nNOTE: Your selection would reveal confidential data for years with sufficient observations.  The results shown may include both vessels that fished in Alaska and those that \nfished for Pacific whiting. See the confidentiality section under the ABOUT tab for more information."
      } else {
        ""
      }
    }
    
    # define labels
    xlab <- function(){
            if(max(dat$AK_FLAG, na.rm=T)==1){
              paste(conf_mess(),source_lab())
            } else {
              source_lab()      
            }
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
                                size = 9, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=rel(.7), face="italic", vjust=0, colour="grey25"),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 10),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
                                 color = "grey25", face = "bold", size = 7),
      legend.key.size=unit(1,'line'),
      legend.title = element_blank())
    
    if(input$PlotSelect!="Line"){
      g <- g +guides(fill=guide_legend(nrow=2, byrow=TRUE))
    } else {
      g <- g +guides(colour=guide_legend(nrow=2, byrow=TRUE))
    }
    ######################################################################################
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
