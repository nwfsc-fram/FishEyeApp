#https://github.com/hadley/ggplot2/issues/1301  #use website for dealing with stacked bar plot order issue
doPlot <- function(dat, x, y){
  if(PermitPlot()){
    dat <- subset(dat, is.na(dat$VALUE)==FALSE)
 
    dat$sort2 <- reorder(dat$VARIABLE, dat$sort) 
   
    rectvars <- dat %>% distinct(sort2,YEAR) %>% 
      group_by(sort2) %>% 
      transmute(minx=min(as.numeric(YEAR)), xmaxscale=length(YEAR[YEAR<2011]), maxx=max(YEAR)) %>% 
      data.frame()%>% distinct %>%
      merge(dat %>% distinct(sort2, SHORTDESCR))
    
     yr <- function(){
       as.numeric(unique(dat$YEAR))
     }
     
    groupVar <- "SHORTDESCR"
    facetVar <- "VARIABLE"
    

    ## Change color palette to printer-friendly colors that are color-blind friendly. Want consistent colors with what Erin is using
    colourList <- c('All variable costs'='#590014',
                    'Buyback fees'="#8f0007","Fish purchases"="#8f0007","Processing crew"="#8f0007",
                    'Captain'="#d73027","Non-processing crew"="#d73027",'Freight'='#d73027',
                    'Cost recovery fees'='#FE8181','Labor'='#FE8181',
                    'Fuel'="#fdae61",'Off-site freezing & storage'="#fdae61",
                    
                    'Crew'="#ff6d33",'Monitoring'="#ff6d33",#
                    'Observers'="#cc7e00",#cc7e00",#fee090
                    #FE8181",#fcc212",#e3d165",
                    'Packing materials'="#fcc212",  
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
        if(input$StatSelect=='Mean per vessel'|
           input$StatSelect=='Median per vessel'|
           input$StatSelect=='Fleet-wide total'|
           input$StatSelect=="Mean per processor"|
           input$StatSelect=='Median per processor'|
           input$StatSelect=='Industry-wide total'|
           input$StatSelect=='Mean per vessel/day'|
           input$StatSelect=='Median per vessel/day'|input$StatSelect=='Fleet-wide average/day'){
          return(max(dat$VALUE,na.rm=T)/1000+max(dat$VALUE,na.rm=T)/10000)
        } else {
          return(max(dat$VALUE,na.rm=T)+max(dat$VALUE,na.rm=T)/10) 
        }
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
    
    
     print(thresh())
      sect <- function(){
      if(input$Sect_sel == "CV"){
        return("Catcher Vessels")
      } else if(input$Sect_sel == "M"){
        return("Mothership Vessels")
      } else if(input$Sect_sel == "CP"){
        return("Catcher-Processor Vessels")
      } else if (input$Sect_sel == "FR"){
        return("First Receivers and Shorebased Processors")
      }}
    

  

 # Plot title construction
     plot.title <- function(){
          return(paste("Summary Cost Measures for West Coast ", sect()))
}
    
    gv <- function(){
          if(input$CategorySelect=="Fisheries"|input$CategorySelect=="Production activities"){
              if(input$Sect_sel=="CV"){
                    sprintf(paste("Group variable:", input$CategorySelect,"  Summed across:", input$FishWhitingSelect))
              } else {
                    sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", 
                                  input$StatSelect, "  Summed across:", input$FishWhitingSelect))
           }} else if(input$CategorySelect=="Production activities"){
                    sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", 
                                  input$StatSelect, "  Summed across:", input$inSelect,' and ', input$FishWhitingSelect))
            } else {
              if(input$Sect_sel=="CV"){
                    sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", 
                                  input$StatSelect, "  Summed across:", input$inSelect, ' and ', 
                                  input$FishWhitingSelect))
              } else {
                    sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", 
                                  input$StatSelect, "  Summed across:", input$inSelect, ' and ', input$FishWhitingSelect))
              }}
      }

      
    main <- function(){
      bquote(atop(.(plot.title()), atop(.(gv()))))  #bquote splits subtitle into two lines
    }
    
    
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
    
    # Beging ggplot    
    g <- ggplot(dat[!is.na(dat$VALUE),], aes_string(x = x, y = y , group = groupVar), environment=environment()) 
    
    
    if(input$PlotSelect=="Line"){
      g <- g + geom_line(aes_string(colour = groupVar), size=1.5) +
        geom_point(aes_string(colour = groupVar), size = 4)
    } # end if statement for line figure
    else if(input$PlotSelect == "Bar"){
      g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="dodge", width = scale_bars())
    } #End if else for side-by-side comparion
    else {
      g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="stack", width = scale_bars())
    }



    # define facet
      g <- g + facet_wrap(~sort2, ncol=2, as.table = TRUE)#
    
    
    # define colors and order
        g <- g + scale_fill_manual(values = colourList, guide=guide_legend(reverse=F)) + 
                 scale_colour_manual(values = colourList, guide=guide_legend(reverse=F))
      
    
    # define solid line y=0
        g <- g + geom_hline(yintercept = 0)
        
    # define x- and y-axis labels
    ylab <- function(){
      if(input$StatSelect=='Mean per vessel'|input$StatSelect=='Median per vessel'|
         input$StatSelect=='Fleet-wide total'|
         input$StatSelect=="Mean per processor"|input$StatSelect=='Median per processor'|
         input$StatSelect=='Industry-wide total'|
         input$StatSelect=='Mean per vessel/day'|input$StatSelect=='Median per vessel/day'|
         input$StatSelect=='Fleet-wide average/day'){
        paste("Thousands of", currentyear, " $ (",input$StatSelect, ")")
      } else if(input$StatSelect=='Mean per vessel/dollar of revenue'|
                input$StatSelect=='Median per vessel/dollar of revenue'|
                input$StatSelect=='Fleet-wide average/dollar of revenue'|
                input$StatSelect=="Mean per processor/dollar of revenue"|
                input$StatSelect=='Median per processor/dollar of revenue'|
                input$StatSelect=='Industry-wide average/dollar of revenue'){
        input$StatSelect
      } else {
        paste(currentyear, " $ (",input$StatSelect, ")")
      }
    }
    
    stacked_bar_mess <- function(){
"\nFor the stacked bar plot, we show either the individual cost categories or the total cost categories (All variable or All fixed costs). \nIf you select a total cost category and an individual cost category, only the individual cost category will be shown."
    }
    conf_mess <- function(){
      if(input$Sect_sel=="CV"){
"\n
        \nSee the confidentiality section under the ABOUT tab for more information."
      } else {
        ""
      }
    }
    supp_obs <- function() {
"\n
      \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality."
    }
    #suff_flag <- function(){
      #paste("\n* Data has been suppressed for this selected",input$CategorySelect, "and year as there are not enough observations to protect confidentiality.")
    #}
    
    supp_whiting <- function() {
      if(input$Sect_sel!='FR') {
"\n
        \nYour selection would reveal confidential data because the categories you selected are additive. \nIn these cases, only results for 'All vessels' have been shown."
      } else {
"\n
        \nYour selection would reveal confidential data because the categories you selected are additive. \nIn these cases, only results for 'All processors' have been shown."
      }
    }

    # define labels
    xlab <- function () {
      if(max(dat$conf, na.rm=T)==0){
        if(max(dat$conf_trt, na.rm = T)==0){
          if(input$PlotSelect!='Stacked bar') {
            ""
          } else {
            paste(stacked_bar_mess())
          }} else{
            if(input$PlotSelect!='Stacked bar'){
              paste(supp_obs(), conf_mess())
            } else {
              paste(stacked_bar_mess(), supp_obs(), conf_mess())
            }}} else {
              if (max(dat$conf_trt, na.rm = T)==0) {
                if(input$PlotSelect!= 'Stacked bar') {
                  paste(supp_obs(), supp_whiting(), conf_mess())
                } else {
                  paste(stacked_bar_mess(), supp_obs(), supp_whiting(), conf_mess())
                }} else {
                  if (input$PlotSelect != 'Stacked bar') {
                    paste(supp_obs(), supp_whiting(), conf_mess())
                  } else {
                    paste(stacked_bar_mess(), supp_obs(), supp_whiting(), conf_mess())
                  }
                }
            }
    }
   
    
    g <- g + labs(y=ylab(), x=xlab(), title=main())

    
     #   reorder(groupVar, dat$barorder    
        if(length(yr())>1 & min(yr())<2011 & max(yr())>2010){
                 g <- g + geom_rect(aes(xmin=-Inf, xmax=table(yr()<=2010)[[2]]+.5, ymin=-Inf, ymax=Inf), 
                                    alpha=.02, fill="grey50") +
                          geom_text(aes(x=table(yr()<=2010)[[2]]/3.5,y=thresh(), label="Pre-Catch shares"), 
                                    family="serif",fontface="italic", 
                                    hjust=0,color = "grey40", size=7/scale_text()) +
                          geom_text(aes(x=table(yr()<=2010)[[2]]+table(yr()>2010)[[2]]/1.5,y=thresh(),
                                        label="Catch shares"),
                                    hjust=0, family = "serif", fontface="italic", color = "grey40", size=7/scale_text())  
            } else {
              g <- g  
     } # end 
    
              
    # define theme
    g <- g + theme(
      plot.title = element_text( vjust=1, hjust=0.5, size=rel(1.5), colour="grey25", family = "sans", face = "bold"),# 
      panel.background = element_rect(fill = "white"),
      plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", size = 16, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=rel(1.1),  face="italic", vjust=-1, hjust=0, colour="grey25"),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", color = "grey25", face = "bold", size = 10),
      legend.key.size=unit(2,'line'),
      legend.title = element_blank()
    )
    
    if(input$PlotSelect!="Line"){
    g <- g +guides(fill=guide_legend(nrow=2, byrow=TRUE))
    } else {
    g <- g +guides(colour=guide_legend(nrow=2, byrow=TRUE))
    }
    ############################################################################################################
    ##function to wrapping facet labels and remove dots before facet labels 
#    strwrap_strip_text = function(p, pad=0.02) { 
      # get facet font attributes
#      th = theme_get()
#      if (length(p$theme) > 0L)
#        th = th + p$theme
      
#      require("grid")
#      grobs <- ggplotGrob(p)
      
      # wrap strip x text
#      ps = calc_element("strip.text.x", th)[["size"]]
#      family = calc_element("strip.text.x", th)[["family"]]
#      face = calc_element("strip.text.x", th)[["face"]]
#      
#      nm = names(p$facet$facets)
      
      # get number of facet columns
#      levs = levels(factor(p$data[[nm]]))
#      npanels = length(levs)
#      cols = n2mfrow(npanels)[1]
      
      # get plot width
#      sum = 0#sum(sapply(grobs$width, function(x) convertWidth(x, "in")))##
#      panels_width = par("din")[1] - sum  # inches
      # determine strwrap width
#      panel_width = panels_width / cols +.5
#      mx_ind = which.max(nchar(levs))
#      char_width = strwidth(levs[mx_ind], units="inches", cex=ps / par("ps"), 
#                            family=family, font=gpar(fontface=face)$font) / 
#        nchar(levs[mx_ind])
#      width = floor((panel_width - pad)/ char_width)  # characters
      
      # wrap facet text
#      p$data[[nm]] = unlist(lapply(strwrap(p$data[[nm]], width=width, 
#                                           simplify=FALSE), paste, collapse="\n"))
#      p$data[[nm]] = gsub("([.])", "\\ ", p$data[[nm]]) 
#      invisible(p)
#    }   
    #################################################################################################################################
    
#    g <- strwrap_strip_text(g)
    
    print(g)
    
    } else plot(0,0,type="n", axes=F, xlab="", ylab="")
}