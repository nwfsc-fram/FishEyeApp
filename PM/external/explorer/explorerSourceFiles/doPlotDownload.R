doPlotDownload <- function(dat, x, y){
  if(PermitPlot()){
    dat <- subset(dat, is.na(dat$VALUE)==FALSE)
    
    
    if(input$Ind_sel=="Economic"){   
      dat$thresh <- data.frame(dat %>% group_by(SHORTDESCR) %>% transmute(threshold=length(table(YEAR[YEAR<=2010]))))[,2]
    }
    
    groupVar <- "whitingv"
    
    colourThirds <- c('Non-whiting vessels'="#d7191c",'Whiting vessels'="#2b83ba",'All vessels'="#000000")

# Plot title construction
    plot.title <- function(){
      if(input$Sect_sel == "CV"){
        return("Performance Metrics for West Coast Catcher Vessels")
      } else if(input$Sect_sel == "M"){
        return("Performance Metrics for West Coast Motherships")
      } else if(input$Sect_sel == "CP"){
        return("Performance Metrics for West Coast Catcher Processors")
      }else if (input$Sect_sel == "FR"){
        return("Performance Metrics for West Coast First Receivers")
      }}
    
    gv <- function(){
      if(input$Ind_sel=="Economic"){
        if(input$CategorySelect=="Fisheries"){
          sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect))
        } else {
          sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect,"    Summed across:", input$inSelect))
        }
      } else {
        if(input$CategorySelect=="Fisheries"){
          sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$MetricSelect))
        } else {
          sprintf(paste("Category:",input$CategorySelect, "     Metric: ", input$MetricSelect,"    Summed across:", input$inSelect))   
        }
      }
    }
    
    main <- function(){
      bquote(atop(.(plot.title()), .(gv())))
    }
    
    
    ylab <- function(){
      if(input$Ind_sel=="Economic") {
        paste("Thousands of 2014 $","(",input$StatSelect, ")")   
        
      } else if(input$Ind_sel!="Economic") {
        if(input$MetricSelect=="Crew wage per day"|input$MetricSelect=="Revenue per crew day"){
          paste("Thousands of 2014 $","(",input$AVE_MED2, ")")
          
        } else if(input$MetricSelect=="Proportion of revenue from CS fishery"){
          "Proportion of revenue from Catch Share fishery"  
        }  else if(input$MetricSelect=="Gini coefficient"){
          "Gini coefficient (0 - 1)"
        } else if(input$MetricSelect=="Seasonality"){
          "Day of year when 50% of catch was landed"
        }  else if(input$MetricSelect=="Share of landings by state"){
          "Share of landings (% of revenue)"
        } else if(input$MetricSelect=="Fishery participation"){
          "Fishery participation (number of fisheries)"
        }  else if(input$MetricSelect=="Vessel length"){
          "Vessel length (in feet)"
        }  else if(input$MetricSelect=="Herfindahl-Hirschman Index"){
          "Herfindahl-Hirschman Index (0 - 10,000)"
        }  else {
          input$MetricSelect         
        }
      }
    }
    
    xlab <- function(){
      if(input$MetricSelect=="Fishery participation"|input$MetricSelect=="Proportion of revenue from CS fishery"){
        if(max(dat$conf)==0) {
          if(max(dat$flag)==0){
            if(input$CategorySelect=="Fisheries"){
              paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                    "\nFor individual fisheries and the", input$MetricSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$MetricSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".")
            } else {
              paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y")) 
            }}else {
              if(input$CategorySelect=="Fisheries"){
                paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
"\nFor individual fisheries and the", input$MetricSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$MetricSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".
                      \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality.") 
              } else {
                paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                      "\nData have been sueppressed for years that are not plotted as there are not enough observations to protect confidentiality.")        
              }}} else {
                if(max(dat$flag)==0){
                  if(input$CategorySelect=="Fisheries"){
                    paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
"\nFor individual fisheries and the", input$MetricSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$MetricSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".
                          \nYour selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
                          only results for 'All vessels' are shown. 
                          \nSee the confidentiality section under the ABOUT tab for more information.")
                  } else{
                    paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
"\nYour selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
                    only results for 'All vessels' are shown. 
                    \nSee the confidentiality section under the ABOUT tab for more information.")
                  }} else {
                    if(input$CategorySelect=="Fisheries"){
                      paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
"\nFor individual fisheries and the", input$MetricSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$MetricSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".
                            \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. 
                            \nIn addition, your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
                            only results for 'All vessels' are shown. 
                            \nSee the confidentiality section under the ABOUT tab for more information.")
                    } else {
                      paste("Sourced from the FISHEyE application (https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                      "\nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. 
                      \nIn addition, your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
                      only results for 'All vessels' are shown. 
                      \nSee the confidentiality section under the ABOUT tab for more information.")
                    } }}
                    } 
      else if(input$MetricSelect=="Share of landings by state"){
        if(max(dat$conf)==0) {
          if(max(dat$flag)==0){
            if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
              paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                    "\nFor the", input$MetricSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$MetricSelect,"for vessels that homeported in", input$VariableSelect,".")
            } else {
              paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"))
            }} else {
              if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
"\nFor the", input$MetricSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$MetricSelect,"for vessels that homeported in", input$VariableSelect,".
                      \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality." )
              } else {
                paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                      "\nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality.") 
              }}
        } else {
          if(max(dat$flag)==0){
            if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
              paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                "\nFor the", input$MetricSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,"\nFor example, the plots above show the", input$MetricSelect,"for vessels that homeported in", input$VariableSelect,".
                \nYour selection would reveal confidential data for years with sufficient observations.  For years when confidential data would be revealed, 
                 only results for 'All vessels' have been shown. 
               \nSee the confidentiality section under the ABOUT tab for more information.")
            } else {
              paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
              "\nYour selection would reveal confidential data for years with sufficient observations.  For years when confidential data would be revealed, 
                 only results for 'All vessels' have been shown. 
                \nSee the confidentiality section under the ABOUT tab for more information.")
            }}  else {
              if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                  "\nFor the", input$MetricSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$MetricSelect,"for vessels that homeported in", input$VariableSelect,".
                \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. 
                \nIn addition, your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
                 only results for 'All vessels' have been shown. 
                \nSee the confidentiality section under the ABOUT tab for more information.")
              } else {
                paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                  "\nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. 
                    \nIn addition, your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
                     only results for 'All vessels' have been shown. 
                   \nSee the confidentiality section under the ABOUT tab for more information.")
              }} }
            } else {
              if(max(dat$conf)==0) {
                if(max(dat$flag)==0){
                  paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"))
                } else {
                  paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
                  "\nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality.") 
                }} else {
                  if(max(dat$flag)==0){
                    paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
            "\nYour selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
           only results for 'All vessels' are shown. 
        \nSee the confidentiality section under the ABOUT tab for more information.")
                  }  else {
                    paste("Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),
        "\nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. 
          \nIn addition, your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
           only results for 'All vessels' are shown. 
        \nSee the confidentiality section under the ABOUT tab for more information.")
                  }}}
                    } #end x label function
    #end x label function

    
    scale_text <- function() {
      if(input$Ind_sel!="Economic"){
        if (min(input$YearSelect)<2009) {
          return(1.45)
        }  else {
              return(1.1)
        }} else { 
          b <- table(table(dat$SHORTDESCR)>1)[[1]]
          if(b == 1 | b ==3) {
            return(1.4)
          } else {
            return(1.8)
          } 
        }}   
    
    g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar, order='sort'), environment=environment()) #+coord_cartesian(xlim = c(0, length(table(dat$YEAR))+1))
    if(length(input$YearSelect)>1){
        g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
    } else {
      g <- g + geom_point(aes_string(colour = groupVar), size=4)
    }
    
    if(input$PlotSelect==T&dat$STAT[1]!="Fleet-wide total"&is.na(max(dat$VARIANCE))==F) { 
      g <- g + geom_ribbon(aes(ymax=VALUE+VARIANCE, ymin=VALUE-VARIANCE, fill=whitingv), alpha=.25)#show.legend = FALSE, 
    } else {
      g <- g
    }
    
    
    
    # Define rectangles and labels
    if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
      if(input$Ind_sel=="Economic"){
       g <- g + geom_rect(aes(xmin=-Inf, xmax=dat$thresh, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.02)
       if(input$PlotSelect==T&dat$STAT[1]!="Fleet-wide total"&is.na(max(dat$VARIANCE))==F) {  
            g <- g + geom_text(aes(x=dat$thresh/3.5,y=max(VALUE+VARIANCE)+max(VALUE)/10, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=3/scale_text()) 
          if(length(input$YearSelect[input$YearSelect<2010])>3 & length(input$YearSelect[input$YearSelect>=2010])==2) {
            g <- g + geom_text(aes(x=dat$thresh+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+VARIANCE)+max(VALUE)/10,label="Post-Catch"),hjust=0, family="serif",color = "grey20", size=3/scale_text())+
              geom_text(aes(x=dat$thresh+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+VARIANCE)-max(VALUE+VARIANCE)/80,label="shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
          } else {
            g <- g + geom_text(aes(x=dat$thresh+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+VARIANCE)+max(VALUE)/10,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
          }
        } else {
          g <- g + geom_text(aes(x=dat$thresh/3.5,y=max(VALUE+0)+max(VALUE)/10, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=3/scale_text()) 
          if(length(input$YearSelect[input$YearSelect<2010])>3 & length(input$YearSelect[input$YearSelect>=2010])==2) {
            g <- g + geom_text(aes(x=dat$thresh+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+0)+max(VALUE)/10,label="Post-Catch"),hjust=0, family="serif",color = "grey20", size=3/scale_text())+
              geom_text(aes(x=dat$thresh+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+0)-max(VALUE)/80,label="shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
          } else {
            g <- g + geom_text(aes(x=dat$thresh+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+0)+max(VALUE)/10,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
          }}
      } 
      else {
        g <- g + geom_rect(aes(xmin=-Inf, xmax=length(table(dat$YEAR[dat$YEAR<=2010])), ymin=-Inf, ymax=Inf),fill="grey50", alpha=.02)
        if(input$PlotSelect==T&dat$STAT[1]!="Fleet-wide total"&is.na(max(dat$VARIANCE))==F) {  
          g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))/3.5,y=max(VALUE+VARIANCE)+max(VALUE)/10, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=3/scale_text())  
          if(length(input$YearSelect[input$YearSelect<2010])>3 & length(input$YearSelect[input$YearSelect>=2010])==2) {
            g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2.75,y=max(VALUE+VARIANCE)+max(VALUE)/10,label="Post-Catch "),hjust=0, family="serif",color = "grey20", size=3/scale_text())#+
              geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2.75,y=max(VALUE+VARIANCE)-max(VALUE+VARIANCE)/50,label="shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
          } else {
            g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2.75,y=max(VALUE+VARIANCE)+max(VALUE)/10,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
          }
        } else {
          g <- g + geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])/3.5,y=max(VALUE+0)+max(VALUE)/10, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=3/scale_text())  
          if(length(input$YearSelect[input$YearSelect<2010])>3 & length(input$YearSelect[input$YearSelect>=2010])==2) {
            g <- g + geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])+length(input$YearSelect[input$YearSelect>2010])/2.75,y=max(VALUE+0)+max(VALUE)/10,label="Post-Catch"),hjust=0, family="serif",color = "grey20", size=3/scale_text())+
              geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])+length(input$YearSelect[input$YearSelect>2010])/2.75,y=max(VALUE+0)-max(VALUE)/50,label="shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
          } else {
            g <- g + geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])+length(input$YearSelect[input$YearSelect>2010])/2.75,y=max(VALUE+0)+max(VALUE)/10,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
          }
        } 
      }} else {
        g <- g  
      }
    g <- g + geom_text(aes(label=star), colour="black", vjust=0, size=3)
    
    # define facet
    if(input$Ind_sel!="Economic"){
      if(input$MetricSelect=="Share of landings by state"){
        g <- g + facet_wrap(~ agid, as.table = TRUE)#, scales="free_x"
      } else { g <- g + facet_wrap(~ sort, as.table = TRUE)#, scales="free_x"
      }} else {
        g <- g + facet_wrap(~SHORTDESCR, scales="free_x")#(~), sortsortas.table=TRUE,
      }
    # define scale
    g <- g + scale_fill_manual(values = colourThirds) + scale_colour_manual(values = colourThirds)

    # define solid line y=0
    g <- g + geom_hline(yintercept = 0)
    
    # define labels
    g <- g + labs(y = ylab(), x=xlab(), title = main())   
    
    
    # define theme
    g <- g + theme(
      plot.title = element_text(size=rel(1), vjust=1, colour="grey25"), 
      plot.title = element_text(family = "sans", face = "bold", vjust = 1),
      plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      panel.background = element_rect(fill = "white"),
      panel.margin = unit(1, "lines"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", size = 9, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=rel(.7), face="italic", vjust=0, colour="grey25"),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 6),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
                                 color = "grey25", face = "bold", size = 8),
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