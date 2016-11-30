doPlotDownload <- function(dat, x, y){
  if(PermitPlot()){
    dat <- subset(dat, is.na(dat$VALUE)==FALSE)
    
    
    if(input$Ind_sel=="Economic"){   
      dat$thresh <- data.frame(dat %>% group_by(SHORTDESCR) %>% transmute(threshold=length(table(YEAR[YEAR<=2010]))))[,2]
    }
    else if(input$Ind_sel!="Economic"){ 
      if(input$LayoutSelect=="Metrics"){   
        if(input$PlotSelect==T&dat$STAT[1]!="Fleet-wide total"&is.na(max(dat$VARIANCE))==F) { 
          dat$thresh <- as.numeric(data.frame(dat %>% group_by(METRIC) %>% transmute(threshold=max(VALUE, na.rm=T)+max(VARIANCE)+max(VALUE, na.rm=T)/10))[,2])
        } else {
          dat$thresh <- as.numeric(data.frame(dat %>% group_by(METRIC) %>% transmute(threshold=max(VALUE, na.rm=T)+max(VALUE, na.rm=T)/10))[,2])
        }
      } else {
        dat$thresh <- 0
      }}
    
    groupVar <- "whitingv"
    
    colourThirds <- if(input$Sect_sel!="FR") {
      c('Non-whiting vessels'="#d7191c",'Whiting vessels'="#2b83ba",'All vessels'="#000000")
    } else {
      c('Non-whiting processors'="#d7191c",'Whiting processors'="#2b83ba",'All processors'="#000000")
    }
    
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
      if(input$LayoutSelect!="Metrics"){
        if(input$Ind_sel=="Economic"){
          if(input$Sect_sel!="CV"&input$Sect_sel!="FR"){
            sprintf(paste("Economic measure:", dat$SHORTDESCR[1], "     Statistic: ",  input$StatSelect))
          } else{
            if(input$CategorySelect=="Fisheries"|input$CategorySelect=="Production activities"){
              sprintf(paste("Economic measure:", dat$SHORTDESCR[1], "     Statistic: ",  input$StatSelect))
            } else {
              sprintf(paste("Economic measure:", dat$SHORTDESCR[1], "     Statistic: ", input$StatSelect,"    Summed across:", input$inSelect))
            }
          }}#end economic 
        else {
          # if(input$MetricSelect[1]!='Number of vessels'&input$MetricSelect!="Share of landings by state"&input$MetricSelect!='Gini coefficient'&input$MetricSelect!='Herfindahl-Hirschman Index'&input$MetricSelect!='Seasonality'&input$MetricSelect!="Vessel length"){
          if(max(dat$metric_flag)==0){
            if(input$Ind_sel=="Demographic"){
              if(input$Sect_sel!="CV"&input$Sect_sel!="FR"){
                sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$demSelect, "   Statistic:", input$AVE_MED2))
              } else{
                if(input$CategorySelect=="Fisheries"|input$CategorySelect=="Production activities"){
                  sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$demSelect, "   Statistic:", input$AVE_MED2))
                } else {
                  sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$demSelect,"   Statistic:", input$AVE_MED2,"    Summed across:", input$inSelect))   
                }}
            } else if (input$Ind_sel=="Social and Regional"){
              if(input$socSelect=="Share of landings by state")  {
                if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"){
                  sprintf(paste("Variable:", input$VariableSelect,"     Metric: ", input$socSelect, '  Statistic: Percentage   Summed across:', input$inSelect))
                } else {
                  sprintf(paste("Variable:",input$VariableSelect,"     Metric: ", input$socSelect, "  Statistic: Percentage"))   
                } 
              } else {
                if(input$Sect_sel=="CV"&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Production activities"){
                  sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$socSelect,"   Statistic:", input$AVE_MED2,"    Summed across:", input$inSelect))   
                } else {
                  sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$socSelect, "   Statistic:", input$AVE_MED2))
                } 
              }}
          }# end normal cases
          else {
            if(input$Ind_sel=="Demographic"){
              if(input$demSelect=="Number of vessels"|input$demSelect=="Number of processors"){
                if(input$Sect_sel=="CV"&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Production activities"){
                  sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$demSelect, '  Statistic: Total     Summed across:', input$inSelect))
                } else {
                  sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$demSelect, "  Statistic: Total"))   
                } 
              } else if(input$demSelect=="Vessel length")  {
                if(input$Sect_sel=="CV"&input$CategorySelect!="Fisheries"){
                  sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$demSelect, '  Statistic: Average maximum length      Summed across:', input$inSelect))
                } else {
                  sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$demSelect, "  Statistic: Average maximum length"))   
                } 
              }
              else {      
                if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Production activities"){
                  sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$demSelect, '  Statistic: Index value',"    Summed across:", input$inSelect))
                } else {
                  sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$demSelect, '  Statistic: Index value'))   
                } 
              }}
            else if(input$Ind_sel=="Social and Regional"){
              if(input$socSelect=="Seasonality"){
                if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Production activities"){
                  sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$socSelect, '  Statistic: Day of year  Summed across:', input$inSelect))
                } else {
                  sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$socSelect, "  Statistic: Day of year"))   
                } 
              } else if(input$socSelect=="Share of landings by state")  {
                if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"){
                  sprintf(paste("Variable:", input$VariableSelect,"     Metric: ", input$socSelect, '  Statistic: Percentage   Summed across:', input$inSelect))
                } else {
                  sprintf(paste("Variable:",input$VariableSelect,"     Metric: ", input$socSelect, "  Statistic: Percentage"))   
                } 
              }
              else {      
                if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Production activities"){
                  sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$socSelect, '  Statistic: Index value',"    Summed across:", input$inSelect))
                } else {
                  sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$socSelect, '  Statistic: Index value'))   
                } 
              }}
            
          } #END HERE
        } #END NOT ECONOMIC
      } #end compare vessel groupings
      else {
        if(input$Ind_sel=="Economic"){
          if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Production activities"){
            sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ",  input$StatSelect,"    Summed across:", input$inSelect))
          } else {
            sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect))
          }
        } else {
          if(max(dat$metric_flag==0)){
            if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Production activities"){
              sprintf(paste(input$CategorySelect, ":",input$VariableSelect, " Statistic:",  input$AVE_MED2,"  Summed across:", input$inSelect))
            } else {
              sprintf(paste(input$CategorySelect,":",input$VariableSelect,  " Statistic:",  input$AVE_MED2))   
            }
          }
          else {
            if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Production activities"){
              sprintf(paste(input$CategorySelect, ":",input$VariableSelect, "  Statistic: ", input$AVE_MED2,"  Summed across:", input$inSelect))
            } else {
              sprintf(paste(input$CategorySelect,":",input$VariableSelect, '  Statistic: ', input$AVE_MED2))   
            }
            
          }
        }
      }}
    
    main <- function(){
      bquote(atop(.(plot.title()), .(gv())))
    }
    
    
    ylab <- function(){
      if(input$Ind_sel=="Economic") {
        expression(paste(bold("Thousands of 2014 $","(",input$StatSelect, ")")))   
      } else if(input$Ind_sel=="Social and Regional") {
        if(input$LayoutSelect!='Metrics'){
          if(input$socSelect=="Crew wage per day"|input$socSelect=="Revenue per crew day"){
            expression(paste(bold("Thousands of 2014 $","(",input$AVE_MED2, ")")))
          }  else if(input$socSelect=="Seasonality"){
            expression(bold("Day of year when 50% of catch was landed"))
          }  else if(input$socSelect=="Share of landings by state"){
            expression(bold("Share of landings (% of revenue)"))
          }  else if(input$socSelect=="Hourly compensation"){
            expression(bold("Hourly compensation ($)"))
          }   else {
            input$socSelect         
          }
        } else {
          expression(bold('Scale and units depend upon metric'))
        } 
      }else if(input$Ind_sel=="Demographic"){
        if(input$LayoutSelect!='Metrics'){
          if(input$demSelect=="Proportion of revenue from CS fishery"){
            expression(bold("Proportion of revenue from Catch Share fishery"))  
          }  else if(input$demSelect=="Gini coefficient"){
            expression(bold("Gini coefficient (0 - 1)"))
          }  else if(input$demSelect=="Fishery participation"){
            expression(bold("Fishery participation (number of fisheries)"))
          }  else if(input$demSelect=="Vessel length"){
            expression(bold("Vessel length (in feet)"))
#          }  else if(input$demSelect=="Herfindahl-Hirschman Index"){
#            expression(bold("Herfindahl-Hirschman Index (0 - 10,000)"))
          } else {
            input$demSelect     
          }
        }else {expression(bold('Scale and units depend upon metric'))}
      }
    }
    
    
source_lab <- function(){
  paste("\nSourced from the FISHEyE application (https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",format(Sys.Date(), format="%B %d %Y"))
}
supp_obs <- function(){
  paste("\nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality.")
}
conf_mess <- function(){
"\nSee the confidentiality section under the ABOUT tab for more information."
}

supp_whiting <- function(){
  if(input$Sect_sel!='FR'){
"\nYour selection would reveal confidential data for years with sufficient observations. 
  For years when confidential data would be revealed, only results for 'All vessels' have been shown."
  } else {
"\nYour selection would reveal confidential data for years with sufficient observations. 
  For years when confidential data would be revealed, only results for 'All processors' have been shown."    
  }
}

supp_metric <- function(){
  "Some metrics may not be shown because the selected statistic is not calculated for that metric."
  #For the Gini Coefficient, the index value is shown, regardless of the statistic selectd.
 #  \nFor number of vessels, only the total number of vessels is shown. For seasonality, the day when 50% of catch was landded is always shown.
#  \nFor Total Vessel length, we show maximum length. To protect confidentiality this value is the average of the longest three vessels."
}

xlab <- function(){
  if(input$LayoutSelect!="Metrics"){
    if(input$Ind_sel=="Economic"){
      if(max(dat$conf)==0) {
        if(max(dat$flag)==0){
          source_lab()
        } else {
          paste(supp_obs(), source_lab())
        }} else {
          if(max(dat$flag)==0){
            paste(supp_whiting(), conf_mess(), source_lab())
          }  else {
            paste(supp_obs(), supp_whiting(), conf_mess(), source_lab())
          }}}
    else if(input$Ind_sel=="Demographic"){
      if(input$demSelect=="Fishery participation"|input$demSelect=="Proportion of revenue from CS fishery"){
        if(max(dat$conf)==0) {
          if(max(dat$flag)==0){
            if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
              paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, 
                    \nnot just their activity in the selected fishery, 
                    \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".",
                    source_lab())
            } else {
              source_lab()
            }}else {
              if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
                paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, 
                      \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".",
                      supp_obs(), source_lab()) 
              } else {
                paste(supp_obs(), source_lab())
              }}} else {
                if(max(dat$flag)==0){
                  if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
                    paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, 
                          \nnot just their activity in the selected fishery, 
                          \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".",
                          supp_whiting(), conf_mess(), source_lab())
                  } else{
                    paste(supp_whiting(), conf_mess(), source_lab())
                  }} else {
                    if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
                      paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, 
                            \nnot just their activity in the selected fishery, 
                            \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".",
                            supp_obs(), supp_whiting(), conf_mess(), source_lab())
                    } else {
                      paste(supp_obs(), supp_whiting(), conf_mess(), source_lab())
                    } }}
      } else  {
        if(max(dat$conf)==0) {
          if(max(dat$flag)==0){
            source_lab()
          } else {
            paste(supp_obs(), source_lab())
          }} else {
            if(max(dat$flag)==0){
              paste(supp_whiting(), conf_mess(), source_lab())
            }  else {
              paste(supp_obs(), supp_whiting(), conf_mess(), source_lab())
            }}}
    }else if(input$Ind_sel=="Social and Regional"){
      if(input$socSelect=="Share of landings by state"){
        if(max(dat$conf)==0) {
          if(max(dat$flag)==0){
            if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
              paste("For the", input$socSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$socSelect,"for vessels that homeported in", input$VariableSelect,".",
                    source_lab())
            } else {
              source_lab()
            }} else {
              if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                paste("For the", input$socSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$socSelect,"for vessels that homeported in", input$VariableSelect,".",
                      supp_obs(), source_lab())
              } else {
                paste(supp_obs(), source_lab())
              }}
        } else {
          if(max(dat$flag)==0){
            if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
              paste("For the", input$socSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,"\nFor example, the plots above show the", input$socSelect,"for vessels that homeported in", input$VariableSelect,".",
                    supp_whiting(), conf_mess(), source_lab())
            } else {
              paste(supp_whiting(), conf_mess(), source_lab())
            }}  else {
              if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                paste("For the", input$socSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$socSelect,"for vessels that homeported in", input$VariableSelect,".",
                      supp_obs(), supp_whiting(), conf_mess(), source_lab())
              } else {
                paste(supp_obs(), supp_whiting(), conf_mess(), source_lab())
              }} }
      } else {
        if(max(dat$conf)==0) {
          if(max(dat$flag)==0){
            source_lab()
          } else {
            paste(supp_obs(), source_lab())
          }} else {
            if(max(dat$flag)==0){
              paste(supp_whiting(), conf_mess(), source_lab())
            }  else {
              paste(supp_obs(), supp_whiting(), conf_mess(), source_lab())
            }}}
    }} else {
      if(max(dat$conf)==0) {
        if(max(dat$flag)==0){
          if(max(dat$metric_flag)==1){
            paste(supp_metric())
          } else {
            source_lab()
          }
        } else if(max(dat$flag)==1){
          if(max(dat$metric_flag)==1){
            paste(supp_metric(), supp_obs())
          } else {
            paste(supp_obs(), source_lab())
          } 
        }
      } else if(max(dat$conf)==1){
        if(max(dat$flag==0)){
          if(max(dat$metric_flag)==1){
            paste(supp_metric(), supp_obs(), supp_whiting(), conf_mess())
          } else {
            paste(supp_whiting(), conf_mess(), source_lab())
          } 
        } else if(max(dat$flag)==1){
          if(max(dat$metric_flag)==1){
            paste(supp_metric(), supp_whiting(), supp_obs(), conf_mess(), source_lab())
          } else {
            paste(supp_whiting(), supp_obs(), conf_mess(), source_lab())
          } 
        }}
    }
}#end x label function 

    
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
       g <- g + geom_rect(aes(xmin=-Inf, xmax=dat$thresh+.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.02)
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
        g <- g + geom_rect(aes(xmin=-Inf, xmax=length(table(dat$YEAR[dat$YEAR<=2010]))+.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.02)
        if(input$PlotSelect==T&dat$STAT[1]!="Fleet-wide total"&is.na(max(dat$VARIANCE))==F) {  
          if (input$LayoutSelect!='Metrics') {
          g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))/3.5,y=max(VALUE+VARIANCE)+max(VALUE)/10, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=3/scale_text())  
          } else {
            g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))/3.5,y=thresh, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=3/scale_text())  
          }
          if(length(input$YearSelect[input$YearSelect<2010])>3 & length(input$YearSelect[input$YearSelect>=2010])==2) {
            if (input$LayoutSelect!='Metrics') {
              g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2.75,y=max(VALUE+VARIANCE)+max(VALUE)/10,label="Post-Catch "),hjust=0, family="serif",color = "grey20", size=3/scale_text())#+
              geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2.75,y=max(VALUE+VARIANCE)-max(VALUE+VARIANCE)/50,label="shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
            } else {
              g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2.75,y=thresh,label="Post-Catch "),hjust=0, family="serif",color = "grey20", size=3/scale_text())#+
              geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2.75,y=thresh,label="shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
            }
              } else {
                if (input$LayoutSelect!='Metrics') {
            g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2.75,y=max(VALUE+VARIANCE)+max(VALUE)/10,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
                } else {
                  g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2.75,y=thresh,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
                }
            }
        } else {
          if (input$LayoutSelect!='Metrics') {
          g <- g + geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])/3.5,y=max(VALUE+0)+max(VALUE)/10, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=3/scale_text())  
          } else {
            g <- g + geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])/3.5,y=thresh, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=3/scale_text())  
          }
          if(length(input$YearSelect[input$YearSelect<2010])>3 & length(input$YearSelect[input$YearSelect>=2010])==2) {
            if (input$LayoutSelect!='Metrics') {
              g <- g + geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])+length(input$YearSelect[input$YearSelect>2010])/2.75,y=max(VALUE+0)+max(VALUE)/10,label="Post-Catch"),hjust=0, family="serif",color = "grey20", size=3/scale_text())+
              geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])+length(input$YearSelect[input$YearSelect>2010])/2.75,y=max(VALUE+0)-max(VALUE)/50,label="shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
            } else {
              g <- g + geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])+length(input$YearSelect[input$YearSelect>2010])/2.75,y=thresh,label="Post-Catch"),hjust=0, family="serif",color = "grey20", size=3/scale_text())+
                geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])+length(input$YearSelect[input$YearSelect>2010])/2.75,y=thresh,label="shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
            }
              } else {
                if (input$LayoutSelect!='Metrics') {
                  g <- g + geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])+length(input$YearSelect[input$YearSelect>2010])/2.75,y=max(VALUE+0)+max(VALUE)/10,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
                } else {
                  g <- g + geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])+length(input$YearSelect[input$YearSelect>2010])/2.75,y=thresh,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=3/scale_text())
                }
                  }
        } 
      }} else {
        g <- g  
      }
 #   g <- g + geom_text(aes(label=star), colour="black", vjust=0, size=3)
    
    # define facet
      if(input$MetricSelect[1]=="Share of landings by state"){
        g <- g + facet_wrap(~ agid, as.table = TRUE)#, scales="free_x"
      } else  if (input$LayoutSelect!='Metrics') {
        g <- g + facet_wrap(~ sort, ncol=2)
      } else {
        g <- g + facet_wrap(~ sort, ncol=2, scales="free_y")
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
      axis.text = element_text(size = 10),
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