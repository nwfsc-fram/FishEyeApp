doPlotDownload <- function(dat, x, y){
  if(PermitPlot()){
    dat <- subset(dat, is.na(dat$VALUE)==FALSE)
    
    
    dat$sort2 <- if(input$LayoutSelect!="Metrics"){
      if(input$Ind_sel=='Other'){
        if(input$socSelect=='Share of landings by state'){
          reorder(dat$AGID, dat$sort)
        } else {
          reorder(dat$VARIABLE, dat$sort)
        }
      }else {
        reorder(dat$VARIABLE, dat$sort)
      }} else {
        if(input$Ind_sel=="Economic"){ 
          reorder(dat$SHORTDESCR, dat$sort)
        } else {
          reorder(dat$METRIC, dat$sort)
        }}
    
    dat$thresh <-  if(input$Ind_sel=="Economic"){   
      data.frame(dat %>% group_by(SHORTDESCR) %>% mutate(threshold=length(table(YEAR[YEAR<=2010]))))%>% subset(select=c(threshold))
    } else if(input$Ind_sel!="Economic"){ 
      if(input$LayoutSelect=="Metrics"){   
        if(input$PlotSelect==T&!is.na(max(dat$VARIANCE))) { 
          data.frame(dat %>% group_by(METRIC) %>% mutate(threshold=max(VALUE, na.rm=T)+max(VARIANCE, na.rm=T)+max(VALUE, na.rm=T)/10))%>% subset(select=c(threshold))
        } else {
          data.frame(dat %>% group_by(METRIC) %>% mutate(threshold=max(VALUE, na.rm=T)+max(VALUE, na.rm=T)/10))%>% subset(select=c(threshold))
        }
      } else {
        0
      }}

    rectvars <- dat %>% distinct(sort2,YEAR) %>% group_by(sort2) %>% mutate(minx=min(as.numeric(YEAR)), xmaxscale=length(YEAR[YEAR<2011]), maxx=max(YEAR))  %>% 
      subset(select=c(sort2, minx,xmaxscale, maxx)) %>%data.frame()%>% distinct %>% 
      merge(dat %>% distinct(sort2,whitingv))
    
    dat$upper <- 
      if(input$Ind_sel=="Economic"){
        if(input$AVE_MED=='A'){
          dat$VALUE+dat$VARIANCE
        } else if(input$AVE_MED=='T'){ 
          dat$VALUE
        } else { 
          dat$q75
        }} else if(input$Ind_sel!="Economic"){
          if(input$AVE_MED2=='Mean'){
            dat$VALUE+dat$VARIANCE
          } else if (input$AVE_MED2=='Total') {
            dat$VALUE
          } else {
            dat$q75
          }}
    
    
    dat$lower <- 
      #      if(input$PlotSelectOption=="Standard deviation or Median average deviation") 
      if(input$Ind_sel=="Economic"){
        if(input$AVE_MED=='A'){
          dat$VALUE-dat$VARIANCE
        } else  { 
          dat$q25
        }} else if(input$Ind_sel!="Economic"){
          if(input$AVE_MED2=='Mean'){
            dat$VALUE-dat$VARIANCE
          } else  { 
            dat$q25
          }}
    
    
    
    upper <- function(){
      #      if(input$PlotSelectOption=="Standard deviation or Median average deviation") 
      if(input$Ind_sel=="Economic"){
        if(input$AVE_MED=='A'){
          max(dat$VALUE+dat$VARIANCE)
        } else if(input$AVE_MED=='T'){ 
          max(dat$VALUE)
        } else {
          max(dat$q75)
        }
      } else if(input$Ind_sel!="Economic"){
        if(input$AVE_MED2=='Mean'){
          max(dat$VALUE+dat$VARIANCE)
        } else if(input$AVE_MED2=='Total') { 
          max(dat$VALUE)
        } else {max(dat$q75)}}
    }
    
    
    lower <- function(){
      #      if(input$PlotSelectOption=="Standard deviation or Median average deviation") 
      if(input$Ind_sel=="Economic"){
        if(input$AVE_MED=='A'){
          dat$VALUE-dat$VARIANCE
        } else  { 
          dat$q25
        }} else if(input$Ind_sel!="Economic"){
          if(input$AVE_MED2=='Mean'){
            dat$VALUE-dat$VARIANCE
          } else  { 
            dat$q25
          }}
    }
    
    
    
    
    yr <- function(){
      return(unique(as.numeric(dat$YEAR)))
    }
    
    groupVar <- "whitingv"
    
    colourThirds <- if(input$Sect_sel!="FR") {
      c('Non-whiting vessels'="#d7191c",'Whiting vessels'="#2b83ba",'All vessels'="#000000")
    } 
    else {
      c('Non-whiting processors'="#d7191c",'Whiting processors'="#2b83ba",'All processors'="#000000")
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
      if(input$Sect_sel == "CV"){
        return("Performance Metrics for West Coast Catcher Vessels")
      } else if(input$Sect_sel == "M"){
        return("Performance Metrics for West Coast Mothership Vessels")
      } else if(input$Sect_sel == "CP"){
        return("Performance Metrics for West Coast Catcher-Processor Vessels")
      }else if (input$Sect_sel == "FR"){
        return("Performance Metrics for West Coast First Receivers and Shorebased Processors")
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
            } else if (input$Ind_sel=="Other"){
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
            else if(input$Ind_sel=="Other"){
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
    
    
    # y-axis label ####
    ylab <- function() {
      if (input$Ind_sel == "Economic") {
        paste(input$ShortdescrSelect,
              "(",
              input$StatSelect,
              "in",
              dat$unit,
              currentyear,
              "$",
              ")")
      } else if (input$Ind_sel == "Other") {
        if (input$LayoutSelect != 'Metrics') {
          if (input$socSelect == "Revenue per day") {
            paste(input$socSelect,
                  "(",
                  input$StatSelect,
                  "in",
                  dat$unit,
                  currentyear,
                  "$",
                  ")")
          }  else if (input$socSelect == "Seasonality") {
            expression(bold("Day of year when 50% of catch was landed"))
          }  else if (input$socSelect == "Share of landings by state") {
            expression(bold("Share of landings (% of revenue)"))
          }  else if (input$socSelect == "Gini coefficient") {
            expression(bold("Gini coefficient (0 - 1)"))
          } else if (input$socSelect == "Fuel use per day") {
            paste(input$socSelect,
                  "(",
                  input$StatSelect,
                  "in",
                  dat$unit,
                  "gallons)")
          } else if (input$socSelect == 'Speed while fishing') {
            paste(input$socSelect,
                  "(",
                  input$StatSelect,
                  "in",
                  dat$unit,
                  "knots)")
          } else {
            input$socSelect
          }
        } else {
          expression(bold('Scale and units depend upon metric'))
        }
      } else if (input$Ind_sel == "Vessel characteristics" ||
                 input$Ind_sel == 'Processor characteristics') {
        if (input$LayoutSelect != 'Metrics') {
          if (input$demSelect == "Proportion of revenue from CS fishery") {
            expression(bold("Proportion of revenue from catch share fishery"))
          }  else if (input$demSelect == "Number of fisheries") {
            expression(bold("Number of fisheries"))
          }  else if (input$demSelect == "Vessel length") {
            expression(bold("Vessel length (in feet)"))
          } else if (input$demSelect == 'Revenue diversification') {
            expression(bold('Revenue diversification (Exponential Shannon Index)'))
          }  else {
            input$demSelect
          }
        } else {
          expression(bold('Scale and units depend upon metric'))
        }
      } else if (input$Ind_sel == 'Labor') {
        if (input$LayoutSelect != 'Metrics') {
          if(input$crewSelect != "Number of positions (captain and crew)" &
             input$crewSelect != 'Number of crew-days') {
            paste(input$crewSelect,
                  "(",
                  input$StatSelect,
                  "in",
                  dat$unit,
                  currentyear,
                  "$",
                  ")")
          } else {
            paste(input$crewSelect)
          }
        } else {
          expression(bold('Scale and units depend upon metric'))
        }
      }
    }
      
    
source_lab <- function(){
  paste("\n
        \nSourced from the FISHEyE application (https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",format(Sys.Date(), format="%B %d %Y"))
}
supp_obs <- function(){
  "\n
  \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality."
}
conf_mess <- function(){
  "\n
  \nSee the confidentiality section under the ABOUT tab for more information."
}

supp_whiting <- function(){
  if(input$Sect_sel!="FR"){
    "\n
    \nYour selection would reveal confidential data because the categories selected are additive. \nIn these cases, only results for 'All vessels' have been shown."
  } else {
    "\n
    \nYour selection would reveal confidential data because the categories selected are additive. \nIn these cases, only results for 'All processors' have been shown."}
  } 
supp_obs_whiting <- function() {
  if(input$Sect_sel!='FR') {
    "\n
    \nAdditionally, your selection would reveal confidential data because the categories selected are additive. \nIn these cases, only results for 'All vessels' have been shown."
  } else {
    "\n
    \nAdditionally, your selection would reveal confidential data because the categories selected are additive. \nIn these cases, only results for 'All processors' have been shown."}        
  }


supp_metric <- function(){
  "\n
  \nSome metrics may not be shown because the selected statistic is not calculated for that metric." 
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
            paste(supp_obs(), supp_obs_whiting(), conf_mess(), source_lab())
          }}}
    else if(input$Ind_sel=="Demographic"){
      if(input$demSelect=="Number of fisheries"|input$demSelect=="Proportion of revenue from CS fishery"){
        if(max(dat$conf)==0) {
          if(max(dat$flag)==0){
            if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
              paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, 
                    \nnot just their activity in the selected fishery. 
                    \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".",
                    source_lab())
            } else {
              source_lab()
            }}else {
              if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
                paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, 
                      \nnot just their activity in the selected fishery. \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".",
                      supp_obs(), source_lab()) 
              } else {
                paste(supp_obs(), source_lab())
              }}} else {
                if(max(dat$flag)==0){
                  if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
                    paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, 
                          \nnot just their activity in the selected fishery. 
                          \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".",
                          supp_whiting(), conf_mess(), source_lab())
                  } else{
                    paste(supp_whiting(), conf_mess(), source_lab())
                  }} else {
                    if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
                      paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, 
                            \nnot just their activity in the selected fishery. 
                            \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".",
                             supp_obs(), supp_obs_whiting(), conf_mess(), source_lab())
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
              paste(supp_obs(), supp_obs_whiting(), conf_mess(), source_lab())
            }}}
    }else if(input$Ind_sel=="Other"){
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
              paste("For the", input$socSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$socSelect,"for vessels that homeported in", input$VariableSelect,".",
                    supp_whiting(), conf_mess(), source_lab())
            } else {
              paste(supp_whiting(), conf_mess(), source_lab())
            }}  else {
              if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                paste("For the", input$socSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$socSelect,"for vessels that homeported in", input$VariableSelect,".",
                      supp_obs(), supp_obs_whiting(), conf_mess(), source_lab())
              } else {
                paste(supp_obs(), supp_obs_whiting(), conf_mess(), source_lab())
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
              paste(supp_obs(), supp_obs_whiting(), conf_mess(), source_lab())
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
            paste(supp_metric(), supp_obs(), supp_obs_whiting(), conf_mess())
          } else {
            paste(supp_whiting(), conf_mess(), source_lab())
          } 
        } else if(max(dat$flag)==1){
          if(max(dat$metric_flag)==1){
            paste(supp_metric(), supp_obs(), supp_obs_whiting(), conf_mess(), source_lab())
          } else {
            paste(supp_obs(), supp_obs_whiting(), conf_mess(), source_lab())
          } 
        }}
    }
}#end x label function 

    
   scale_text <- function() {
      if(input$Ind_sel!="Economic"){
        return(1.1)
      } else { 
        return(1.2)
      }}   
    
    scale_geom_text <- function() {
      if(sum(dat$VALUE, na.rm=T)!=0) {
        return(max(dat$VALUE, na.rm=T))
      } else {
        return(0)
      }
    }
    
#----- Define ggplot ------#    
    # format data for graph and add to graph ####
    # special data for seasonality plot
    #  print(paste0(seasonality, 1))
    if(input$Ind_sel == 'Other' & input$LayoutSelect != 'Metrics') {
      # and seasonality is selected
      if(input$socSelect =="Seasonality") {
        ssn <- mutate(dat, 
                      VALUE = as.Date(VALUE, origin = "2014-01-01", format = "%Y-%m-%d"),
                      sort2 = reorder(VARIABLE, sort))
        g <- ggplot(ssn, aes_string(x = x, y = y , group = groupVar), environment =
                      environment()) 
        # otherwise normal plot:
      } else {
        dat <- dat[order(dat$sort), ]
        g <-
          # I think this is where the NAs are getting removed which causes lines to be connected through suppressed/missing values #removeNAs
          ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment =
                   environment()) #+coord_cartesian(xlim = c(0, length(table(dat$YEAR))+1))
      }
    } else {
      dat <- dat[order(dat$sort), ]
      g <-
        # I think this is where the NAs are getting removed which causes lines to be connected through suppressed/missing values #removeNAs
        ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment =
                 environment()) #+coord_cartesian(xlim = c(0, length(table(dat$YEAR))+1))
    }
    
    #add lines and points to the plot ####
    g <- g + geom_line(aes_string(colour = groupVar), size = 1.5) +
      geom_point(aes_string(colour = groupVar), size = 4)

#------ Add variance ------#    
    if(input$PlotSelect==T&is.na(max(dat$VARIANCE))==F) { 
      g <- g + geom_ribbon(aes(ymax=upper, ymin=lower, fill=whitingv), alpha=.25)#show.legend = FALSE, 
    } else {
      g <- g
    }
    
    
      
#----- define facet -----#
      if (input$LayoutSelect!='Metrics') {
        g <- g + facet_wrap(~ sort2, ncol=2)
      } 
    else {
        g <- g + facet_wrap(~ sort2, ncol=2)
      }
    
#----- define scale -----#
    g <- g + scale_fill_manual(values = colourThirds) + scale_colour_manual(values = colourThirds)

#----- define solid line y=0 ------#
    g <- g + geom_hline(yintercept = 0)
    
#----- define labels ------#
    g <- g + labs(y = ylab(), x=xlab(), title = main())   
    
  
#----- Define rectangles and labels -----#
    #----- Define grey shading and Non-CS/CS labels ------####
    # choose label text size ####
    labeltext <- ifelse(input$tabs == 'Panel1', 7, 5)
    
    # geom_rect (define the grey boxes for pre-catch shares) ####
    geom_rect_fun <- function(ymin_val = -Inf, ymax_val = Inf) {
      geom_rect(
        aes(
          xmin = -Inf,
          xmax = table(yr() <= 2010)[[2]] + .5,
          ymin = ymin_val,
          ymax = ymax_val
        ),
        alpha = .05,
        fill = "grey50"
      )
    }
    
    geom_rect4seasonality <- geom_rect(
      aes(
        xmin = -Inf,
        xmax = table(yr() <= 2010)[[2]] + .5,
        ymin = structure(-Inf, class = "Date"),
        ymax = structure(Inf, class = "Date")
      ),
      alpha = .05,
      fill = "grey50"
    )
    
    # geom_text function ####
    
    geom_text_fun <- function(x_val, y_val, label_val, vjust_val = .5) {
      
      geom_text(
        aes(
          x = x_val,
          y = y_val,
          vjust = vjust_val,
          label = label_val,
          family = "serif",
          fontface = "italic"
        ),
        hjust = 0,
        color = "grey20",
        size = labeltext / scale_text()
      )
      
    }
    
    # set rect and text for plots with both CS and non-CS years ####
    # otherwise no rect or text
    # the original code for the geom_text* are commented out at the bottom of the doc
    # if there are years shown before and after implementation of catch shares
    if (length(yr()) > 1 & min(yr()) < 2011 & max(yr()) > 2010) {
      # if the "Group by vessels" display is chosen
      if (input$LayoutSelect != 'Metrics') {
        # if seasonality is clicked
        if(input$Ind_sel == 'Other') {
          # and seasonality is selected
          if(input$socSelect =="Seasonality") {
            g <- g + geom_rect_fun(
              ymin_val = structure(-Inf, class = "Date"),
              ymax_val = structure(Inf, class = "Date"))
            g <- g + geom_text_fun(
              x_val = table(yr() <= 2010)[[2]] / 3.5,
              y_val = min(as.Date(upper(), origin = "2014-01-01")),
              label_val = "Pre-catch shares")
            g <- g + geom_text_fun(
              x_val = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
              y_val = min(as.Date(upper(), origin = "2014-01-01")),
              label_val = "Catch shares")
            # for all other variables
          }} else {
            g <- g + geom_rect_fun()
            # geom_text1
            g <- g + geom_text_fun(
              x_val = table(yr() <= 2010)[[2]] / 3.5,
              y_val = max(upper()) + scale_geom_text()/5,
              label_val = "Pre-catch shares")
            g <- g +
              # geom_text3
              geom_text_fun(
                x_val = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
                y_val = max(upper()) + scale_geom_text() / 5,
                label_val = "Catch shares")
          } 
      } else { # Compare by metrics
        g <- g + geom_rect_fun()
        g <- g +
          # geom_text4
          geom_text_fun(
            x_val = table(yr() <= 2010)[[2]] / 3.5,
            y_val = Inf,
            label_val = "Pre-catch shares",
            vjust_val = 1.5)
        g <- g +
          # geom_text6
          geom_text_fun(
            x_val = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
            y_val = Inf,
            label_val = "Catch shares",
            vjust_val = 1.5)
        
      } } else {
        # end of rect/text for cs/non-cs, no CS box required for plots with only one "kind" of year
        g <- g
      }

#----- define theme ------#
    g <- g + theme(
      plot.title = element_text(size=rel(1.2), colour="grey25",family = "sans", face = "bold", vjust = 1, hjust=.5),
      plot.margin = unit(c(0.25, 0.25, 1, 0.25), "cm"),
      panel.background = element_rect(fill = "white"),
      #panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", size = 13, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=rel(.7), face="italic", hjust= 0, vjust=0, colour="grey25"),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 11),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
                                 color = "grey25", face = "bold", size = 10),
      legend.title = element_blank())
    

    
    ##function to wrapping facet labels
 #   strwrap_strip_text = function(p, pad=0) { 
 #     # get facet font attributes
#      th = theme_get()
#      if (length(g$theme) > 0L)
#        th = th + g$theme
      
#      require("grid")
#      grobs <- ggplotGrob(g)
      
      # wrap strip x text
#      ps = calc_element("strip.text.x", th)[["size"]]
#      family = calc_element("strip.text.x", th)[["family"]]
#      face = calc_element("strip.text.x", th)[["face"]]
      
#      nm = names(g$facet$facets)
      
      # get number of facet columns
#      levs = levels(factor(g$data[[nm]]))
#      npanels = length(levs)
#      cols = n2mfrow(npanels)[1]
      
      # get plot width
 #     sum = .4#sum(sapply(grobs$width, function(x) convertWidth(x, "in")))
 #     panels_width = par("din")[1] - sum  # inches
      # determine strwrap width
 #     panel_width = panels_width / cols
 #     mx_ind = which.max(nchar(levs))
 #     char_width = strwidth(levs[mx_ind], units="inches", cex=ps / par("ps"), 
 #                           family=family, font=gpar(fontface=face)$font) / 
 #       nchar(levs[mx_ind])
 #     width = floor((panel_width - pad)/ char_width)  # characters (pad=0)
      
      # wrap facet text
 #     g$data[[nm]] = unlist(lapply(strwrap(g$data[[nm]], width=width, 
 #                                          simplify=FALSE), paste, collapse="\n"))
 #     g$data[[nm]] = gsub("([.])", "\\ ", g$data[[nm]]) 
      
 #     invisible(g)
#    }   
    
    #    print(g)
#    g <- invisible(strwrap_strip_text(g)) #use instead of print(g)
    print(g)
    
  } #else plot(0,0,type="n", axes=F, xlab="", ylab="")
}