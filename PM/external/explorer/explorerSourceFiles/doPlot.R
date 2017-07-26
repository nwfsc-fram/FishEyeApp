doPlot <- function(dat, x, y){
  if(PermitPlot()){

    dat <- subset(dat, is.na(dat$VALUE)==FALSE)

    ######################################################
    
    dat$sort2 <- if(input$LayoutSelect!="Metrics"){
      if(input$Ind_sel=='Social and Regional'){
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
       data.frame(dat %>% group_by(SHORTDESCR) %>% transmute(threshold=length(table(YEAR[YEAR<=2010]))))[,2]
    }    else if(input$Ind_sel!="Economic"){ 
      if(input$LayoutSelect=="Metrics"){   
        if(input$PlotSelect==T&!is.na(max(dat$VARIANCE))) { 
          as.numeric(data.frame(dat %>% group_by(METRIC) %>% transmute(threshold=max(VALUE, na.rm=T)+max(VARIANCE, na.rm=T)+max(VALUE, na.rm=T)/10))[,2])
        } else {
          as.numeric(data.frame(dat %>% group_by(METRIC) %>% transmute(threshold=max(VALUE, na.rm=T)+max(VALUE, na.rm=T)/10))[,2])
        }
          } else {
          0
          }}

        rectvars <- dat %>% distinct(sort2,YEAR) %>% group_by(sort2) %>% transmute(minx=min(as.numeric(YEAR)), xmaxscale=length(YEAR[YEAR<2011]), maxx=max(YEAR))  %>% data.frame()%>% distinct %>% 
          merge(dat %>% distinct(sort2,whitingv))
       

#      if(input$PlotSelectOption=="Standard deviation or Median average deviation")
      dat$upper <- 
        if(input$Ind_sel=="Economic"){
            if(input$AVE_MED=='A'){
                dat$VALUE+dat$VARIANCE
            } else if(input$AVE_MED=='T'){ 
                dat$VALUE
            } else { 
                dat$q75
        }} else if(input$Ind_sel!="Economic"){
            if(input$AVE_MED2=='Average'){
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
            if(input$AVE_MED2=='Average'){
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
          if(input$AVE_MED2=='Average'){
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
          if(input$AVE_MED2=='Average'){
            dat$VALUE-dat$VARIANCE
          } else  { 
            dat$q25
          }}
    }
    
    
    
    yr <- function(){
      return(as.numeric(unique(dat$YEAR)))
    }
    
    groupVar <- "whitingv"


#    colourThirds <- c('Non-whiting vessels'="#253494",'Whiting vessels'="#41b6c4",'All vessels'="#a1dab4")
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
          return("Performance Metrics for West Coast Mothership Vessels")
        } else if(input$Sect_sel == "CP"){
          return("Performance Metrics for West Coast Catcher-Processor Vessels")
        } else if (input$Sect_sel == "FR"){
          return("Performance Metrics for West Coast First Receivers and Shorebased Processors")
      }}

    gv <- function(){
      if(input$LayoutSelect!="Metrics"){
      if(input$Ind_sel=="Economic"){
        if(!input$Sect_sel %in% c("CV","FR")){
          sprintf(paste("Economic measure:", dat$SHORTDESCR[1], "     Statistic: ",  input$StatSelect))
        } else{
          if(input$CategorySelect=="Fisheries"){
          sprintf(paste("Economic measure:", dat$SHORTDESCR[1], "     Statistic: ",  input$StatSelect))
        } else {
          sprintf(paste("Economic measure:", dat$SHORTDESCR[1], "     Statistic: ", input$StatSelect,"    Summed across:", input$inSelect))
        }
      }}#end economic 
        else {
       # if(input$MetricSelect[1]!='Number of vessels'&input$MetricSelect!="Share of landings by state"&input$MetricSelect!='Gini coefficient'&input$MetricSelect!='Herfindahl-Hirschman Index'&input$MetricSelect!='Seasonality'&input$MetricSelect!="Vessel length"){
        if(max(dat$metric_flag)==0){
          if(input$Ind_sel=="Demographic"){
          if(!input$Sect_sel %in% c("CV","FR")){
            sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$demSelect, "   Statistic:", input$AVE_MED2))
           } else{
            if(input$CategorySelect=="Fisheries"){
          sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$demSelect, "   Statistic:", input$AVE_MED2))
        } else {
          sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$demSelect,"   Statistic:", input$AVE_MED2,"    Summed across:", input$inSelect))   
        }}
          } else if (input$Ind_sel=="Social and Regional"){
            if(input$socSelect=="Share of landings by state")  {
              if(input$CategorySelect!="Fisheries"){
                sprintf(paste("Variable:", input$VariableSelect,"     Metric: ", input$socSelect, '  Statistic: Percentage   Summed across:', input$inSelect))
              } else {
                sprintf(paste("Variable:",input$VariableSelect,"     Metric: ", input$socSelect, "  Statistic: Percentage"))   
              } 
            } else {
            if(input$Sect_sel=="CV"&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Fisheries"){
              sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$socSelect,"   Statistic:", input$AVE_MED2,"    Summed across:", input$inSelect))   
             } else {
              sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$socSelect, "   Statistic:", input$AVE_MED2))
             } 
          }}
        }# end normal cases
         else {
           if(input$Ind_sel=="Demographic"){
           if(input$demSelect %in% c("Number of vessels","Number of processors")){
             if(input$Sect_sel=="CV"&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Fisheries"){
               sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$demSelect, '  Statistic: Total     Summed across:', input$inSelect))
             } else {
               sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$demSelect, "  Statistic: Total"))   
             } 
             } else if(input$demSelect=="Vessel length")  {
             if(input$Sect_sel=="CV"&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Fisheries"){
               sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$demSelect, '  Statistic: Average maximum length      Summed across:', input$inSelect))
             } else {
               sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$demSelect, "  Statistic: Average maximum length"))   
             } 
           }
           else {      
           if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Fisheries"){
           sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$demSelect, '  Statistic: Index value',"    Summed across:", input$inSelect))
         } else {
           sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$demSelect, '  Statistic: Index value'))   
         } 
         }}
        else if(input$Ind_sel=="Social and Regional"){
           if(input$socSelect=="Seasonality"){
             if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Fisheries"){
               sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$socSelect, '  Statistic: Day of year  Summed across:', input$inSelect))
             } else {
               sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$socSelect, "  Statistic: Day of year"))   
             } 
           } else if(input$socSelect=="Share of landings by state")  {
             if(input$CategorySelect!="Fisheries"){
               sprintf(paste("Variable:", input$VariableSelect,"     Metric: ", input$socSelect, '  Statistic: Percentage   Summed across:', input$inSelect))
             } else {
               sprintf(paste("Variable:",input$VariableSelect,"     Metric: ", input$socSelect, "  Statistic: Percentage"))   
             } 
           }
           else {      
             if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Fisheries"){
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
          if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Fisheries"){
            sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ",  input$StatSelect,"    Summed across:", input$inSelect))
          } else {
            sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect))
          }
        } else {
          if(max(dat$metric_flag==0)){
          if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Fisheries"){
            sprintf(paste(input$CategorySelect, ":",input$VariableSelect, " Statistic:",  input$AVE_MED2,"  Summed across:", input$inSelect))
          } else {
            sprintf(paste(input$CategorySelect,":",input$VariableSelect,  " Statistic:",  input$AVE_MED2))   
          }
          }
         else {
          if(input$Sect_sel=='CV'&input$CategorySelect!="Fisheries"|input$Sect_sel=='FR'&input$CategorySelect!="Fisheries"){
            sprintf(paste(input$CategorySelect, ":",input$VariableSelect,"  Summed across:", input$inSelect, "  Statistic:", input$AVE_MED2))
          } else {
            sprintf(paste(input$CategorySelect,":",input$VariableSelect, '  Statistic:', input$AVE_MED2))   
          }
          
        }
    }
    }}
    
    main <- function(){
      bquote(atop(.(plot.title()), .(gv())))
    }
    
    ylab <- function(){
      if(input$Ind_sel=="Economic"){
        if(input$StatSelect!='Average per vessel/metric-ton caught'&input$StatSelect!='Median per vessel/metric-ton caught'&input$StatSelect!='Fleet-wide average/metric-ton caught'&
         input$StatSelect!='Average per processor/metric-ton produced'&input$StatSelect!='Median per processor/metric-ton produced'&input$StatSelect!='Industry-wide average/metric-ton produced') {
          expression(paste(bold("Thousands of 2015 $","(",input$StatSelect, ")")))  
      } else {
        expression(paste(bold("2015 $","(",input$StatSelect, ")")))  
        }}else if(input$Ind_sel=="Social and Regional") {
           if(input$LayoutSelect!='Metrics'){
                if(input$socSelect=="Crew wage per day"|input$socSelect=="Revenue per crew day"){
                    expression(paste(bold("Thousands of 2015 $","(",input$AVE_MED2, ")")))    
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
          expression(bold("Proportion of revenue from catch share fishery"))  
        }  else if(input$demSelect=="Gini coefficient"){
          expression(bold("Gini coefficient (0 - 1)"))
        }  else if(input$demSelect=="Fishery participation"){
          expression(bold("Fishery participation (number of fisheries)"))
        }  else if(input$demSelect=="Vessel length"){
          expression(bold("Vessel length (in feet)"))
        }  else {
          input$demSelect     
        }
          }else {expression(bold('Scale and units depend upon metric'))}
    }
    }
    
    supp_obs <- function(){
"\nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality."
    }
    conf_mess <- function(){
"\nSee the confidentiality section under the ABOUT tab for more information."
    }
    
    supp_whiting <- function(){
      if(input$Sect_sel!="FR"){
"\nYour selection would reveal confidential data for years with sufficient observations. 
   For years when confidential data would be revealed, only results for 'All vessels' have been shown."
      } else {
"\nYour selection would reveal confidential data for years with sufficient observations. 
   For years when confidential data would be revealed, only results for 'All processors' have been shown."}
         } 
    
    supp_metric <- function(){
"Some metrics may not be shown because the selected statistic is not calculated for that metric." 
#For the Gini Coefficient, the index value is shown, regardless of the statistic selectd.\n
#   For number of vessels, only the total number of vessels is shown. For seasonality, the day when 50% of catch was landded is always shown.
# \nFor Total Vessel length, we show maximum length. To protect confidentiality this value is the average of the longest three vessels."
    }
    
    xlab <- function(){
      if(input$LayoutSelect!="Metrics"){
        if(input$Ind_sel=="Economic"){
          if(max(dat$conf)==0) {
            if(max(dat$flag)==0){
              ""
            } else {
              paste(supp_obs())
            }} else {
              if(max(dat$flag)==0){
                paste(supp_whiting(), conf_mess())
              }  else {
                paste(supp_obs(), supp_whiting(), conf_mess())
              }}}
        else if(input$Ind_sel=="Demographic"){
          if(input$demSelect=="Fishery participation"|input$demSelect=="Proportion of revenue from CS fishery"){
          if(max(dat$conf)==0) {
            if(max(dat$flag)==0){
              if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
                paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for\n all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".")
              } else {
                ""
              }}else {
                if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
                  paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for\n all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".",
                        supp_obs()) 
                } else {
                  paste(supp_obs())
                }}} else {
                  if(max(dat$flag)==0){
                    if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
                      paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for\n all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".",
                            supp_whiting(), conf_mess())
                    } else{
                      paste(supp_whiting(), conf_mess())
                    }} else {
                      if(input$CategorySelect=="Fisheries"&input$Sect_sel=="CV"){
                        paste("For individual fisheries and the", input$demSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$demSelect,"for\n all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".",
                              supp_obs(), supp_whiting(), conf_mess())
                      } else {
                        paste(supp_obs(), supp_whiting(), conf_mess())
                      } }}
        } else  {
          if(max(dat$conf)==0) {
            if(max(dat$flag)==0){
              ""
            } else {
              paste(supp_obs())
            }} else {
              if(max(dat$flag)==0){
                paste(supp_whiting(), conf_mess())
              }  else {
                paste(supp_obs(), supp_whiting(), conf_mess())
              }}}
        }else if(input$Ind_sel=="Social and Regional"){
          if(input$socSelect=="Share of landings by state"){
          if(max(dat$conf)==0) {
            if(max(dat$flag)==0){
              if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                paste("For the", input$socSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$socSelect,"for vessels that homeported in", input$VariableSelect,".")
              } else {
                ""
              }} else {
                if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                  paste("For the", input$socSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$socSelect,"for vessels that homeported in", input$VariableSelect,".",
                        supp_obs())
                } else {
                  paste(supp_obs())
                }}
          } else {
            if(max(dat$flag)==0){
              if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                paste("For the", input$socSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,"\nFor example, the plots above show the", input$socSelect,"for vessels that homeported in", input$VariableSelect,".",
                      supp_whiting(), conf_mess())
              } else {
                paste(supp_whiting(), conf_mess())
              }}  else {
                if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                  paste("For the", input$socSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$socSelect,"for vessels that homeported in", input$VariableSelect,".",
                        supp_obs(), supp_whiting(), conf_mess())
                } else {
                  paste(supp_obs(), supp_whiting(), conf_mess())
                }} }
        } else {
          if(max(dat$conf)==0) {
            if(max(dat$flag)==0){
              ""
            } else {
              paste(supp_obs())
            }} else {
              if(max(dat$flag)==0){
                paste(supp_whiting(), conf_mess())
              }  else {
                paste(supp_obs(), supp_whiting(), conf_mess())
              }}}
      }} else {
        if(max(dat$conf)==0) {
          if(max(dat$flag)==0){
            if(max(dat$metric_flag)==1){
              paste(supp_metric())
            } else {
              ""
            }
          } else if(max(dat$flag)==1){
            if(max(dat$metric_flag)==1){
              paste(supp_metric(), supp_obs())
            } else {
              paste(supp_obs())
            } 
          }
        } else if(max(dat$conf)==1){
          if(max(dat$flag==0)){
            if(max(dat$metric_flag)==1){
              paste(supp_metric(), supp_obs(), supp_whiting(), conf_mess())
            } else {
              paste(supp_whiting(), conf_mess())
            } 
          } else if(max(dat$flag)==1){
            if(max(dat$metric_flag)==1){
              paste(supp_metric(), supp_whiting(), supp_obs(), conf_mess())
            } else {
              paste(supp_whiting(), supp_obs(), conf_mess())
            } 
          }}
      }
    }#end x label function 
    
 scale_text <- function() {
   if(input$Ind_sel!="Economic"){
#     if (min(dat$YEAR)<2009) {
 #      return(1.2)
#     }  else {
      return(1.1)
#  }
   } else { 
#   b <- table(table(dat$SHORTDESCR)>1)[[1]]
#   if(b == 1 | b ==3) {
     return(1.2)
#   } else {
#     return(1.2)
 #  } 
 }}   
 
 scale_geom_text <- function() {
   if(sum(dat$VALUE, na.rm=T)!=0) {
     return(max(dat$VALUE, na.rm=T))
   } else {
     return(0)
   }
 }
 
# if(input$MetricSelect=="Date 50 percent of total catch landed"){
#    g <-    g <- ggplot(dat, aes_string(x = x, y = as.Date(origin="1970-01-01", y), group = groupVar, order='sort'), environment=environment())#+ scale_y_date(breaks=date_breaks("month"), labels=date_format("%d %m"))
#   } else {
#     dat <- dat[order(dat$sort),]
 g <- ggplot(dat[!is.na(dat$VALUE),], aes_string(x = x, y = y , group = groupVar), environment=environment()) #+coord_cartesian(xlim = c(0, length(table(dat$YEAR))+1))
 #   }

     
     
     
  if(length(yr())>1){
        g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
  } else {
        g <- g + geom_point(aes_string(colour = groupVar), size=4)
  }   
         
   if(input$PlotSelect==T&is.na(max(dat$VARIANCE))==F) { 
           g <- g + geom_ribbon(aes(ymax=upper, ymin=lower, fill=whitingv), alpha=.25)#show.legend = FALSE, 
             } else {
           g <- g
             }

     
#     if(input$PlotSelect==T&dat$STAT[1]!="Fleet-wide total"&is.na(max(dat$VARIANCE))==F) { 
#     g <- g + geom_ribbon(aes(ymax=VALUE+VARIANCE, ymin=VALUE-VARIANCE, fill=whitingv), alpha=.25)#show.legend = FALSE, 
#     } else {
#       g <- g
#     }

#----- define facet -----#
   if (input$LayoutSelect!='Metrics') {
     g <- g + facet_wrap(~ sort2, ncol=2)
   } else {
     g <- g + facet_wrap(~sort2, ncol=2)
     }
   
     # define scale
    g <- g + scale_fill_manual(values = colourThirds) + scale_colour_manual(values = colourThirds) #+ scale_x_discrete('YEAR2', drop=FALSE)

        g <- g + geom_hline(yintercept = 0)
    
#---- define labels ------#
        if(input$tabs=='Panel1'){
    g <- g + labs(y = ylab(), x=xlab(), title = main())   
        } else {
          g <- g + labs(y = ylab(), x='', title = main())   
        }
     
    
    labeltext <- ifelse(input$tabs=='Panel1', 7,5)
#----- Define rectangles and labels ------#
    if(length(yr())>1 & min(yr())<2011 & max(yr())>2010){
      g <- g + geom_rect(aes(xmin=-Inf, xmax=table(yr()<=2010)[[2]]+.5, ymin=-Inf, ymax=Inf), alpha=.05, fill="grey50")
      g <- g + geom_text(aes(x=table(yr()<=2010)[[2]]/3.5,y=max(upper())+scale_geom_text()/20, label="Pre-catch shares", family="serif"),hjust=0,color = "grey20", size=labeltext/scale_text()) 
      if(length(yr()<2010)==6&length(yr()>=2010)<=4){
        g <- g + geom_text(aes(x=table(yr()<=2010)[[2]]+table(yr()>2010)[[2]]/1.5,y=max(upper()+scale_geom_text()/20,label="Post-catch"),hjust=0, family="serif"),color = "grey20", size=labeltext/scale_text())+
          geom_text(aes(x=table(yr()<=2010)[[2]]+table(yr()>2010)[[2]]/1.5,y=max(upper())-max(upper())/100,label="shares"),hjust=0, family="serif",color = "grey20", size=labeltext/scale_text())
      } else {
        g <- g + geom_text(aes(x=table(yr()<=2010)[[2]]+table(yr()>2010)[[2]]/1.5,y=max(upper())+scale_geom_text()/20,label="Post-catch shares"),hjust=0, family="serif",color = "grey20", size=labeltext/scale_text())
      }
      #g <- g + geom_rect(data=rectvars,aes(x=NULL, y=NULL, xmin=-Inf, xmax=xmaxscale+.5, ymin=-Inf, ymax=Inf), alpha=.05, fill="grey50")
      #g <- g + geom_text(data=rectvars,aes(x=xmaxscale/3.5,y=max(upper())+scale_geom_text()/20, label="Pre-catch shares", family="serif"),hjust=0,color = "grey20", size=7/scale_text()) 
      #if(length(yr()<2010)==6&length(yr()>=2010)<=4){
      #  g <- g + geom_text(data=rectvars,aes(x=xmaxscale+table(yr()>2010)[[2]]/1.5,y=max(upper()+scale_geom_text()/20,label="Post-catch"),hjust=0, family="serif"),color = "grey20", size=7/scale_text())+
      #    geom_text(data=rectvars,aes(x=xmaxscale+table(yr()>2010)[[2]]/1.5,y=max(upper())-max(upper())/100,label="shares"),hjust=0, family="serif",color = "grey20", size=7/scale_text())
      #} else {
      #  g <- g + geom_text(data=rectvars,aes(x=xmaxscale+table(yr()>2010)[[2]]/1.5,y=max(upper())+scale_geom_text()/20,label="Post-catch shares"),hjust=0, family="serif",color = "grey20", size=7/scale_text())
      } 
      else {
       g <- g  
      }

    if(input$tabs=='Panel1'){
      strptextsize <- 18
    } else {
      strptextsize <- 14
    }
##############################################################    
    # define theme
##############################################################
    g <- g + theme(
      plot.title = element_text( vjust=1, hjust=.5, size=rel(1.5), colour="grey25", family = "sans", face = "bold"),# 
     # plot.title = element_text(, vjust = 1),
      panel.background = element_rect(fill = "white"),
#      panel.spacing = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans",size = strptextsize, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=rel(1.1),  face="italic", vjust=-1, hjust=.05, colour="grey25"),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", color = "grey25", face = "bold", size = 12),
      legend.title = element_blank()
    #  text = element_text(family="sans", color = "red", size=rel(1.3))
    )
 ############################################################################################################

#################################################################################################################################
    
 
    print(g) 
   
   
   } else plot(0,0,type="n", axes=F, xlab="", ylab="")
#  print(head(dat))
}