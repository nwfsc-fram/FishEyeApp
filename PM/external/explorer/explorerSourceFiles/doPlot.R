doPlot <- function(dat, x, y){
  if(PermitPlot()){
    dat <- subset(dat, is.na(dat$VALUE)==FALSE)

    ######################################################
    

      
#    if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
#      if(input$Ind_sel=="Economic"){
#        temp <-unique(data.frame(dat %>% group_by(SHORTDESCR,whitingv) %>% 
#                                         transmute(y=as.numeric(VALUE[YEAR==max(YEAR)]), VALUE.x=mean(VALUE[YEAR<2011], na.rm=T),
#                                                   VALUE.y=mean(VALUE[YEAR>=2011], na.rm=T), YEAR=max(YEAR))))
#        temp$pchange <- (temp$VALUE.y-temp$VALUE.x)/abs(temp$VALUE.x)
#        temp$y <- ifelse(temp$whitingv=="All vessels", max(dat$VALUE), min(dat$VALUE))
#        temp$y <- ifelse(temp$whitingv=="Non-whiting vessels", (max(dat$VALUE)+min(dat$VALUE))/2, temp$y)
#        dat <- unique(merge(dat, temp[,c("SHORTDESCR","whitingv",'pchange','y','YEAR')], by=c("SHORTDESCR",'whitingv','YEAR'), all.x=T))
#        dat$pchange <- ifelse(is.na(dat$pchange)==T, "", paste(round(dat$pchange,2)*100,'%', sep=""))
        
#        
#      } else if(input$Ind_sel!="Economic"){
#        if(input$MetricSelect=="Share of landings by state"){
#          temp <-unique(data.frame(dat %>% group_by(agid,whitingv) %>% transmute(y=as.numeric(VALUE[YEAR==max(YEAR)]),VALUE.x=mean(as.numeric(VALUE[YEAR<2011]), na.rm=T),
#                                                                                 VALUE.y=mean(as.numeric(VALUE[YEAR>=2011]), na.rm=T),YEAR=max(YEAR))))
#         temp$pchange <- (temp$VALUE.y-temp$VALUE.x)/abs(temp$VALUE.x)
#          temp$y <- ifelse(temp$whitingv=="All vessels", max(dat$VALUE), min(dat$VALUE))
#          temp$y <- ifelse(temp$whitingv=="Non-whiting vessels", (max(dat$VALUE)+min(dat$VALUE))/2, temp$y)
#          dat <- unique(merge(dat, temp[,c("agid","whitingv",'pchange','y','YEAR')], by=c("agid",'whitingv','YEAR'), all.x=T))
#          dat$pchange <- ifelse(is.na(dat$pchange)==T, "", paste(round(dat$pchange,2)*100,'%', sep=""))
#        } else { 
#          temp <-unique(data.frame(dat %>% group_by(VARIABLE,whitingv) %>% transmute(y=as.numeric(VALUE[YEAR==max(YEAR)]),VALUE.x=mean(as.numeric(VALUE[YEAR<2011]), na.rm=T),
#                                                                                           VALUE.y=mean(as.numeric(VALUE[YEAR>=2011]), na.rm=T),YEAR=max(YEAR)))) 
#          temp$pchange <- (temp$VALUE.y-temp$VALUE.x)/abs(temp$VALUE.x)
#          temp$y <- ifelse(temp$whitingv=="All vessels", max(dat$VALUE), min(dat$VALUE))
#          temp$y <- ifelse(temp$whitingv=="Non-whiting vessels", (max(dat$VALUE)+min(dat$VALUE))/2, temp$y)
#          dat <- unique(merge(dat, temp[,c("VARIABLE","whitingv",'pchange','y','YEAR')], by=c("VARIABLE",'whitingv','YEAR'), all.x=T))
#          dat$pchange <- ifelse(is.na(dat$pchange)==T, "", paste(round(dat$pchange,2)*100,'%', sep=""))
#        
#        }}} else {
#          dat$pchange <- ''
#        }
    if(input$Ind_sel=="Economic"){   
      dat$thresh <- data.frame(dat %>% group_by(SHORTDESCR) %>% transmute(threshold=length(table(YEAR[YEAR<=2010]))))[,2]
    }
    
    
    groupVar <- "whitingv"

#    colourThirds <- c('Non-whiting vessels'="#253494",'Whiting vessels'="#41b6c4",'All vessels'="#a1dab4")
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
      if(input$LayoutSelect!="Metrics"){
      if(input$Ind_sel=="Economic"){
        if(input$CategorySelect=="Fisheries"){
          sprintf(paste("Economic measure:", input$SHORTDESCR, "     Statistic: ",  input$StatSelect))
        } else {
          sprintf(paste("Economic measure:", input$SHORTDESCR, "     Statistic: ", input$StatSelect,"    Summed across:", input$inSelect))
        }
      } else {
        if(input$CategorySelect=="Fisheries"){
          sprintf(paste("Category:", input$CategorySelect,"     Metric: ", input$MetricSelect))
        } else {
          sprintf(paste("Category:",input$CategorySelect,"     Metric: ", input$MetricSelect,"    Summed across:", input$inSelect))   
        }
      }
      } else {
        if(input$Ind_sel=="Economic"){
          if(input$CategorySelect=="Fisheries"){
            sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ",  input$StatSelect))
          } else {
            sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect,"    Summed across:", input$inSelect))
          }
        } else {
          if(input$CategorySelect=="Fisheries"){
            sprintf(paste(input$CategorySelect, ":",input$VariableSelect))
          } else {
            sprintf(paste(input$CategorySelect,":",input$VariableSelect,"Summed across:", input$inSelect))   
          }
        }
    }
    }
    
    main <- function(){
      bquote(atop(.(plot.title()), .(gv())))
    }
    
    ylab <- function(){
      if(input$Ind_sel=="Economic") {
        expression(paste(bold("Thousands of 2014 $","(",input$StatSelect, ")")))   
        
      } else if(input$Ind_sel!="Economic") {
        if(input$MetricSelect=="Crew wage per day"|input$MetricSelect=="Revenue per crew day"){
          expression(paste(bold("Thousands of 2014 $","(",input$AVE_MED2, ")")))
          
        } else if(input$MetricSelect=="Proportion of revenue from CS fishery"){
          expression(bold("Proportion of revenue from Catch Share fishery"))  
        }  else if(input$MetricSelect=="Gini coefficient"){
          expression(bold("Gini coefficient (0 - 1)"))
        } else if(input$MetricSelect=="Seasonality"){
          expression(bold("Day of year when 50% of catch was landed"))
        }  else if(input$MetricSelect=="Share of landings by state"){
          expression(bold("Share of landings (% of revenue)"))
        }  else if(input$MetricSelect=="Fishery participation"){
          expression(bold("Fishery participation (number of fisheries)"))
        }  else if(input$MetricSelect=="Vessel length"){
          expression(bold("Vessel length (in feet)"))
        }  else if(input$MetricSelect=="Herfindahl-Hirschman Index"){
          expression(bold("Herfindahl-Hirschman Index (0 - 10,000)"))
        } else {
          input$MetricSelect         
        }
      }
    }
    
    xlab <- function(){
      if(input$MetricSelect=="Fishery participation"|input$MetricSelect=="Proportion of revenue from CS fishery"){
        if(max(dat$conf)==0) {
          if(max(dat$flag)==0){
            if(input$CategorySelect=="Fisheries"){
            paste("For individual fisheries and the", input$MetricSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$MetricSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".")
            } else {
            "" 
            }}else {
            if(input$CategorySelect=="Fisheries"){
              paste("For individual fisheries and the", input$MetricSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$MetricSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".
              \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality.") 
            } else {
              "Data have been sueppressed for years that are not plotted as there are not enough observations to protect confidentiality."        
         }}} else {
           if(max(dat$flag)==0){
             if(input$CategorySelect=="Fisheries"){
             paste("For individual fisheries and the", input$MetricSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$MetricSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".
             \nYour selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
              only results for 'All vessels' are shown. 
             \nSee the confidentiality section under the ABOUT tab for more information.")
              } else{
                "Your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
                 only results for 'All vessels' are shown. 
               \nSee the confidentiality section under the ABOUT tab for more information."
          }} else {
            if(input$CategorySelect=="Fisheries"){
              paste("For individual fisheries and the", input$MetricSelect, "metric, we show all activities for vessels that fished in the selected fisheries, \nnot just their activity in the selected fishery, \nFor example, the", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {""},"plot above shows the", input$AVE_MED2, input$MetricSelect,"for all vessels that fished for", if(length(input$VariableSelect)>2){input$VariableSelect[3]} else if(length(input$VariableSelect)==2) {input$VariableSelect[2]} else {input$VariableSelect[1]},".
            \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. 
           \nIn addition, your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
             only results for 'All vessels' are shown. 
           \nSee the confidentiality section under the ABOUT tab for more information.")
          } else {
            "Data have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. 
            \nIn addition, your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
             only results for 'All vessels' are shown. 
           \nSee the confidentiality section under the ABOUT tab for more information."
          } }}
        } 
        else if(input$MetricSelect=="Share of landings by state"){
          if(max(dat$conf)==0) {
            if(max(dat$flag)==0){
              if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
              paste("For the", input$MetricSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$MetricSelect,"for vessels that homeported in", input$VariableSelect,".")
              } else {
                ""
              }} else {
                 if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                   paste("For the", input$MetricSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$MetricSelect,"for vessels that homeported in", input$VariableSelect,".
                  \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality." )
                 } else {
                   "Data have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality." 
                 }}
            } else {
              if(max(dat$flag)==0){
              if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                paste("For the", input$MetricSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,"\nFor example, the plots above show the", input$MetricSelect,"for vessels that homeported in", input$VariableSelect,".
                \nYour selection would reveal confidential data for years with sufficient observations.  For years when confidential data would be revealed, 
                 only results for 'All vessels' have been shown. 
               \nSee the confidentiality section under the ABOUT tab for more information.")
              } else {
                "Your selection would reveal confidential data for years with sufficient observations.  For years when confidential data would be revealed, 
                 only results for 'All vessels' have been shown. 
                \nSee the confidentiality section under the ABOUT tab for more information."
              }}  else {
                    if(input$CategorySelect=="State"|input$CategorySelect=="Homeport"){
                  paste("For the", input$MetricSelect, "metric, we show all activities for vessels that homeported in the selected",input$CategorySelect,", \nnot just their activity in the selected",input$CategorySelect,".\nFor example, the plots above show the", input$MetricSelect,"for vessels that homeported in", input$VariableSelect,".
                \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. 
                \nIn addition, your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
                 only results for 'All vessels' have been shown. 
                \nSee the confidentiality section under the ABOUT tab for more information.")
                    } else {
                    "Data have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. 
                    \nIn addition, your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
                     only results for 'All vessels' have been shown. 
                   \nSee the confidentiality section under the ABOUT tab for more information."
              }} }
          } else {
      if(max(dat$conf)==0) {
          if(max(dat$flag)==0){
            ""
          } else {
          "Data have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality." 
          }} else {
          if(max(dat$flag)==0){
          "Your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
           only results for 'All vessels' are shown. 
        \nSee the confidentiality section under the ABOUT tab for more information."
            }  else {
          "Data have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. 
          \nIn addition, your selection would reveal confidential data for years with sufficient observations. For years when confidential data would be revealed, 
           only results for 'All vessels' are shown. 
        \nSee the confidentiality section under the ABOUT tab for more information."
         }}}
    } #end x label function
    
   
    
 scale_text <- function() {
   if(input$Ind_sel!="Economic"){
     if (min(input$YearSelect)<2009) {
       return(1.2)
     }  else {
      return(1.1)
  }} else { 
   b <- table(table(dat$SHORTDESCR)>1)[[1]]
   if(b == 1 | b ==3) {
     return(1.3)
   } else {
     return(1.4)
   } 
 }}   
 
 
# if(input$MetricSelect=="Date 50 percent of total catch landed"){
#    g <-    g <- ggplot(dat, aes_string(x = x, y = as.Date(origin="1970-01-01", y), group = groupVar, order='sort'), environment=environment())#+ scale_y_date(breaks=date_breaks("month"), labels=date_format("%d %m"))
#   } else {
#     dat <- dat[order(dat$sort),]
     g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment=environment()) #, order='sort'+coord_cartesian(xlim = c(0, length(table(dat$YEAR))+1))
#   }

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
           g <- g + geom_text(aes(x=dat$thresh/3.5,y=max(VALUE+VARIANCE)+max(VALUE)/10, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=7/scale_text()) 
           if(length(table(dat$YEAR[dat$YEAR<2010]))==6&length(table(dat$YEAR[dat$YEAR>=2010]))<=4){
             g <- g + geom_text(aes(x=dat$thresh+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+VARIANCE)+max(VALUE)/10,label="Post-Catch"),hjust=0, family="serif",color = "grey20", size=7/scale_text())+
                      geom_text(aes(x=dat$thresh+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+VARIANCE)-max(VALUE+VARIANCE)/100,label="shares"),hjust=0, family="serif",color = "grey20", size=7/scale_text())
           } else {
             g <- g + geom_text(aes(x=dat$thresh+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+VARIANCE)+max(VALUE)/10,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=7/scale_text())
           }
                   } else {
           g <- g + geom_text(aes(x=dat$thresh[1]/3.5,y=max(VALUE+0)+max(VALUE)/10, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=7/scale_text()) 
             if(length(table(dat$YEAR[dat$YEAR<2010]))==6&length(table(dat$YEAR[dat$YEAR>=2010]))<=4){
               g <- g + geom_text(aes(x=dat$thresh[1]+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+0)+max(VALUE)/10,label="Post-Catch"),hjust=0, family="serif",color = "grey20", size=7/scale_text())+
               geom_text(aes(x=dat$thresh[1]+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+0)-max(VALUE)/100,label="shares"),hjust=0, family="serif",color = "grey20", size=7/scale_text())
             } else {
               g <- g + geom_text(aes(x=dat$thresh[1]+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+0)+max(VALUE)/10,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=7/scale_text())
         }}
      } 
      else {
        g <- g + geom_rect(aes(xmin=-Inf, xmax=length(table(dat$YEAR[dat$YEAR<=2010])), ymin=-Inf, ymax=Inf),fill="grey50", alpha=.02)
         if(input$PlotSelect==T&dat$STAT[1]!="Fleet-wide total"&is.na(max(dat$VARIANCE))==F) {  
         g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))/3.5,y=max(VALUE+VARIANCE)+max(VALUE)/10, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=7/scale_text())  
           if(length(table(dat$YEAR[dat$YEAR<2010]))==6&length(table(dat$YEAR[dat$YEAR>=2010]))<=3){
                    g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+VARIANCE)+max(VALUE)/10,label="Post-Catch"),hjust=0, family="serif",color = "grey20", size=7/scale_text())+
                             geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+VARIANCE)-max(VALUE+VARIANCE)/10,label="shares"),hjust=0, family="serif",color = "grey20", size=7/scale_text())
           } else {
                   g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+VARIANCE)+max(VALUE)/10,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=7/scale_text())
                     }
                    } else {
         g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))/3.5,y=max(VALUE+0)+max(VALUE)/10, label="Pre-Catch shares", family="serif"),hjust=0,color = "grey20", size=7/scale_text())  
           if(length(table(dat$YEAR[dat$YEAR<2010]))>3&length(table(dat$YEAR[dat$YEAR>=2010]))==2) {
             g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+0)+max(VALUE)/10,label="Post-Catch"),hjust=0, family="serif",color = "grey20", size=7/scale_text())+
                      geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+0)-max(VALUE)/10,label="shares"),hjust=0, family="serif",color = "grey20", size=7/scale_text())
           } else {
           g <- g + geom_text(aes(x=length(table(dat$YEAR[dat$YEAR<2011]))+length(table(dat$YEAR[dat$YEAR>2010]))/2,y=max(VALUE+0)+max(VALUE)/10,label="Post-Catch shares"),hjust=0, family="serif",color = "grey20", size=7/scale_text())
           }
             } 
      }} else {
       g <- g  
      }
   g <- g + geom_text(aes(label=star), colour="black", vjust=0, size=10)
   

    # define facet
 #   if(input$LayoutSelect!="Metrics"){
     g <- g + facet_wrap(~ sort, ncol=2, as.table = TRUE)
 # } else {
 #     if(input$Ind_sel=="Economic"){
 #     g <- g + facet_wrap(~SHORTDESCR, scales="free_x")#(~), sortsortas.table=TRUE,
 #   } else if(input$MetricSelect=="Share of landings by state"){
 #      g <- g + facet_wrap(~ agid, ncol=2, as.table = TRUE)#, scales="free_x"
#     } else {
#       g <- g + facet_wrap(~ METRIC, ncol=2, as.table=TRUE)
#     } #, scales="free_x"
#    }
     # define scale
    g <- g + scale_fill_manual(values = colourThirds) + scale_colour_manual(values = colourThirds)

#Pchange labels
#      if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
#        if(input$Ind_sel!="Economic"){
#     g <- g+geom_text(aes(length(table(input$YearSelect))+.4,y,label=pchange, group=whitingv, color=factor(whitingv)), size=4,fontface="bold")#length(table(YEAR))+
#        } else {
#          g <- g+geom_text(aes(.8,y,label=pchange, group=whitingv, color=factor(whitingv)), size=4,fontface="bold")#length(table(YEAR))+
#        }
#    } else {
#      g <- g
#    }

    #,data=dat[which(dat$YEAR==max(dat$YEAR)),]  
    # define solid line y=0
    g <- g + geom_hline(yintercept = 0)
    
    # define labels
    g <- g + labs(y = ylab(), x=xlab(), title = main())   

#     if(input$MetricSelect=="Date 50 percent of total catch landed"){
#        g <-    g <- g+ scale_y_date(breaks=date_breaks("month"), labels=date_format("%d %m"))
#       } else {
#         g <- g
 #      }
    
    # define theme
    g <- g + theme(
      plot.title = element_text( vjust=1, size=rel(1.5), colour="grey25", family = "sans", face = "bold"),# 
     # plot.title = element_text(, vjust = 1),
      panel.background = element_rect(fill = "white"),
      #panel.margin = unit(1.2, "lines"),
      plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", 
                                size = 18, color = "grey25", vjust=1),
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
    
    #function to remove dots infront of labels but 
#    label_mod <- function(variable, value) {
#      gsub("([.])", "\\ ", as.character(value))
      #    lapply(strwrap(as.character(value), width=25, simplify=FALSE), 
      #           paste, collapse="\n")
#    }   
    
#################################################################################################################################
    
    g <- strwrap_strip_text(g)
    print(g) 
   
   
   } else plot(0,0,type="n", axes=F, xlab="", ylab="")
}