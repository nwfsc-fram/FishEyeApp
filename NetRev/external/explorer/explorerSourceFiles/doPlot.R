#https://github.com/hadley/ggplot2/issues/1301  #use website for dealing with stacked bar plot order issue
doPlot <- function(dat, x, y, type){
  if(PermitPlot()){
    
   
    groupVar <- ifelse(type=="summary", "SHORTDESCR", "THIRDS")
    
    ## Change color palette to printer-friendly colors that are color-blind friendly. Want consistent colors with what Erin is using
    colourList <- c('Revenue'="#256D36",'Variable costs'="#fed25d",'Fixed costs'="#fca836",'Variable Cost Net Revenue'="#4B958D", 'Total Cost Net Revenue'="#4575B4")
    colourThirds <- c('Top third'="#253494",'Middle third'="#41b6c4",'Lower third'="#a1dab4")

    sect <- function(){
      if(input$Sect_sel == "CV"){
        return("Catcher Vessels")
      } else if(input$Sect_sel == "M"){
        return("Motherships")
      } else if(input$Sect_sel == "CP"){
        return("Catcher Processors")
      } else if (input$Sect_sel == "FR"){
        return("First Receivers")
      }
      }
    
     yr <- function(){
     if(input$Sect_sel == "CV"){
    return(as.numeric(input$YearSelect))  
    } else {
    return(as.numeric(input$YearSelect2))
   }
     }
#     print(yr())
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
      bquote(atop(.(plot.title()), atop(.(gv()), .(sv()))))  #bquote splits subtitle into two lines
    }
    
    
    # simple scaling for bar charts based on number of inputs
    scale_bars <- function(){
        b = length(yr())  
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
    
# Scaling of "Pre and Post catch shares" text based on number of variables selected    
    scale_text <- function() {
      if(type == "summary"){
      if(input$CategorySelect =="Fisheries" | input$CategorySelect == "Homeport") {
        b <- table(table(dat$VARIABLE)>1)[[1]]
        if(b >= 5 & b <=11){
          return(1.3)
        } else if(b<5 | b == 12){
          return(1.45)
        } 
        } else {
        return(1.3)
      }
    } else {  
      b <- table(table(dat$SHORTDESCR)>1)[[1]]
      if(b == 2 | b ==5) {
        return(1.4)
      } else {
        return(1.4)
      } 
    }}   
    
# Beging ggplot    
    g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment=environment())
    if(type == "summary"){    
      if(input$DodgeSelect == "Economic measures side-by-side"){
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
         
        if(length(unique(dat$YEAR))>1 & min(dat$YEAR)<2011 & max(dat$YEAR)>2010){
                g <- g + geom_rect(aes_string(xmin=-Inf, xmax=table(unique(dat$YEAR)<2011)[[2]]+.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(unique(dat$YEAR)))
                g <- g + geom_text(aes(x=(table(unique(dat$YEAR)<2011)[[2]])/4,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000, label="Pre-Catch shares"), family="serif",fontface="italic", hjust=0,color = "grey40", size=7/scale_text()) 
                g <- g + geom_text(aes(x=table(unique(dat$YEAR)<2011)[[2]]+table(unique(dat$YEAR)>2010)[[2]]/1.5,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000,label="Post-Catch shares"),hjust=0, 
                          family = "serif", fontface="italic", color = "grey40", size=7/scale_text())  
            } else {
              g <- g  
            }
      } # end economics measure side by side plots
    
   
      if(input$DodgeSelect != "Economic measures side-by-side"){          
#        dat <- dat[order(dat$SHORTDESCR),]

#        g <- ggplot(dat, aes_string(x = x, y = y, order = 'SHORTDESCR'))+ # 
#          geom_bar(stat = "identity", position = "stack", width = scale_bars(), aes_string(order = 'SHORTDESCR', fill = 'SHORTDESCR'))
        g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="stack", width = scale_bars())
        
            if(length(yr())>1 & min(yr())<2011 & max(yr())>2010){
            if(yr()[2]==2010){
              if(table(yr()>2010)[[2]]==1){
                g <- g + geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                  geom_text(aes(x=.4,y=max(VALUE,na.rm=T)/500, label="Pre-Catch shares"),family="serif",fontface="italic",hjust=0, size=7/scale_text(), color="grey40") + 
                  geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1,y=max(VALUE,na.rm=T)/500,label="Post-Catch shares"),family="serif",fontface="italic",hjust=1, size=7/scale_text(), color="grey40")# +
              } else {
                g <- g + geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                  geom_text(aes(x=.3,y=max(VALUE,na.rm=T)/500, label="Pre-Catch shares"),family="serif",fontface="italic",hjust=0, size=7/scale_text(), color="grey40") + 
                  geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1.5,y=max(VALUE,na.rm=T)/500,label="Post-Catch shares"),family="serif",fontface="italic",hjust=0, size=7/scale_text(), color="grey40")# +
              }
            } else  {
              if(table(yr()>2010)[[2]]==1){  
                g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                  geom_text(aes(x=.25,y=max(VALUE,na.rm=T)/500, label="Pre-Catch shares"),family="sans",fontface="italic",hjust=0, size=7/scale_text(), color="grey40") + 
                  geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1,y=max(VALUE,na.rm=T)/500,label="Post-Catch shares"),family="serif",fontface="italic",hjust=1, size=7/scale_text(), color="grey40") #+
              } else {
                g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(yr()))+ 
                  geom_text(aes(x=.15,y=max(VALUE,na.rm=T)/500, label="Pre-Catch shares"),family="sans",fontface="italic",hjust=0, size=7/scale_text(), color="grey40") + 
                  geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1.5,y=max(VALUE,na.rm=T)/500,label="Post-Catch shares"),family="serif",fontface="italic",hjust=0, size=7/scale_text(), color="grey40") #+
              }
            }}   else {
              g <- g 
            }
        }#end net revenue figures
      g <- g + geom_text(aes(label=star), colour="black", vjust=0, size=10)
      }# end Summary loop for total and variable revenue figures
    
    else if(type != "summary"){    # Begin Variability analysis figure
      if(length(yr()) > 1){
        g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
        if(length(yr())>1 & min(yr())<2011 & max(yr())>2010){
          g <- g + geom_rect(aes_string(xmin=-Inf, xmax=table(yr()<2011)[[2]]+.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(yr()))
          g <- g + geom_text(aes(x=table(yr()<2011)[[2]]/2,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000, label="Pre-Catch shares"), family="serif",fontface="italic",hjust=0,color = "grey40", size=7/scale_text()) + 
                   geom_text(aes(x=table(yr()<2011)[[2]]+table(yr()>2010)[[2]]/1.5,y=max(VALUE,na.rm=T)/1000+max(VALUE,na.rm=T)/10000,label="Post-Catch shares"),fontface="italic",hjust=0, family="serif",color = "grey40", size=7/scale_text()) #+
              } else {
              g <- g 
            }
        } else{
              g <- g + geom_point(aes_string(colour =groupVar), size=4)
            }
#      g <- g + geom_text(aes(label=star), colour="black", vjust=0, size=10)
      
    } # end variability figure
    
    # define facet
    if(type =="summary"){
      g <- g + facet_wrap(~sort, ncol=2, as.table = TRUE, scales="free_x")#
    } else {
      g <- g + facet_wrap(~sort, scales="free_x")#(~SHORTDESCR)
    }
    
    
    # define colors and order
    if(type == "summary") {
      if(input$DodgeSelect == "Economic measures side-by-side"){
        g <- g + scale_fill_manual(values = colourList, guide=guide_legend(reverse=F)) + 
          scale_colour_manual(values = colourList, guide=guide_legend(reverse=F))}
      else {
        g <- g + scale_fill_manual(values = colourList, guide=guide_legend(reverse=T)) + 
          scale_colour_manual(values = colourList)}
      
    } # end summary plots 
    else {
      g <- g + scale_fill_manual(values = colourThirds) + scale_colour_manual(values = colourThirds)
    }  #end thirds plots
    
    
    # define solid line y=0
    g <- g + geom_hline(yintercept = 0)
    
    # define x- and y-axis labels
    ylab <- function(){
      if(input$Sect_sel=='FR'){
        paste("Thousands of 2014 $", "(",input$StatSelect, ")")
      } else {
        paste("Thousands of 2015 $", "(",input$StatSelect, ")")
      }
    }
    
     supp_obs <- function(){
      "\nData has been suppressed for years that are not plotted as there are not enough observations to protect confidentiality."
    }
    conf_mess <- function(){
      if(input$Sect_sel=="CV"){
        "\nNOTE: Your selection would reveal confidential data for years with sufficient observations.  The results shown may include both vessels that fished in Alaska and those that \nfished for Pacific whiting. See the confidentiality section under the ABOUT tab for more information."
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
            paste(thirds_mess(),supp_obs())
          } else {
            paste(thirds_mess(),supp_obs(), conf_mess())
          }} else {
            if(max(dat$AK_FLAG, na.rm=T)==0){
              paste(thirds_mess()) 
            } else {
              paste(thirds_mess(), conf_mess())
            }
          }
      } else {
        if(max(dat$flag)==1) {
          if(max(dat$AK_FLAG, na.rm=T)==0){
            paste(suff_flag())
          }  else {
            paste(suff_flag(),conf_mess())
          }
        }
        else if(max(dat$AK_FLAG, na.rm=T)==1){
          paste(conf_mess())
        } else {
          ""      
        }}
    }
    
    g <- g + labs(y=ylab(), x=xlab(), title=main())
    
              
    # define theme
    g <- g + theme(
      plot.title = element_text( vjust=1, size=rel(1.5), colour="grey25", family = "sans", face = "bold"),# 
      panel.background = element_rect(fill = "white"),
      plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", size = 16, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=rel(1.1),  face="italic", vjust=-1, hjust=-.05, colour="grey25"),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", color = "grey25", face = "bold", size = 12),
      legend.title = element_blank()
    )
    ############################################################################################################
    ##function to wrapping facet labels and remove dots before facet labels 
    strwrap_strip_text = function(p, pad=0.02) { 
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
      sum = 0#sum(sapply(grobs$width, function(x) convertWidth(x, "in")))##
      panels_width = par("din")[1] - sum  # inches
      # determine strwrap width
      panel_width = panels_width / cols +.5
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
    #################################################################################################################################
    
    g <- strwrap_strip_text(g)
    
    print(g)
    
    } else plot(0,0,type="n", axes=F, xlab="", ylab="")
}