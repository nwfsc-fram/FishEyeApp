#https://github.com/hadley/ggplot2/issues/1301  #use website for dealing with stacked bar plot order issue
doPlot <- function(dat, x, y, type){
  if(PermitPlot()){
    
    
    groupVar <- ifelse(type=="summary", "SHORTDESCR", "THIRDS")
    
    ## Change color palette to printer-friendly colors that are color-blind friendly. Want consistent colors with what Erin is using
    colourList <- c('Revenue'="#256D36",'Variable costs'="#fed25d",'Fixed costs'="#fca836",'Variable Cost Net Revenue'="#4B958D", 'Total Cost Net Revenue'="#4575B4")
    colourThirds <- c('Top third'="#253494",'Middle third'="#41b6c4",'Lower third'="#a1dab4")

   # Plot title construction
     plot.title <- function(){
      if(type == "summary"){
        if(input$DodgeSelect == "Economic measures side-by-side"){
          return("Summary Economic Measures for West Coast Catcher Vessels")
        } else if(input$DodgeSelect == "Composition of Total Cost Net Revenue"){
          return("Composition of Total Cost Net Revenue for West Coast Catcher Vessels")
        } else if(input$DodgeSelect == "Composition of Variable Cost Net Revenue"){
          return("Composition of Variable Cost Net Revenue for West Coast Catcher Vessels")
        }#}
      } else {
        return("Variability Analysis of West Coast Catcher Vessels")
      }}
    
    gv <- function(){
      if(type == "summary"){
        if(input$CategorySelect=="Fisheries"){
          sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect, "    Fished in AK included:", input$FishAkSelect, "    Fished for whiting included:", input$FishWhitingSelect))
        } else {
          sprintf(paste("Group variable:", input$CategorySelect, "     Statistic: ", input$StatSelect, "    Fished in AK included:", input$FishAkSelect,"    Fished for whiting included:", input$FishWhitingSelect,"    Summed across:", input$inSelect))
        }
      } else {
        if(input$CategorySelect=="Fisheries"){
          sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect, "    Fished in AK included:", input$FishAkSelect,"    Fished for whiting included:", input$FishWhitingSelect))
        } else {
          sprintf(paste(input$CategorySelect, ":", input$VariableSelect, "     Statistic: ", input$StatSelect, "    Fished in AK included:", input$FishAkSelect,"    Fished for whiting included:", input$FishWhitingSelect,"    Summed across:", input$inSelect))   
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
      bquote(atop(.(plot.title()), atop(.(gv()), .(sv()))))  #bquote splits subtitle into two lines
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
    
# Scaling of "Pre and Post catch shares" text based on number of variables selected    
    scale_text <- function() {
      if(type == "summary"){
      if(input$CategorySelect =="Fisheries" | input$CategorySelect == "Homeport") {
        b <- table(table(dat$VARIABLE)>1)[[1]]
        if(b == 10 | b ==9 ) {
          return(1)
        } else if(b == 8){
          return(1.2)
        } else if(b >= 5 & b < 8){
          return(1.3)
        } else if(b<5 | b == 12){
          return(1.65)
        } else {
          if(b==11) {
            return(1)
          }
        }
      } else {
        return(1.2)
      }
    } else {  
      b <- table(table(dat$SHORTDESCR)>1)[[1]]
      if(b == 2 | b ==5) {
        return(1.8)
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
         
        if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
                g <- g + geom_rect(aes_string(xmin=-Inf, xmax=length(table(input$YearSelect[input$YearSelect<2011]))+.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))
              #  g <- g + geom_text(aes(x=length(table(input$YearSelect[input$YearSelect<2011]))/2,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-Catch shares", family="sans"),hjust=0,color = "grey20", size=7/scale_text()) 
                g <- g + geom_text(aes(x=length(table(input$YearSelect[input$YearSelect<2011]))+length(table(input$YearSelect[input$YearSelect>2010]))/2,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-Catch shares"),hjust=0, 
                          fontface=1, family = "sans", color = "grey20", size=7/scale_text())  
            } else {
              g <- g  
            }
      } # end economics measure side by side plots
    
   
      if(input$DodgeSelect != "Economic measures side-by-side"){          
        dat <- dat[order(dat$SHORTDESCR),]

        g <- ggplot(dat, aes_string(x = x, y = y, order = 'SHORTDESCR'))+ # 
          geom_bar(stat = "identity", position = "stack", width = scale_bars(), aes_string(order = 'SHORTDESCR', fill = 'SHORTDESCR'))
        
        if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
                g <- g + geom_rect(aes_string(xmin=-Inf, xmax=length(table(dat$YEAR[dat$YEAR<=2010]))+.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))
                g <- g + geom_text(aes(x=length(input$YearSelect[input$YearSelect<2011])/2,y=max(VALUE)/400+max(VALUE[VALUE!=max(VALUE)])/400, label="Pre-Catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text()) + 
                  geom_text(aes(x=length(table(input$YearSelect[input$YearSelect<2011]))+length(table(input$YearSelect[input$YearSelect>2010]))/2,y=max(VALUE)/400+max(VALUE[VALUE!=max(VALUE)])/400,label="Post-Catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text())# +
               } else {
              g <- g
            } #end if statement for text and greyed block
      }#end net revenue figures
      g <- g + geom_text(aes(label=star), colour="black", vjust=0, size=10)
      }# end Summary loop for total and variable revenue figures
    
    if(type != "summary"){    # Begin Variability analysis figure
      if(length(input$YearSelect) > 1){
        g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
        if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
          g <- g + geom_rect(aes(xmin=-Inf, xmax=length(table(dat$YEAR[dat$YEAR<=2010]))+.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))
          g <- g + geom_text(aes(x=length(table(input$YearSelect[input$YearSelect<2011]))/2,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-Catch shares", family="sans"),hjust=0,color = "grey20", size=7/scale_text()) + 
                geom_text(aes(x=length(table(input$YearSelect[input$YearSelect<2011]))+length(table(input$YearSelect[input$YearSelect>2010]))/2,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-Catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text()) #+
              } else {
              g <- g 
            }} else{
              g <- g + geom_point(aes_string(colour =groupVar), size=4)
            }
#      g <- g + geom_text(aes(label=star), colour="black", vjust=0, size=10)
      
    } # end variability figure
    
    # define facet
    if(type =="summary"){
      g <- g + facet_wrap(~ sort, ncol=2, as.table = TRUE, scales="free_x")#
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
    if(type!="summary"){
      if(max(dat$con_flag)==1){
        if(max(dat$AK_FLAG, na.rm=T)==0){
        g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x="  Vessels are grouped into three tiered categories: top, middle, and lower earners based on revenue. This is done for each year separately.
                      \nData has been suppressed for years that are not plotted as there are not enough observations to protect confidentiality.", title = main()) 
      } else {
        g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x=" Vessels are grouped into three tiered categories: top, middle, and lower earners based on revenue. This is done for each year separately.  
                          \nData has been suppressed for years that are not plotted as there are not enough observations to protect confidentiality.  
                          \nNOTE: In addition, your selection would reveal confidential data for years with sufficient observations.  The results shown may include both vessels that fished in Alaska and those that \nfished for Pacific whiting. See the confidentiality section under the ABOUT tab for more information.", title = main())        
      }} else {
      
      if(max(dat$AK_FLAG, na.rm=T)==0){
        g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x="  Vessels are grouped into three tiered categories: top, middle, and lower earners based on revenue. This is done for each year separately.", title = main()) 
      } else {
        g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x=" Vessels are grouped into three tiered categories: top, middle, and lower earners based on revenue. This is done for each year separately.  
                          \nNOTE: Unfortunately, your selection would reveal confidential data.  The results shown may include both vessels that fished in Alaska and those that \nfished for Pacific whiting. See the confidentiality section under the ABOUT tab for more information.", title = main())        
      }
      }}
    else {
        if(max(dat$flag)==1) {
             if(max(dat$AK_FLAG, na.rm=T)==0){
            g <- g + labs(y = paste("Thousands of 2014 $","(",input$StatSelect, ")"), x=paste("* Data has been suppressed for this selected",input$CategorySelect, "and year as there are not enough observations to protect confidentiality."), title = main())   
          }  else {
            g <- g + labs(y = paste("Thousands of 2014 $","(",input$StatSelect, ")"), x=
                            paste("* Data has been suppressed for this selected",input$CategorySelect, "and year as there are not enough observations to protect confidentiality.
                          \nNOTE: In addition, your selection would reveal confidential data for years with sufficient observations.  The results shown may include both vessels that fished in Alaska and those that \nfished for Pacific whiting. See the confidentiality section under the ABOUT tab for more information."), title = main())        
          }
           }
        else if(max(dat$AK_FLAG, na.rm=T)==1){
            g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x="NOTE: Unfortunately, your selection would reveal confidential data.  The results shown may include both vessels that fished in Alaska and those that \nfished for Pacific whiting. See the confidentiality section under the ABOUT tab for more information.", title = main())       
          } else {
            g <- g + labs(y = paste("Thousands of 2014 $", "(",input$StatSelect, ")"), x="", title = main())       
            
        }}

    
    # define theme
    g <- g + theme(
      plot.title = element_text( vjust=1, size=rel(1.5), colour="grey25", family = "sans", face = "bold"),# 
      panel.background = element_rect(fill = "white"),
      plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", size = 18, color = "grey25", vjust=1),
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
    #################################################################################################################################
    
    g <- strwrap_strip_text(g)
    
    print(g)
    
    } else plot(0,0,type="n", axes=F, xlab="", ylab="")
}