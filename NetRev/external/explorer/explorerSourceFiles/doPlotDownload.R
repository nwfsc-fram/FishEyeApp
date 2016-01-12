doPlotDownload <- function(dat, x, y, type){
  if(PermitPlot()){
    
    groupVar <- ifelse(type=="summary", "SHORTDESCR", "THIRDS")
    facetVar <- ifelse(type== "summary" , "VARIABLE", "SHORTDESCR")
    # groupVar2 <-  factor(c("Variable costs","Fixed costs","Total cost net revenue"))

    ## Change color palette to printer-friendly colors that are color-blind friendly. Want consistent colors with what Erin is using
    #  colourList <- c("#d73027","#fee090","#91bfdb","#fc8d59", "#4575b4")
 #   colourList <- c(Revenue="#d73027",'Variable costs'="#fee090", 'Total cost net revenue'="#4575b4",'Variable cost net revenue'="#fc8d59",'Fixed costs'="#91bfdb")
    colourThirds <- c('Top third'="#253494",'Middle third'="#41b6c4",'Lower third'="#a1dab4")
#    colourList <- c('Revenue'="#256D36",'Variable costs'="#FEE090",'Fixed costs'="#FDBE68",'Variable cost net revenue'="#54A69D", 'Total cost net revenue'="#4575B4")
#    colourList <- c('Revenue'="#256D36",'Variable costs'="#FEE090",'Fixed costs'="#FDBE68",'Variable cost net revenue'="#4B958D", 'Total cost net revenue'="#4575B4")
#colourList <- c('Revenue'="#256D36",'Variable costs'="#fed977",'Fixed costs'="#fdb34f",'Variable cost net revenue'="#4B958D", 'Total cost net revenue'="#4575B4")
colourList <- c('Revenue'="#256D36",'Variable costs'="#fed25d",'Fixed costs'="#fca836",'Variable cost net revenue'="#4B958D", 'Total cost net revenue'="#4575B4")

    
    plot.title <- function(){
      if(type == "summary"){
        if(input$DodgeSelect == "Economic measures side-by-side"){
          return("Summary Economic Measures for West Coast Catcher Vessels")
        } else if(input$DodgeSelect == "Composition of total cost net revenue"){
          return("Composition of Total Cost Net Revenue for West Coast Catcher Vessels")
        } else if(input$DodgeSelect == "Composition of variable cost net revenue"){
          return("Composition of Variable Cost Net Revenue for West Coast Catcher Vessels")
        }#}
      } else {
        return("Variability Analysis of West Coast Catcher Vessels")
      }}
 
    gv <- function(){
      if(type == "summary"){
        if(input$CategorySelect=="Fisheries"){
          sprintf(paste("Group variable:", input$CategorySelect, " Statistic: ", input$StatSelect, " Fished in AK included:", input$FishAkSelect, " Fished for whiting included:", input$FishWhitingSelect))
        } else {
          sprintf(paste("Group variable:", input$CategorySelect, "Statistic: ", input$StatSelect, "Fished in AK included:", input$FishAkSelect, "Fished for whiting included:", input$FishWhitingSelect,"Summed across:", input$inSelect))
        }
      } else {
        if(input$CategorySelect=="Fisheries"){
          sprintf(paste(input$CategorySelect, ":", input$VariableSelect, " Statistic: ", input$StatSelect, "Fished in AK included:", input$FishAkSelect, " Fished for whiting included:", input$FishWhitingSelect))
        } else {
          sprintf(paste(input$CategorySelect, ":", input$VariableSelect, " Statistic: ", input$StatSelect, " Fished in AK included:", input$FishAkSelect, " Fished for whiting included:", input$FishWhitingSelect," Summed across:", input$inSelect))   
        }
      }
    }
    
    #   sv <- function(){
    #    sprintf(paste())
    #   } 
    sv <- function(){
      if(type == "summary"){
        if(input$DodgeSelect == "Composition of total cost net revenue"){
          return("Total cost net revenue = Revenue - Variable costs - Fixed costs")
        } else if(input$DodgeSelect == "Composition of variable cost net revenue"){
          return("Variable cost net revenue = Revenue - Variable costs")
        } else {
          return()
        }
      } else {
        return()
      }
    }
    
    main <- function(){
      bquote(atop(.(plot.title()), atop(.(gv()), .(sv()))))
      #    if(type == "summary"){
      #      if(input$DodgeSelect == "Economic measures side-by-side"){
      #      sprintf(paste("Summary Economic Measures for West Coast Catcher Vessels\n Group variable:", input$CategorySelect,
      #              "\nStatistic: ", input$StatSelect))
      #      } else if(input$DodgeSelect == "Composition of total cost net revenue"){
      #        sprintf(paste("Composition of Total Cost Net Revenue for West Coast Catcher Vessels\n Group variable:", input$CategorySelect,
      #                "\nStatistic: ", input$StatSelect, "\n Total cost net revenue = Revenue - Variable costs - Fixed costs"))
      #      } else if(input$DodgeSelect == "Composition of variable cost net revenue"){
      #        sprintf(paste("Composition of Variable Cost Net Revenue for West Coast Catcher Vessels\n Group variable:", input$CategorySelect,
      #                "\nStatistic: ", input$StatSelect, "\n Variable cost net revenue = Revenue - Variable costs"))
      #      }#}
      
      #    } else {
      #      sprintf(paste("Variability Analysis of West Coast Catcher Vessels\n Variable:",input$VariableSelect, 
      #              "\nStatistic: ", input$StatSelect))
      #    }
    }
    
 
    
    # simple scaling for bar charts based on number of inputs
    scale_bars <- function(){
      #       a = length(input$ShortdesrSelect)
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
      if(b == 10 | b ==9) {
        return(1)
      } else if(b == 8){
        return(1.2)
      } else if(b >= 5 & b < 8){
        return(1.4)
      } else if(b<5 | b==12){
        return(1.75)
      }   else {
          if(b==11) {
            return(1)
          }
      }
      } else {
        return(1.3)
      }
    }   
    
    scale_text2 <- function() {
      
      b <- table(table(dat$SHORTDESCR)>1)[[1]]
      if(b == 2 | b ==5) {
        return(1.7)
      } else {
        return(1.3)
      } 
    }   
    
    
    g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment=environment()) 

        if(type == "summary"){
          if(input$DodgeSelect == "Economic measures side-by-side"){
            if(input$PlotSelect!="Bar"){
        if(input$PlotSelect == "Point"){
          
           g <- g + geom_point(aes_string(colour = groupVar), size=4)   
             } else {
           g <- g + geom_line(aes_string(colour = groupVar), size=1.5)
        }} # end if statement for line figure
          
        if(input$PlotSelect == "Bar"){
           # if(!is.null(input$DodgeSelect)){
            #  if(input$DodgeSelect == "Economic measures side-by-side"){
                g <- g + geom_bar(aes_string(fill = groupVar, order=groupVar), stat="identity", position="dodge", width = scale_bars())
              } #End if else for side-by-side comparion
           # }}     
          
            if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
              if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
                if(length(input$YearSelect[input$YearSelect>2010])==1){
                  g <- g + geom_rect(aes_string(xmin=.1, xmax=2.35, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=0.4,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares", family="sans"),hjust=0, color="grey20", size=4.5/scale_text()) + 
                  geom_text(aes(x=length(table(as.numeric(YEAR)))+.6,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares", family="sans"),hjust=1, size=4.5/scale_text(), color="grey20")# +
 
              } else {
                g <- g + geom_rect(aes_string(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=0.3,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares", family="sans"),hjust=0, color="grey20", size=4.5/scale_text()) + 
                  geom_text(aes(x=2.65,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares", family="sans"),hjust=0, size=4.5/scale_text(), color="grey20")# +
                
              }} else  {
                if(length(input$YearSelect[input$YearSelect>2010])==1){
                  g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=.25,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares", family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                  geom_text(aes(x=length(table(as.numeric(YEAR)))+.6,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares", family="sans"),hjust=1, size=4.5/scale_text(), color="grey20") #+
                } else {
                  g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                    geom_text(aes(x=.15,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares", family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                    geom_text(aes(x=1.9,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares", family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") #+
                }
              }} # end if-else for adding dashed lines or not (pre- and post- catch shares)
            else {
              g <- g
            }
            }} #End standard plots

    
    if(type == "summary"){    
      #if(input$PlotSelect!="Bar"){
       # if(!is.null(input$DodgeSelect)){          
          if(input$DodgeSelect != "Economic measures side-by-side"){          
                g <- ggplot(dat, aes_string(x = x, y = y , group = groupVar, fill=groupVar, order=groupVar)) + 
                  geom_bar(aes_string(fill = groupVar), stat = "identity", position = "stack", width = scale_bars())
          #    }  

            if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
              if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
                if(length(input$YearSelect[input$YearSelect>2010])==1){
                g <- g + geom_rect(aes(xmin=.1, xmax=2.35, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=.4,y=max(VALUE)/500, label="Pre-catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                  geom_text(aes(x=length(table(as.numeric(YEAR)))+.6,y=max(VALUE)/500,label="Post-catch shares",family="sans"),hjust=1, size=4.5/scale_text(), color="grey20")# +
                } else {
                  g <- g + geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                    geom_text(aes(x=.3,y=max(VALUE)/500, label="Pre-catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                    geom_text(aes(x=2.35,y=max(VALUE)/500,label="Post-catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20")# +
                }
                } else  {
                  if(length(input$YearSelect[input$YearSelect>2010])==1){  
                g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=.25,y=max(VALUE)/500, label="Pre-catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                  geom_text(aes(x=length(table(as.numeric(YEAR)))+.2,y=max(VALUE)/500,label="Post-catch shares",family="sans"),hjust=1, size=4.5/scale_text(), color="grey20") #+
                  } else {
                    g <- g + geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf), fill="grey50", alpha=.05/length(input$YearSelect))+ 
                      geom_text(aes(x=.15,y=max(VALUE)/500, label="Pre-catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") + 
                      geom_text(aes(x=1.9,y=max(VALUE)/500,label="Post-catch shares",family="sans"),hjust=0, size=4.5/scale_text(), color="grey20") #+
                  }
                 }}   else {
              g <- g 
            }
            }} # end if statement for variable cost revenue figure
      #  }} 


    
# Begin Variability analysis figure
      if(type!="summary"){
      if(length(input$YearSelect) > 1){
        if(length(input$YearSelect)>1 & min(input$YearSelect)<2011 & max(input$YearSelect)>2010){
          if(input$YearSelect[1]==2009&input$YearSelect[2]==2010){
            if(length(input$YearSelect[input$YearSelect>2010])==1){
            g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+  
              geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
              geom_text(aes(x=.5,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") + 
              geom_text(aes(x=length(table(as.numeric(YEAR)))+.7,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=1, size=4.5/scale_text2(), color="grey20",family="sans") #+
            #  geom_text(aes(x=2.5,y=(max(VALUE)/1000+max(VALUE)/4000),label="West Coast trawl catch shares program")) 
            # geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white") 
            } else {
              g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+  
                geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
                geom_text(aes(x=.25,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") + 
                geom_text(aes(x=length(table(as.numeric(YEAR)))-1.5,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") #+
            }  }  else {
              if(length(input$YearSelect[input$YearSelect>2010])==1){              
            g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+ 
              geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
              geom_text(aes(x=.25,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") + 
              geom_text(aes(x=2.5,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=1, size=4.5/scale_text2(), color="grey20",family="sans") #+
            # geom_text(aes(x=1.5,y=(max(VALUE)/1000+max(VALUE)/4000),label="West Coast trawl catch shares program")) 
            # geom_linerange(aes(ymin=Inf, ymax=max(VALUE)/1000+max(VALUE)/4000),col="white") 
              } else {
                g <- g + geom_line(aes_string(colour = groupVar), size=1.5)+ 
                  geom_rect(aes(xmin=.1, xmax=1.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/length(input$YearSelect))+ 
                  geom_text(aes(x=.25,y=max(VALUE)/1000+max(VALUE)/10000, label="Pre-catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") + 
                  geom_text(aes(x=2.2,y=max(VALUE)/1000+max(VALUE)/10000,label="Post-catch shares"),hjust=0, size=4.5/scale_text2(), color="grey20",family="sans") #+
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
    
   #       
    #     
    
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

        
    # defien x scale
    #     g <- g +  scale_x_discrete(labels = c("2010" = "", "2011" = ""))
    
    # define solid line y=0
    g <- g + geom_hline(yintercept = 0)
    
 
    
    # define labels
#    if(type!="summary"){
 #     if(max(dat$flag)>0){#input$CategorySelect == "Homeport" & 
#        g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x="Vessels are sorted annually into top, middle, and lower earners based on revenue. 
 #                 \nSome of the data selected may not be shown. These data have been suppressed as there are not enough observations to protect confidentiality", title = main()) 
 #     } else {
 #       g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x="Vessels are sorted annually into top, middle, and lower earners based on revenue.", title = main()) 
#      }
#    } else {
#      if(PermitMessage()){
#        g <- g + labs(y = paste("Thousands ($)","(",input$StatSelect, ")"), x="NOTE: Data from the Groundfish fixed gear with trawl endorsement fishery in 2009 has been suppressed as there are not enough observations to protect confidentiality", title = main())   
#      }else{
#        g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x="", title = main())       
#      }
#    }
       # define labels
#    if(type != "summary"){
#      if(max(dat$flag)>0){
#        g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x= paste("Some of the data selected may not be shown. These data have been suppressed as there are not enough observations to protect confidentiality. \nSourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y")), title = main()) 
#        } else {
#          g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x= paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y")), title = main()) 
#    } 
#      }else {
#    if(PermitMessage()){
#      g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x= paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),"\n",                                                   "Note that if the Groundfish fixed gear with trawl endorsement fishery has been selected, data from 2009 has been suppressed as there are not enough observations to protect confidentiality."), title = main()) 
#    } else {
#    g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x= paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y")), title = main()) 
#  } }


    
    # define labels
    if(type!="summary"){
      if(max(dat$flag)>0){
        if(max(dat$AK_FLAG, na.rm=T)==0){#input$CategorySelect == "Homeport" & 
          g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),     
             "\nVessels are sorted annually into top, middle, and lower earners based on revenue. 
             \nSome of the data selected may not be shown. These data have been suppressed as there are not enough observations to protect confidentiality."), title = main()) 
        } else {
          g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),     
             "\nVessels are sorted annually into top, middle, and lower earners based on revenue. 
             \nSome of the data selected may not be shown. These data have been suppressed as there are not enough observations to protect confidentiality.
             \nNOTE: There are some cases where there are not enough observations of vessels that either 1) fished solely in the West Coast fisheries, 2) also also fished in AK \nor 3) fished for whiting. When this occurs, we show results for groups combined, regardless of whether you selected the INCLUDE VESSELS THAT FISHED IN AK or \nIthe NCLUDE VESSELS THAT FISHED FOR WHITING buttons."), title = main())        
        } 
      } else {
        if(max(dat$AK_FLAG, na.rm=T)==0){
          g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),     
         "\nVessels are sorted annually into top, middle, and lower earners based on revenue."), title = main()) 
        } else {
          g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),     
            "\nVessels are sorted annually into top, middle, and lower earners based on revenue.
             \nNOTE: There are some cases where there are not enough observations of vessels that either 1) fished solely in the West Coast fisheries, 2) also also fished in AK \n or 3) fished for whiting. When this occurs, we show results for groups combined, regardless of whether you selected the INCLUDE VESSELS THAT FISHED IN AK or \nthe INCLUDE VESSELS THAT FISHED FOR WHITING buttons."), title = main())        
        } }
    } else {
      if(PermitMessage()){
        if(max(dat$AK_FLAG, na.rm=T)==0){
          g <- g + labs(y = paste("Thousands ($)","(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),     
             "\nNOTE: Data from the Groundfish fixed gear with trawl endorsement fishery in 2009 has been suppressed as there are not enough observations to protect confidentiality."), title = main())   
        } else {
          g <- g + labs(y = paste("Thousands ($)","(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y"),     
            "\nNOTE: Data from the Groundfish fixed gear with trawl endorsement fishery in 2009 has been suppressed as there are not enough observations to protect confidentiality.
             \nNOTE: There are some cases where there are not enough observations of vessels that either 1) fished solely in the West Coast fisheries, 2) also also fished in AK \nor 3) fished for whiting. When this occurs, we show results for groups combined, regardless of whether you selected the INCLUDE VESSELS THAT FISHED IN AK or \nthe INCLUDE VESSELS THAT FISHED FOR WHITING buttons."), title = main())        
        }
      }else{
        #   if(max(dat$AK_FLAG)==0){
        g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")"), x=paste("Sourced from the FISHEyE application (http://devdataexplorer.nwfsc.noaa.gov/fisheye/FisheyeApp/) maintained by NOAA Fisheries NWFSC on ",format(Sys.Date(), format="%B %d %Y")), title = main())       
        #    } else {
        #     g <- g + labs(y = paste("Thousands ($)", "(",input$StatSelect, ")",
        #                             x=" Less than three vessels participated in an Alaskan fishery. To protect confidentiality, we do not differentiate  
        #                              \nbetween vessels that fished solely off the West Coast and vessels that also participated in an Alaskan fisheries.", title = main())                  
        #    }
      }
    }
    
    
    # define theme
    g <- g + theme(
      plot.title = element_text(size=rel(1), vjust=1, colour="grey25"), 
      plot.title = element_text(family = "sans", face = "bold", vjust = 1),
      plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
      panel.background = element_rect(fill = "white"),
      panel.margin = unit(1.2, "lines"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", 
                                size = 11, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=rel(.7), face="italic", vjust=0, colour="grey25"),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 9.5),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
                                 color = "grey25", face = "bold", size = 12),
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
      sum = sum(sapply(grobs$width, function(x) convertWidth(x, "in")))
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