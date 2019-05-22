doPlotDownload <- function(dat, x, y){
  if(PermitPlot()){
    dat <- subset(dat, is.na(dat$VALUE)==FALSE)
    ##Prepping data for plotting by converting to more "plot friendly" values
    dat<- mutate(dat,
                 VARIANCE = case_when(
                   unit == '' ~ VARIANCE,
                   unit == 'thousands' ~ VARIANCE/1e3,
                   unit == 'millions' ~ VARIANCE/1e6,
                   unit == 'billions' ~ VARIANCE/1e9,
                   T ~ -999),
                 q25 = case_when(
                   unit == '' ~ q25,
                   unit == 'thousands' ~ q25/1e3,
                   unit == 'millions' ~ q25/1e6,
                   unit == 'billions' ~ q25/1e9,
                   T ~ -999),
                 q75 = case_when(
                   unit == '' ~ q75,
                   unit == 'thousands' ~ q75/1e3,
                   unit == 'millions' ~ q75/1e6,
                   unit == 'billions' ~ q75/1e9,
                   T ~ -999),
                 VALUE = case_when(
                   unit == '' ~ VALUE,
                   unit == 'thousands' ~ VALUE/1e3,
                   unit == 'millions' ~ VALUE/1e6,
                   unit == 'billions' ~ VALUE/1e9,
                   T ~ -999))
    
    dat$sort2 <- if (!input$LayoutSelect) {
      reorder(dat$VARIABLE, dat$sort)
    } else {
        reorder(dat$ylab, dat$sort)
      }
    
    dat$thresh <-  if(input$Ind_sel=="Economic"){   
      length(unique(dat$YEAR<=2010))
    } else if(input$Ind_sel!="Economic"){ 
      if(input$LayoutSelect){   
        if(input$PlotSelect==T&!is.na(max(dat$VARIANCE))) { 
          data.frame(dat %>% 
                       group_by(METRIC) %>% 
                       mutate(threshold=max(VALUE, na.rm=T)+max(VARIANCE, na.rm=T)+max(VALUE, na.rm=T)/10))%>% 
            subset(select=c(threshold))
        } else {
          data.frame(dat %>% 
                       group_by(METRIC) %>% 
                       mutate(threshold=max(VALUE, na.rm=T)+max(VALUE, na.rm=T)/10))%>% 
            subset(select=c(threshold))
        }
      } else {
        0
      }}

    rectvars <- dat %>% 
      distinct(sort2,YEAR) %>% 
      group_by(sort2) %>% 
      mutate(minx=min(as.numeric(YEAR)), 
             xmaxscale=length(YEAR[YEAR<2011]), 
             maxx=max(YEAR))  %>% 
      subset(select=c(sort2, minx,xmaxscale, maxx)) %>%
      data.frame()%>% 
      distinct %>% 
      merge(dat %>% 
              distinct(sort2,whitingv))
    
    dat$upper <-
      if (input$Ind_sel == "Economic") {
        if (input$AVE_MED == 'A') {
          dat$VALUE + dat$VARIANCE
        } else if (input$AVE_MED == 'T') {
          dat$VALUE
        } else {
          dat$q75
        }} else if (input$Ind_sel == 'Cost') {
          if (input$AVE_MED_COSTS == 'A') {
            dat$VALUE + dat$VARIANCE
          } else if (input$AVE_MED_COSTS == 'T') {
            dat$VALUE
          } else {
            dat$q75
          }
        } else if (input$Ind_sel == 'Other') {
          if (input$otherStat == 'Mean') {
            dat$VALUE + dat$VARIANCE
          } else if (input$otherStat == 'Total') {
            dat$VALUE
          } else {
            dat$q75
          }
        } else if (input$Ind_sel == 'Labor') {
          if(input$crewStats == 'Mean') {
            dat$VALUE + dat$VARIANCE
          } else if (input$crewStats == 'Total') {
            dat$VALUE
          } else {
            dat$q75
          }
        } else if (input$Ind_sel == 'Vessel characteristics' || input$Ind_sel == 'Processor characteristics') {
          if (input$AVE_MED2 == 'Mean') {
            dat$VALUE + dat$VARIANCE
          } else if (input$AVE_MED2 == 'Total') {
            dat$VALUE
          } else {
            dat$q75
          }
        }
    
    
    dat$lower <-
      if (input$Ind_sel == "Economic") {
        if (input$AVE_MED == 'A') {
          dat$VALUE - dat$VARIANCE
        } else if (input$AVE_MED == 'T') {
          dat$VALUE
        } else  {
          dat$q25
        }
      } else if (input$Ind_sel == 'Cost') {
        if (input$AVE_MED_COSTS == 'A') {
          dat$VALUE - dat$VARIANCE
        } else if (input$AVE_MED_COSTS == 'T') {
          dat$VALUE
        } else {
          dat$q25
        }
      } else if (input$Ind_sel == 'Other') {
        if (input$otherStat == 'Mean') {
          dat$VALUE - dat$VARIANCE
        } else if (input$otherStat == 'Total') {
          dat$VALUE
        } else {
          dat$q25
        }
      } else if (input$Ind_sel == 'Labor') {
        if (input$crewStats == 'Mean') {
          dat$VALUE - dat$VARIANCE
        } else if (input$crewStats == 'Total') {
          dat$VALUE
        } else {
          dat$q25
        }
      } else if (input$Ind_sel == 'Vessel characteristics' || input$Ind_sel == 'Processor characteristics') {
        if (input$AVE_MED2 == 'Mean') {
          dat$VALUE - dat$VARIANCE
        } else if (input$AVE_MED2 == 'Total') {
          dat$VALUE
        } else  {
          dat$q25
        }
      }
    
    upper <- function() {
      if (input$Ind_sel == "Economic") {
        if (input$PlotSelect == T) {
          if (input$AVE_MED == 'A') {
            max(dat$VALUE + dat$VARIANCE, na.rm = T)
          } else if (input$AVE_MED == 'M') {
            max(dat$q75, na.rm = T)
          } else  {
            max(dat$VALUE, na.rm = T)
          } } else {
            max(dat$VALUE, na.rm = T)
          }
      } else if (input$Ind_sel == 'Cost') {
        if (input$PlotSelect == T) {
          if (input$AVE_MED_COSTS == 'A') {
            max(dat$VALUE + dat$VARIANCE, na.rm = T)
          } else if (input$AVE_MED_COSTS == 'M') {
            max(dat$q75, na.rm = T)
          } else  {
            max(dat$VALUE, na.rm = T)
          } } else {
            max(dat$VALUE, na.rm = T)
          }
      } else if (input$Ind_sel == 'Other') {
        if (input$PlotSelect == T) {
          if (input$otherStat == 'Mean') {
            max(dat$VALUE + dat$VARIANCE, na.rm = T)
          } else if (input$otherStat == 'Median') {
            max(dat$q75, na.rm = T)
          } else {
            max(dat$VALUE, na.rm = T)
          } } else {
            max(dat$VALUE, na.rm = T)
          }
      } else if (input$Ind_sel == 'Labor') {
        if (input$PlotSelect == T) {
          if (input$crewStats == 'Mean') {
            max(dat$VALUE + dat$VARIANCE, na.rm = T)
          } else if (input$crewStats == 'Median') {
            max(dat$q75, na.rm = T)
          } else {
            max(dat$VALUE, na.rm = T)
          }
        } else {
          max(dat$VALUE, na.rm = T)
        }
      } else if (input$Ind_sel == 'Vessel characteristics' || input$Ind_sel == 'Processor characteristics') {
        if (input$PlotSelect == T) {
          if (input$AVE_MED2 == 'Mean') {
            max(dat$VALUE + dat$VARIANCE, na.rm = T)
          } else if (input$AVE_MED2 == 'Median') {
            max(dat$q75, na.rm = T)
          } else {
            max(dat$VALUE, na.rm = T) 
          } } else {
            max(dat$VALUE, na.rm = T)
          }
      } }
    
    yaxislabel <- function () {
      if(input$LayoutSelect) {
        if(input$Ind_sel %in% c('Economic', 'Cost')) {
          paste(dat$STAT,
                "(Scale and units depend upon metric)")
        } else {
          paste(dat$SUMSTAT,
                "(Scale and units depend upon metric)")
        }
      } else {
        dat$ylab
      }
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
    
      # Plot header construction ####
      # title
      plot.title <- function() {
        if (input$Sect_sel == "CV") {
          return("West Coast Catcher Vessels")
        } else if (input$Sect_sel == "M") {
          return("West Coast Mothership Vessels")
        } else if (input$Sect_sel == "CP") {
          return("West Coast Catcher-Processor Vessels")
        } else if (input$Sect_sel == "FR") {
          return("West Coast First Receivers and Shorebased Processors")
        }
      }
      
      
      gv <- function () {
        if (input$Sect_sel == 'CV') {
          if (!input$LayoutSelect) {
            if (input$CategorySelect != 'Fisheries') {
              sprintf(paste(
                "Vessels in",
                input$inSelect))
            } else {
              sprintf(paste(
                ""
              ))
            }} else if (input$CategorySelect == 'Homeport' | input$CategorySelect == 'State') {
              sprintf(paste(
                input$VariableSelect, 
                "vessels in",
                input$inSelect
              ))
            }
          else if (input$CategorySelect == 'Vessel length class') {
            sprintf(paste(
              "Vessels in",
              input$inSelect,
              "for",
              input$VariableSelect
            ))
          }
          else {
            sprintf(paste(
              "Vessels in",
              input$VariableSelect
            ))
          }
        }
        else {
          sprintf(paste(
            ""
          ))
        }
      }
      
      
      main <- function() {
        bquote(atop(.(plot.title()), .(gv())))
      }       
      ##gv <- function() {
      
    
source_lab <- function(){
  paste("\n
        \nSourced from the FISHEyE application (https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",format(Sys.Date(), format="%B %d %Y"))
}
# confidentiality messages ####
supp_obs <- function() {
  "\n
  \nData have been suppressed for years that are not plotted as there are not enough observations to protect confidentiality. \nPlease see the About tab for more information."
}


# x-axis label ####
# xlab is actually "notes"
xlab <- function() {
  if (max(dat$conf) == 0) {
    if(max(dat$flag) == 0) {
      paste(source_lab())
    }
    else {
      paste(supp_obs(), source_lab())
    }
  } else {
    if (max(dat$flag) == 0) {
      paste(supp_obs(), source_lab())
    } else {
      paste(supp_obs(), source_lab())
    }
  }}

    
   scale_text <- function() {
      if(input$Ind_sel!="Economic"){
        return(1.1)
      } else { 
        return(1.2)
      }}   
    
   # scaling factor for geom text size ####
   scale_geom_text <- function() {
     if (any(dat$VALUE > 0, na.rm = T)) {
       return(max(dat$VALUE, na.rm = T))
     } else {
       return(0)
     }
   }
    
#----- Define ggplot ------#    
    # format data for graph and add to graph ####
    # special data for seasonality plot
    #  print(paste0(seasonality, 1))
    if(input$Ind_sel == 'Other' & !input$LayoutSelect) {
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
        dat$bystategrp <- paste0(dat$AGID, dat$whitingv)
        g <-
          # I think this is where the NAs are getting removed which causes lines to be connected through suppressed/missing values #removeNAs
          ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment =
                   environment()) #+coord_cartesian(xlim = c(0, length(table(dat$YEAR))+1))
      }
    } else {
      #dat <- dat[order(dat$sort), ]
      g <-
        # I think this is where the NAs are getting removed which causes lines to be connected through suppressed/missing values #removeNAs
        ggplot(dat, aes_string(x = x, y = y , group = groupVar), environment =
                 environment()) #+coord_cartesian(xlim = c(0, length(table(dat$YEAR))+1))
    }
    
    # add lines and points to the plot ####
   if (!input$LayoutSelect) {
    if (input$Ind_sel == 'Other') {
      if (input$socSelect == 'Share of landings by state') {
        g <-
          g + geom_line(aes_string(colour = groupVar, group = 'bystategrp'), size = 1.5) +
          geom_point(aes_string(colour = groupVar, shape = 'AGID', group = 'bystategrp'),
                     size = 4)
      } else {
        g <- g + geom_line(aes_string(colour = groupVar), size = 1.5) +
          geom_point(aes_string(colour = groupVar), size = 4)
      }} else {
        g <- g + geom_line(aes_string(colour = groupVar), size = 1.5) +
          geom_point(aes_string(colour = groupVar), size = 4)
      }
   }
   g <- g + geom_line(aes_string(colour = groupVar), size = 0.75) +
     geom_point(aes_string(colour = groupVar), size = 2)

#------ Add variance ------#    
    if(input$PlotSelect==T& !exists('ssn')) { 
      g <- g + geom_ribbon(aes(ymax=upper, ymin=lower, fill=whitingv), alpha=.25)#show.legend = FALSE, 
    } else {
      g <- g
    }
    
    
      
#----- define facet -----#
    if (!input$LayoutSelect) {
      g <- g + facet_wrap(~ sort2, ncol = 2)
    }else {
      g <- g + facet_wrap(~ sort2, scales = 'free_y', ncol = 2)
    }
    
    
#----- Define rectangles and labels -----#
    #----- Define grey shading and Non-CS/CS labels ------####
    # choose label text size ####
    labeltext <- ifelse(input$LayoutSelect & input$tabs == 'Panel1', 5, 7)
    
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
      if (!input$LayoutSelect) {
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

    #----- define scale -----#
    g <- g + scale_fill_manual(values = colourThirds) + scale_colour_manual(values = colourThirds)
    
    #----- define solid line y=0 ------#
    g <- g + geom_hline(yintercept = 0)
    
    #----- define labels ------#
    g <- g + labs(y = yaxislabel(), x=xlab(), title = main())   
    
    if (input$LayoutSelect) {
      if (input$tabs == 'Panel1') {
        strptextsize <- 8
      } else {
        strptextsize <- 12
      } } else {
        strptextsize <- 12
      }
    
    if(input$LayoutSelect) {
      if (length(unique(dat$YEAR)) > 9) {
      xaxissize <- 6
    } else {
      xaxissize <- 11
    } }
    else {
      xaxissize <- 11
    }
    
#----- define theme ------#
    g <- g + theme(
      plot.title = element_text(
        size=rel(1.2), 
        colour="grey25",
        family = "sans", 
        face = "bold", 
        vjust = 1, 
        hjust=.5),
      plot.margin = unit(c(0.25, 0.25, 1, 0.25), "cm"),
      panel.background = element_rect(fill = "white"),
      #panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(
        family = "sans",
        size = strptextsize,
        color = "grey25",
        vjust = 1
      ),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(
        size=rel(.7), 
        face="italic", 
        hjust= 0, 
        vjust=0, 
        colour="grey25"),
      axis.title.y = element_text(
        size=rel(1.2), 
        vjust=2, 
        colour="grey25"),
      axis.line.x = element_line(
        size = 2, 
        colour = "black", 
        linetype = "solid"),
      axis.text = element_text(size = 11),
      axis.text.x = element_text(size = xaxissize),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
                                 color = "grey25", 
                                 face = "bold",
                                 size = 10),
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