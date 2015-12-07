dat <- netrevThirds

dat <- subset(dat, 
                   SHORTDESCR %in% c("Revenue","Variable cost net revenue") &
                   CATEGORY == "Fisheries" &
                   VARIABLE == "All Catch Share Fisheries" &
                   FISHAK == TRUE &
                   STAT == "Average per vessel"
)

dat$SHORTDESCR <- factor(dat$SHORTDESCR, 
                            levels =  factorOrder$shortdescr)
dat$YEAR <- factor(dat$YEAR)
dat$VARIABLE <- factor(dat$VARIABLE)
dat$col <- factor(dat$col)
dat$VALUE <- dat$VALUE/1000
dat$col <- ifelse(dat$THIRDS=="Bottom third", "#a1dab4", "#41b6c4")
dat$col <- ifelse(dat$THIRDS=="Top third", "#253494", dat$col)
    groupVar <- dat$THIRDS
    facetVar <- dat$SHORTDESCR
    
colortop="#253494"
colormid="#41b6c4"
colorbottom ="#a1dab4"

    main <-   sprintf(paste("Variability analysis of West Coast Catcher Vessels\n Variable: All Catch Share Fisheries\nStatistic: Average per vessel"))
  
    
    # simple scaling for bar charts based on number of inputs
    scale_bars <-0.9
    scale_text2 <- 1.3
    
    
    g <- ggplot(dat, aes(x=YEAR,y=VALUE, group = factor(THIRDS))) + 
                geom_line(aes_string(colour = dat$THIRDS), size=1.5)+  
                geom_rect(aes(xmin=.1, xmax=2.5, ymin=-Inf, ymax=Inf),fill="grey50", alpha=.05/5)+ 
                geom_text(aes(x=.5,y=max(dat$VALUE)/1000+max(dat$VALUE)/10000, label="Pre-catch shares", family="sans"),hjust=0,color = "grey20", size=7/scale_text2) + 
                geom_text(aes(x=3,y=max(dat$VALUE)/1000+max(dat$VALUE)/10000,label="Post-catch shares"),hjust=0, family="sans",color = "grey20", size=7/scale_text2) #+
              
    # define facet
      g <- g + facet_grid(.~SHORTDESCR)

    # define scale
      g <- g + scale_fill_manual(labels=c("Top third","Middle third","Bottom third"),
                                 values = c("#253494","#41b6c4","#a1dab4")) + 
        scale_colour_manual(labels=c("Top third","Middle third","Bottom third"),
                            values = c("#253494","#41b6c4","#a1dab4"))
    
    
    # define solid line y=0
    g <- g + geom_hline(yintercept = 0)
    
    # define labels
      g <- g + labs(y = "Thousands ($)", x="Vessels are sorted annually into top, middle, and bottom earners based on revenue", title = main()) 
    
    
    # define theme
    g <- g + theme(
      plot.title = element_text(size=rel(1.75), vjust=1, colour="grey25"), 
      plot.title = element_text(family = "sans", face = "bold", vjust = 1),
      panel.background = element_rect(fill = "white"),
      #  panel.margin = unit(1.2, "lines"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", 
                                size = 18, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=rel(1.2),  face="italic", vjust=-1, hjust=-.05, colour="grey25"),
      axis.title.y = element_text(size=rel(1.2), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
                                 color = "grey25", face = "bold", size = 12),
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
      if ((class(p$facet)[1] == "grid" && !is.null(names(p$facet$cols))) ||
          class(p$facet)[1] == "wrap")
      {
        ps = calc_element("strip.text.x", th)[["size"]]
        family = calc_element("strip.text.x", th)[["family"]]
        face = calc_element("strip.text.x", th)[["face"]]
        
        if (class(p$facet)[1] == "wrap") {
          nm = names(p$facet$facets)
        } else {
          nm = names(p$facet$cols)
        }
        
        # get number of facet columns
        levs = levels(factor(p$data[[nm]]))
        npanels = length(levs)
        if (class(p$facet)[1] == "wrap") {
          cols = n2mfrow(npanels)[1]
        } else {
          cols = npanels
        }
        
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
      }
      
      if (class(p$facet)[1] == "grid" && !is.null(names(p$facet$rows))) {  
        ps = calc_element("strip.text.y", th)[["size"]]
        family = calc_element("strip.text.y", th)[["family"]]
        face = calc_element("strip.text.y", th)[["face"]]
        
        nm = names(p$facet$rows)
        
        # get number of facet columns
        levs = levels(factor(p$data[[nm]]))
        rows = length(levs)
        
        # get plot height
        sum = sum(sapply(grobs$height, function(x) convertWidth(x, "in")))
        panels_height = par("din")[2] - sum  # inches
        # determine strwrap width
        panels_height = panels_height / rows
        mx_ind = which.max(nchar(levs))
        char_height = strwidth(levs[mx_ind], units="inches", cex=ps / par("ps"), 
                               family=family, font=gpar(fontface=face)$font) / 
          nchar(levs[mx_ind])
        width = floor((panels_height - pad)/ char_height)  # characters
        
        # wrap facet text
        p$data[[nm]] = unlist(lapply(strwrap(p$data[[nm]], width=width, 
                                             simplify=FALSE), paste, collapse="\n"))
      }
      
      invisible(p)
    }   
    #################################################################################################################################
    g <- strwrap_strip_text(g) #use instead of print(g)
    
    print(g)
    
  } else plot(0,0,type="n", axes=F, xlab="", ylab="")
}