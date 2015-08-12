dat$x <- c(500, 0,0,250, 250,0,125,125,250)
dat$groupVar <- c("Revenue","Variable cost net revenue", "Variable cost net revenue","Total cost net revenue", "Total cost net revenue","Total cost net revenue")
dat$y <- c("Revenue","Variable cost net revenue","Variable costs","Total cost net revenue","Fixed costs","Variable costs")
    # Plot title construction
dat$groupVar <-factor(dat$groupVar, levels=c("Revenue","Variable cost net revenue","Total cost net revenue"))
dat$x <- factor(dat$x, levels=c("Revenue","Variable cost net revenue","Variable costs","Total cost net revenue","Fixed costs"))
 
    main <- "Economic indicators reference figure"

                 # Reveue  #VCNR     #VC        TCNR      TC
#pal.netrev <- c("#5a7559","#33a02c","#d5e4d0", "#59aea5","#acd0cc")
#pal.netrev <- c("#253494","#41b6c4","#ffffcc", "#2c7fb8","#a1dab4")
pal.netrev <- c("#d73027","#fc8d59","#fee090", "#4575b4","#91bfdb")
#pal.netrev <- c("#8c510a","#d8b365","#f6e8c3", "#01665e","#5ab4ac")
#pal.netrev <- c("#b2182b","#ef8a62","#fddbc7", "#2166ac","#67a9cf")

    # simple scaling for bar charts based on number of inputs
    ggplot(dat, aes(groupVar, y,fill=x)) + geom_bar(stat="identity", width=.75)+
    scale_fill_manual(values = pal.netrev, guide=guide_legend(reverse=F)) + 
      geom_hline(yintercept = 0)+ labs(y = "Thousands ($)", title = "") + theme(
      plot.title = element_text(size=rel(1.85), vjust=1, colour="grey25"), 
      plot.title = element_text(family = "sans", face = "bold", vjust = 1),
      panel.background = element_rect(fill = "white"),
      panel.margin = unit(1.2, "lines"),
      panel.grid.minor = element_line(linetype = "blank"),
      panel.grid.major.x = element_line(linetype = "blank"),
      panel.grid.major.y = element_line(color = "#656C70", linetype = "dotted"),
      strip.text = element_text(family = "sans", 
                                size = 5, color = "grey25", vjust=1),
      strip.background = element_rect(fill = "lightgrey"),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=rel(1.5), vjust=2, colour="grey25"),
      axis.line.x = element_line(size = 2, colour = "black", linetype = "solid"),
      axis.text = element_text(size = 16, colour="black"),
      legend.position = "top",
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(family = "sans", 
                                 color = "grey25", face = "bold", size = 13),
      legend.title = element_blank())

    