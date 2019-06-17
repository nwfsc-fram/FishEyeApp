doPlotPrep <- reactive({
  if (PermitPlot()) {
    #removeNAs
    #print(input$demStats)
    dat <- subset(DatSub(), is.na(DatSub()$VALUE) == FALSE)
    x = 'YEAR'
    y = 'VALUE'
    ##Prepping data for plotting by converting to more "plot friendly" values
    
    # Data prep functions ####
    convertfunction <- function(x, unit = dat$unit) {
      case_when(
        unit == '' ~ x, 
        unit == 'thousands' ~ x / 1e3, 
        unit == 'millions' ~ x / 1e6, 
        unit == 'billions' ~ x / 1e9, 
        T ~ -999)
    }
    #browser()
    dat4plot <- mutate(dat,
      VARIANCE = convertfunction(VARIANCE),
      q25      = convertfunction(q25),
      q75      = convertfunction(q75),
      VALUE    = convertfunction(VALUE),
      lower    = convertfunction(lower),
      upper    = convertfunction(upper))
    
    dat4plot$sort2 <- if (!input$LayoutSelect) {
      reorder(dat4plot$VARIABLE, dat4plot$sort)
    } else {
      reorder(dat4plot$ylab, dat4plot$sort)
    }
    
    dat4plot$bystategrp <- paste0(dat4plot$AGID, dat4plot$whitingv)
    

    # special handling for seasonality data
    
    if(!is.null(input[['otherSelect']])) if (input$otherSelect == "Seasonality") {
      dat4plot <- mutate(dat4plot,
        VALUE = as.Date(VALUE, origin = "2014-01-01", format = "%Y-%m-%d"),
        lower = as.Date(VALUE, origin = "2014-01-01", format = "%Y-%m-%d"),
        upper = as.Date(VALUE, origin = "2014-01-01", format = "%Y-%m-%d"),
        sort2 = reorder(VARIABLE, sort))
    }
    
    # constants ####
    groupVar <- "whitingv"
    
    # constants except for sector differences ####
    colourThirds <- if (input$Sect_sel != "FR") {
      c(
        'Non-whiting vessels' = "#d7191c",
        'Whiting vessels' = "#2b83ba",
        'All vessels' = "#000000"
      )
    } else {
      c(
        'Non-whiting processors' = "#d7191c",
        'Whiting processors' = "#2b83ba",
        'All processors' = "#000000"
      )
    }
    
    # sector title
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
    
    # subcomponents of title 
    gv <- function () {
      if (input$Sect_sel == 'CV') {
        if (!input$LayoutSelect) {
          if (input$CategorySelect != 'Fisheries') {
            sprintf(paste("Vessels in", input$inSelect)) } else {
              sprintf(paste(""))
            } } else if (input$CategorySelect == 'Homeport' | input$CategorySelect == 'State') {
              sprintf(paste(input$VariableSelect, "vessels in", input$inSelect))
            } else if (input$CategorySelect == 'Vessel length class') {
              sprintf(paste("Vessels in", input$inSelect, "for", input$VariableSelect ))
            } else {
              sprintf(paste("Vessels in", input$VariableSelect))
            } } else {
              sprintf(paste(""))
            }
    }
    
    # assembles the plot title with the gv call
    main <- function() {
      bquote(atop(.(plot.title()), .(gv())))
    }
    
    # plotting functions ####
    # y axis label - ylab is set in dataprep
    yaxislabel <- function () {
      if (input$LayoutSelect) {
        paste(dat4plot$STAT,
          "(see plot title for units)")
      } else {
        dat4plot$ylab
      }
    }
    
    yr <- function() return(unique(as.numeric(dat4plot$YEAR)))
      
      # choose label text size ####
      labeltext <- ifelse(input$tabs == 'Panel1', 7, 5) # this was 5, 7 in doPlotDownload
      
      # draw seasonality rectangle
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
            fontface = "italic"),
          hjust = 0,
          color = "grey20",
          size = labeltext / scale_text())
      }
      
      upper <- function()  return(max(dat4plot$upper))
      #print(yr())
      
      # geom_rect (define the grey boxes for pre-catch shares) ####
      geom_rect_fun <- function(ymin_val = -Inf,
        ymax_val = Inf) {
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
      # confidentiality messages ####
      supp_obs <- function() {
        "\n
      \nData have been suppressed for som tears years because there are not enough observations to protect confidentiality. \nPlease see the About tab for more information."
      }
      
      
      # x-axis label ####
      # xlab is actually "notes"
      xlab <- function() {
        if (sum(dat4plot$conf) == 0 & sum(dat4plot$flag) == 0) {
          ""
        } else {
          paste(supp_obs())
        }
      }
      
      # Scaling factor for text size ####
      scale_text <- function() {
        if (input$Ind_sel != "Economic") {
          return(1.1)
        } else {
          return(1.2)
        }
      }
      
      # scaling factor for geom text size ####
      scale_geom_text <- function() {
        if (any(dat4plot$VALUE > 0, na.rm = T)) {
          return(max(dat4plot$VALUE, na.rm = T))
        } else {
          return(0)
        }
      }
      
      # CREATE FIRST PLOT INSTANCE ####
      
      g <- ggplot(dat4plot, aes_string(x = x, y = y , group = groupVar),
        environment = environment())
      
      # PLOT: add lines and points to the plot ####
      if (input$Ind_sel == 'Other') {
        if (input$otherSelect == 'Share of landings by state') {
          g <-
            g + geom_line(aes_string(colour = groupVar, group = 'bystategrp'),
              size = 1.5) +
            geom_point(aes_string(
              colour = groupVar,
              shape = 'AGID',
              group = 'bystategrp'
            ),
              size = 4)
        }} else {
          g <- g + geom_line(aes_string(colour = groupVar), size = 1.5) +
            geom_point(aes_string(colour = groupVar), size = 4)
        }

      # PLOT: add 'data variability' band ####
        g <-
          g + geom_ribbon(aes(
            ymax = upper,
            ymin = lower,
            fill = whitingv
          ), alpha = .25)
      
      # PLOT: Facets #####
      if (!input$LayoutSelect) {
        g <- g + facet_wrap( ~ sort2, ncol = 2)
      } else {
        g <- g + facet_wrap( ~ sort2, scales = 'free_y', ncol = 2)
      }
      
     
      # PLOT: Define grey shading and Non-CS/CS labels ------####
      # set rect and text for plots with both CS and non-CS years ####
      # otherwise no rect or text
      # the original code for the geom_text* are commented out at the bottom of the doc
      # if there are years shown before and after implementation of catch shares
      # if (length(yr()) > 1 & min(yr()) < 2011 & max(yr()) > 2010) {
      #   # if the "Group by vessels" display is chosen
      #   if (!input$LayoutSelect) {
      #     # if seasonality is clicked
      #     if (input$Ind_sel == 'Other') {
      #       # and seasonality is selected
      #       if (input$otherSelect == "Seasonality") {
      #         g <- g + geom_rect_fun(
      #           ymin_val = structure(-Inf, class = "Date"),
      #           ymax_val = structure(Inf, class = "Date")
      #         )
      #         g <- g + geom_text_fun(
      #           x_val = table(yr() <= 2010)[[2]] / 3.5,
      #           y_val = min(as.Date(upper(), origin = "2014-01-01")),
      #           label_val = "Pre-catch shares"
      #         )
      #         g <- g + geom_text_fun(
      #           x_val = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
      #           y_val = min(as.Date(upper(), origin = "2014-01-01")),
      #           label_val = "Catch shares"
      #         )
      #         # for all other variables
      #       } else {
      #         g <- g + geom_rect_fun()
      #         # geom_text1
      #         g <- g + geom_text_fun(
      #           x_val = table(yr() <= 2010)[[2]] / 3.5,
      #           y_val = max(upper()) + scale_geom_text() / 5,
      #           label_val = "Pre-catch shares"
      #         )
      #         g <- g +
      #           # geom_text3
      #           geom_text_fun(
      #             x_val = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
      #             y_val = max(upper()) + scale_geom_text() / 5,
      #             label_val = "Catch shares"
      #           )
      #       }
      #     } else {
      #       g <- g + geom_rect_fun()
      #       # geom_text1
      #       g <- g + geom_text_fun(
      #         x_val = table(yr() <= 2010)[[2]] / 3.5,
      #         y_val = max(upper()) + scale_geom_text() / 5,
      #         label_val = "Pre-catch shares"
      #       )
      #       g <- g +
      #         # geom_text3
      #         geom_text_fun(
      #           x_val = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
      #           y_val = max(upper()) + scale_geom_text() / 5,
      #           label_val = "Catch shares"
      #         )
      #     }
      #   } else {
      #     # Compare by metrics
      #     g <- g + geom_rect_fun()
      #     g <- g +
      #       # geom_text4
      #       geom_text_fun(
      #         x_val = table(yr() <= 2010)[[2]] / 3.5,
      #         y_val = Inf,
      #         label_val = "Pre-catch shares",
      #         vjust_val = 1.5
      #       )
      #     g <- g +
      #       # geom_text6
      #       geom_text_fun(
      #         x_val = table(yr() <= 2010)[[2]] + table(yr() > 2010)[[2]] / 1.5,
      #         y_val = Inf,
      #         label_val = "Catch shares",
      #         vjust_val = 1.5
      #       )
      #     
      #   }
      # } 
      
      # PLOT: set colors for the three lines (whiting vessels, non-whiting vessels, all vessels) ####
      g <- g + scale_fill_manual(values = colourThirds) + scale_colour_manual(values = colourThirds) 
      
      # PLOT: add y axis line ####
      g <- g + geom_hline(yintercept = 0)
      
      # PLOT: define labels (not sure what this does------####
      g <- g + labs(y = yaxislabel(),
        x = ifelse(input$tabs == 'Panel1', xlab(), ''),
        title = main())
  }
  
  int <- list(plot = g, dat = dat4plot)
  return(int)
})
