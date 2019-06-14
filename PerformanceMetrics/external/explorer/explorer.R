#this is the main content or output page for the explorer app


#source reactive expressions and other code
source("external/explorer/explorerSourceFiles/ex.reactives.R", local = TRUE)
source("external/explorer/explorerSourceFiles/ex.io.sidebar1.R", local = TRUE)
#source("external/explorer/explorerSourceFiles/doPlotPrep.R", local = TRUE)
source("external/explorer/explorerSourceFiles/doPlot.R", local = TRUE)
source("external/explorer/explorerSourceFiles/doPlotDownload.R", local = TRUE)
source("external/explorer/explorerSourceFiles/defaultText.R", local = TRUE)


enableBookmarking("url")


observeEvent(input$istat, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Statistics may not be appliable for all metrics. The median and mean both attempt to provide information about the results for a representative vessel, however they do it in different ways. The median means that half of the vessels have a larger result than the median, and half are smaller. The mean, is the sum of the values divided by the number of responses. If the data do not have many extreme responses in a particular direction, the median and mean will be very similar. However, if the data are skewed by extreme responses, then the median is a better measure of the result for a typical vessel. The total provides a measure of the fleet as a whole. The fleet-wide total is used to measure how the entire fleet is doing, rather than a representative vessel.')
})
observeEvent(input$istatimpacts, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Income and employment impacts are estimated for the West Coast overall. Impacts by homeport or state are impacts for the West Coast overall based on the vessels with a homeport in the selected port or state.')
})
observeEvent(input$ipo, {
  session$sendCustomMessage(type = 'testmessage',
                            if(input$LayoutSelect) {
                            message = 'See the Definitions Page for a description of each metric. Not all metrics may be applicable for a given statistic.'
                            } else { 
                              message = 'See the Definitions Page for a description of each metric. Not all statistics may be applicable for a given metric.'}
                       
                              )
})
observeEvent(input$FRr, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'The region corresponding to each processors. For First Receivers and Shorebased Processors, two regions are recognized: Washington/Oregon and California.')
})
observeEvent(input$FRs, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'First receivers and shorebased processors are grouped into three size classes based on the average number of workers: large (> 200 workers), medium (100 - 200 workers), and small (< 100 workers). Processor size was determined by examining the maximum weekly number of production workers employed by a processor (as reported on the EDC form) for each year. Then the weekly maximum was averaged across all EDC years to place processors in one size category for all years.')
})
observeEvent(input$FRi, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'For shorebased processors, information on the EDC form  is collected at the species level (e.g. fish production information), not the fishery level like the catcher vessels.')
})
observeEvent(input$ifg, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Use these buttons to automatically select a group of fisheries. Clicking the button a second time will deselect the selected fisheries. For example, clicking All fisheries will select each box below and will produce figures for each individual fishery, the combined activities across all fisheries, the combined activities across all catch share fisheries, and the combined activities across all non-catch share fisheries.')
})
observeEvent(input$iof, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'You can choose to show activities for an individual fisheries or activities across all fisheries, all catch share fisheries, or all non-catch share fisheries.')
})
####EDIT HERE
observeEvent(input$isummed, {
  session$sendCustomMessage(type = 'testmessage',
                            if(input$Sect_sel=="CV"){
                            message = 'For each homeport, state, or vessel length class, you can select to show activities across all fisheries, only catch share fisheries, or only non-catch share fisheries.'
                            } else {
                            message = 'For each region or processor size class, you can select to show activities across all production, only groundfish production, or only other species production.'
                            })
})
observeEvent(input$iem, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Variable cost net revenue is revenue minus variable costs. Total cost net revenue is revenue minus fixed and total costs. Further information on the economic measures can be found in the Definitions page.'
  )
})
observeEvent(input$ivs, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'We provide the options to view activities for individual fisheries and activities combined over a group of fisheries. For example, the option All catch share fisheries shows the combined activies across the five catch share fisheries.')
})
observeEvent(input$iVesSum, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'For all vessels that fished within selected fisheries, show data for activities either within the selected fisheries, across all catch share fisheries, or across all West Coast fisheries. ')
})
observeEvent(input$icompare, {
  session$sendCustomMessage(type= 'testmessage',
                            if(input$Sect_sel=="CV"){
                            message = 'You are selecting to either 1) look at a single metric for vessels grouped by fisheries, states, homeports, or vessel length or 2) compare multiple metrics for a single fishery, state, homeport, or vessel length. When comparing multiple metrics, results do not include data from activites in Alaskan fisheries.'
                            } else {
                              message = 'You are selecting to either 1) look at a single metric for processors grouped by production activities, region, or processor size or 2) compare multiple metrics for a single production activities, region, or processor size.'
                            }
                            )
                            })
observeEvent(input$ivariance, {
  session$sendCustomMessage(type = 'testmessage',
                            message = 'When MEAN is selected, we show one standard deviation about the mean, When MEDIAN is selected, we show the upper and lower quartiles (25th/75th percentiles). We use algorithm 8 from Hyndman and Fan (1996), which is particularly suited to non-normally distributed data. m=(p+1)/3. p_k = (k - 1/3)/(n+1/3). Then p_k =~ median[F(x_k)]. ')
})
observeEvent(input$iwhiting, {
  session$sendCustomMessage(type = 'testmessage'#'JsonObject'
                            ,message= 'See the Definitions Page for a diagram of how vessels are divided into whiting or non-whiting categories.'#
                             # ('www/WhitingFigure.png',200,150,'alt text')
                            )
})
##bsPopover('iwhiting', 'Title', content='test', trigger='click')

scale_height <- function(){
 if(length(input$VariableSelect)<=2){ 
   700 }  else if(length(input$VariableSelect)>2 & length(input$VariableSelect)<=4) { 
     800}  else if(length(input$VariableSelect)>4 & length(input$VariableSelect)<=6) { 
       900} else if(length(input$VariableSelect)>6 & length(input$VariableSelect)<=8) { 
         1000} else if(length(input$VariableSelect)>8 & length(input$VariableSelect)<=10) {
           1100} else { 
             1200 }
}

output$PlotMain <- renderPlot({
  validate(need(
    sum(!is.na(as.numeric(DatSub()$VALUE))) !=0,
    paste(
      'Your selection is invalid. The selected statistic is not calculated for this metric. Please try selecting a different statistic.'
    )
  ))
  doPlot(dat = DatSub(), x = "YEAR", y = "VALUE")
},  height=scale_height, width = "auto")

output$PlotMain2 <- renderPlot({
  validate(need(
    sum(!is.na(as.numeric(DatSub()$VALUE))) !=0,
    paste(
      'Your selection is invalid. The selected statistic may not be calculated for this metric. Please try selecting a different statistic.'
    )
  ))
  input$data
  doPlot(dat = DatSub(), x = "YEAR", y = "VALUE")
},  height=400, width = 700)

output$PlotMain3 <- renderPlot({
  validate(need(
    sum(!is.na(as.numeric(DatSub()$VALUE))) !=0,
    paste(
      'Your selection is invalid. The selected statistic may not be calculated for this metric. Please try selecting a different statistic.'
    )
  ))
  input$data
  doPlot(dat = DatSub(), x = "YEAR", y = "VALUE")
},  height=400, width = 700)

output$TableMain <- renderDataTable({ 
  validate(need(
    sum(!is.na(as.numeric(DatSub()$VALUE))) !=0,
    paste(
      'Your selection is invalid. The selected statistic may not be calculated for this metric. Please try selecting a different statistic.'
    )
  ))

  table <- DatSubTable()
  table
  
})


# download buttons ------------------------------------------------------------
##---Table-----#
##### output$dlTable
output$dlTable <- downloadHandler(
    filename = function() { 'perfmetricsTable.csv' },
    content = function(file) {
      table = as.data.frame(table)
      table <- DatSubTable()
      names(table) <- c(paste(
        "Sourced from the FISHEyE application (http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/) maintained by NOAA Fisheriess NWFSC on ",
        format(Sys.Date(), format="%B %d %Y")), 
        rep("", dim(temp)[2]-1))
      write.csv(table, file)
    })

#####

# render plot from  to pdf for download ####
output$dlFigure <- downloadHandler(
  filename = function() {'perfmetricsPlot.pdf'},
  content = function(file){
     if(!PermitPlot()) return()
   
    pdf(file = file, width=10.25, height=7.5, onefile=T)
    if(length(input$VariableSelect)<=6){
            doPlotDownload(dat = DatSub(), x = "YEAR", y = "VALUE")
    } else {
      dat <- DatSub()
      dat <- subset(dat, VARIABLE %in% input$VariableSelect[1:6])
      doPlotDownload(dat = dat, x="YEAR", y= "VALUE")
      dat <- DatSub()
      dat <- subset(dat, VARIABLE %in% input$VariableSelect[7:length(input$VariableSelect)])
      doPlotDownload(dat = dat, x="YEAR", y= "VALUE")
      }
    dev.off()
 })
#######

#Interactives plots trial #######
output$hover_info <- renderUI({
  if(!is.null(input$plot_hover)){
    dat <- DatSub()
    if(!input$LayoutSelect & length(input$VariableSelect)==1||
       input$LayoutSelect & length(input$econSelect)==1){
      lvls <- levels(as.factor(dat$YEAR))
    } else {
      lvls <- rep(levels(as.factor(dat$YEAR)),2) 
    }
    hover=input$plot_hover

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <-
      (.125+hover$domain$left) / (hover$domain$right - hover$domain$left)
 #     if(hover$x<.89){
 #         (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
 #     } else {
 #         (hover$x-.2 - hover$domain$left) / (hover$domain$right - hover$domain$left) 
 #       }#
#    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    top_pct <- .15#hover$domain$top
    
    # calculate distance from left and bottom side of the picture in pixels
    #left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style1 <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px-16, #left_px -5,
                    "px; top:", top_px + .1, "px;"
                   )
    
    
#  # actual tooltip created as wellPanel
    ###WITHIN PLOT
    wellPanel(
      style = style1,
      if (lvls[round(length(lvls)*input$plot_hover$x)]==2015) {
    tags$div(tags$p('The Blob and El', HTML("Ni&ntilde;o"), 'converge. Biomass is low.',tags$br(),
             'Council establishes catch limits for unfished and unmanaged forage fish.',tags$br(),
             'Canary rockfish and petrale sole are declared rebult.', tags$br(),
             'Dover sole and thornyhead limits increased.'))
      } else if(lvls[round(length(lvls)*input$plot_hover$x)]==2014) {
        tags$div('Fishery participants can buy and sell quota shares.', tags$br(), 
                 'Non-whiting groundfish gets MSC certification',tags$br(),
                 'Russia implements ongoing trade sanctions.')  
      }else if(lvls[round(length(lvls)*input$plot_hover$x)]==2013) {
        paste0('Ban on year-end quota pound transfers is lifted.')  
      }else if(lvls[round(length(lvls)*input$plot_hover$x)]==2012) {
        paste0('Widow rockfish is declared rebuilt.')  
      }else if(lvls[round(length(lvls)*input$plot_hover$x)]==2011) {
        tags$div(HTML("T&#333;hoku earthquake and tsunami hits Japan."), tags$br(), 
                 "Sablefish prices are high.", tags$br(), 
                 "Fishery participants can lease quota pounds.")  
      }else if(lvls[round(length(lvls)*input$plot_hover$x)]==2010) {
        paste0('Petrale sole is declared overfished.')  
      }else if(lvls[round(length(lvls)*input$plot_hover$x)]==2009) {
        tags$div('Sector-specific quota allocation for bycatch species.', tags$br(),
        'Pacific whiting gets MSC certification.')
      }
    )
  }
})

    
    #Interactives plots trial
    #######
    output$click_info <- renderUI({
      if(!is.null(input$plot_click)){
        dat <- DatSub()
        if(!input$LayoutSelect & length(input$VariableSelect)==1||
           input$LayoutSelect & length(input$econSelect)==1){
          lvls <- levels(as.factor(dat$YEAR))
        } else {
          lvls <- rep(levels(as.factor(dat$YEAR)),2) 
        }
        click=input$plot_click
        
        #  # actual tooltip created as wellPanel
        ####BELOW PLOT 
    wellPanel(
      if (lvls[round(length(lvls)*input$plot_click$x)]==2015) {
        tags$div(tags$p('The Blob (first detected in 2013) and El', HTML("Ni&ntilde;o"), 'converge, leading to record low biomass of key prey species for many catch share species; catch attainment is lower for Pacific whiting.', tags$a(href='http://www.nationalgeographic.com/magazine/2016/09/warm-water-pacific-coast-algae-nino/', 'Click here for more details.', target='_blank'),tags$br(),
                        'Council establishes catch limits for unfished and unmanaged forage fish.
                        Dover sole limits increased by over 100% and Longspine thornyhead limits increased by 60%.'))
      } else if(lvls[round(length(lvls)*input$plot_click$x)]==2014) {
        tags$div('Fishery participants can buy and sell quota shares.', tags$br(), 
                 'Russia implements an import ban on agricultural and processed food exports from the USA, EU, Norway, Canada, and Australia.', 
                  tags$a(href='https://www.nytimes.com/2014/08/08/world/europe/russia-sanctions.html','Read this NYTimes article for background.', target='_blank'), 
                  'The ban is still in place.
                 According to a', tags$a(href='http://trade.ec.europa.eu/doclib/docs/2015/december/tradoc_154025.pdf' , 'paper in Chief Economist Note,', target='_blank'), 
                 'the impact of the ban on the US fisheries industry has been minimal.', tags$br(),
                 'Non-whiting groundfish gets Marine Stewardship Council certification.')  
      }else if(lvls[round(length(lvls)*input$plot_click$x)]==2013) {
        paste0('Ban on year-end quota pound transfers is lifted')  
      }else if(lvls[round(length(lvls)*input$plot_click$x)]==2012) {
        paste0('Widow rockfish, a species that was constraining co-occurring target species, is declared rebuilt.')  
      }else if(lvls[round(length(lvls)*input$plot_click$x)]==2011) {
        tags$div(HTML("T&#333;hoku earthquake and tsunami hits Japan."), tags$br(), 
                 "Sablefish prices are high.", tags$br(), 
                 "Fishery participants can lease quota pounds but cannot yet purchase quota shares.")  
      }else if(lvls[round(length(lvls)*input$plot_click$x)]==2010) {
        paste0('Petrale sole is declared overfished.')  
      }else if(lvls[round(length(lvls)*input$plot_click$x)]==2009) {
        paste0('Sector-specific bycatch species quota allocation ends race-to-fish for constraining species between at-sea motherships.
               Pacific whiting gets MSC certification.')  
      }
    )
    
  }
})
#######
#End interactive plots code