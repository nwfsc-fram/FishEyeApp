
###--------Links to other fisheye apps----------------####
output$ApplicationsText <- renderUI({
  tags$div(style="margin:15px 15px 30px; with: 60%",
           tags$br(),
  HTML('<a class="btn btn-primary", href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/"
                        style="height:37px;border-radius:25px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
       padding-bottom:10px"> FISHEyE Net Revenue Explorer</a>' ),
  HTML('<a class="btn btn-primary", href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/Costs/"
                        style="height:37px;border-radius:25px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
       padding-bottom:10px"> FISHEyE Costs Explorer</a>' )
  )
})

###--------------Email----------------####
output$Email <- renderUI({
  
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           h3("Contact us"),
           tags$p("We look forward to receiving feedback and questions.", tags$br()),
           tags$p(h4("Please email us at", strong("nmfs.nwfsc.fisheye@noaa.gov")),
                  # tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'nwfsc.fisheye@noaa.gov'),
                  tags$br()),
           
           tags$p(
             "Lisa Pfeiffer", tags$br(),
             "Economist", tags$br(),
             "Northwest Fisheries Science Center", tags$br(),tags$br(),
             #                    "nwfsc.fisheye@noaa.gov", 
             
             "Melanie Harsch", tags$br(), 
             "Contractor-ECS Federal, Inc.", tags$br(),
             "In support of NMFS", tags$br(),
             "Northwest Fisheries Science Center", tags$br(),
             #                    "nwfsc.fisheye@noaa.gov", tags$br(),
             tags$br(),
             tags$hr(),
             
             # "You can send comments and questions directly to us by clicking",
             #                 tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'contact us'), 'or by copying our email address', 
             #                 tags$em('nwfsc.fisheye@noaa.gov'), 'and using your favorite email program.',
             tags$br(),
             tags$br()
           ))
})

###--------Blog page intro text--------------####
output$BlogText <- renderUI({
  
  tags$div(style = "margin-top: 15px;margin-bottom:0; width: 80%; ",
           h3("Bulletin Board"),
           tags$p("On this page we will provide information on updates and responses to questions received by email that are of general interest."),
           tags$hr()
  )
}) #        tags$br(),
#       ,
###-------------------------Blog response-----------------------####
output$BlogUpdates <- renderUI({  
  tags$div(style = "margin-top:0; padding-top:0;background-color:#F8F8E9;",
           tags$h3("Updates"),
  #Major update - adding other sectors

  tags$div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
            HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>July</span><br />
                 <span class='day' style='font-size:16px;'>12</span><br />
                 <span class='year' style='font-size:11px;line-height: 10px;'>2017</span>")
  ),
  tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Percentiles around the median</span>")), 
  tags$p(tags$br(),"We have been asked to provide percentiles as a measure of variance. All plots and tables now show the 25th and 75th percentiles when median is selected and the standard deviation when mean is selected."),
  tags$hr(),
  

  tags$div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
            HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Jun</span><br />
                 <span class='day' style='font-size:16px;'>23</span><br />
                 <span class='year' style='font-size:11px;line-height: 10px;'>2017</span>")
            ),
  tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Updates to economic metrics calculations</span>")), 
  tags$p(tags$br(),"We have updated the analysis of the economic metrics. We now provide the fleet-wide or industry-wide average rather than the 
         total for rate measurements (per day, per metric-ton). The fleet-wide average is calculated as the sum of the economic measure divided by the
         sum of days at sea or metric-tons."),
  tags$hr(),

  tags$div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
            HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Dec</span><br />
                 <span class='day' style='font-size:16px;'>02</span><br />
                 <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
            ),
  tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>New sectors!</span>")), 
  tags$p(tags$br(),"We have added motherships, catcher-processors, and first receivers and shorebased processors."),
  tags$hr(),
  
tags$div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Nov</span><br />
                          <span class='day' style='font-size:16px;'>02</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
                     ),
           tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Improved functionality!</span>")), 
           tags$p(tags$br(),"Several major updates have been made to this application. We have provided updated data for catcher vessels.
                  In addition, we revised the functionality of the application. You can now compare by vessel or by metrics. 
                  When comparing metrics, the metrics displayed will depend upon the selected statistics."),
           tags$hr(),
  # Data updates
           tags$div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Jun</span><br />
                          <span class='day' style='font-size:16px;'>20</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
                     ),
           tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Data updated</span>")), 
           tags$p(tags$br(),"Data are periodically updated. Please check back here for updates on data."),
           tags$hr(),
  # Welcome to fisheye
           tags$div( class='date', style='height:45px;width:30px;font-family:Arial;font-weight:bold;background-color:#ffffff;text-align:center;border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Jun</span><br />
                          <span class='day' style='font-size:16px;'>20</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
                     ),
           tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Welcome to the Performance Metrics application!</span>")), 
           tags$p(tags$br(),
                  "Welcome to FISHEyE Performance Metrics. If you have any difficulties accessing or using this application, please contact us at nwfsc.fisheye@noaa.gov. 
                  Your comments will help us improve performance of this application."),
           tags$br(),
           
           tags$hr()#, 
           
           )#,
})
#    tags$p("10.21.15"),
#              tags$ul(   tags$li("Started this blog"),
#     tags$li("Added a", tags$em('clear all'), 'button')),
#   tags$p("10.28.15"),
#   tags$ul(
#     tags$li("Restructured this bulletin board.")
#  ),
#        HTML("<hr color='red'/>"),

###--------------------FAQ--------------------####
output$BlogResponses <- renderUI({    
  tags$div(style = "background-color:#F8F8E9",
           tags$h3("Responses to questions"),
           #       tags$ul(
           #          tags$li("We look forward to receiving feedback and questions. Please send questions and feedback to", tags$strong('nwfsc.fisheye@noaa.gov.'))
           #       ),
           tags$div( class='date', style='height:45px;width:30px;font-family:Arial;font-weight:bold;background-color:#ffffff;text-align:center;border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>July</span><br />
                          <span class='day' style='font-size:16px;'>12</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2017</span>")
           ),
           tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>What exactly is the proportion of revenue from catch share fishery showing?</span>")), 
           tags$p(tags$br(),
                  'Vessels participate in multiple fisheries. With this metric you are subsetting to vessels that participated in the selected fishery. The presented values are the proportion of revenue from ALL catch share fisheries for the subsetted vessels. You can further subset to vessels that did not fish for whiting or vessels that did fish for whiting.',
                  tags$br()),
           

   tags$div( class='date', style='height:45px;width:30px;font-family:Arial;font-weight:bold;background-color:#ffffff;text-align:center;border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Feb</span><br />
                          <span class='day' style='font-size:16px;'>17</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
                     ),
           tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Have port groups been combined?</span>")), 
           tags$p(tags$br(),
                  'For many homeports, there are relatively few vessels that fish for groundfish. To maximize the amount of data we can display while retaining geographic variation, we have aggregated vessles into port groups.  For instance, vessels that reported Morro Bay and Monterey ports as their homeports have been grouped in a single homeport.',
                  tags$br()),
           
           tags$div( class='date', style='height:45px;width:30px;font-family:Arial;font-weight:bold;background-color:#ffffff;text-align:center;border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Feb</span><br />
                          <span class='day' style='font-size:16px;'>8</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
                     ),
           tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Check out other FISHEyE applications. </span>")), 
           tags$p(tags$br(),
                  "This is the second FISHEyE application. The first application, the Net Revenue Explorer, can be accessed through the", tags$a(href="https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye", 'FISHEyE homepage'),'.',
                  tags$br()),
           
           tags$hr())
  
  
})


###--------------Default Plot Text----------------------####
output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Summary Plots and Data"),
           tags$p('The performance metrics in this web application provide a set of measures that can be used to gauge and track changes in the West Coast groundfish trawl limited entry fishery  following implementation of the',
                  tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", 'West Coast Groundfish Trawl Catch Share Program ', target="_blank"),
                  'in 2011. Select whether to view results for catcher vessels, motherships, catcher-processors, or first receivers and shorebased processors. Metrics are grouped into three broad indicator categories: demographic, economic, and social. Information on how metrics were chosen can be found in the', tags$em('About'), 'tab.',
                  tags$strong('Information on the individual metrics can be found by clicking the blue information icon above the list of metrics'), 'or in the Definitions tab.' 
                  ),
           tags$p(strong("To get started, make at least one selection in each of the fields in the Control Panel.")),
           #          tags$br(),
           HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.8em; margin-top:15px; margin-bottom:-2px;font-size:11.5pt'>
                <b>Plot Options:</b></div>"),
           
          
           tags$p(strong('Show Plot(s):'),'The', tags$em('Show Plot(s)'), 'button returns the display to the plot output. Pre- and post- implementation of the catch share program is distinguished with shading. 
                  You can select whether or not to show the variance around the mean or median values.'),
            tags$br(),
           img(src="indicators_example2.png", height=300),  #  img(src="EconInd4.png", height=350),       
           tags$br(), 
           tags$br(),
             tags$p(strong('Show Data:'), 'The', tags$em('Show Data'), 'button displays a data table that can be filtered using the Search box or the boxes at the bottom of the table.'),
         
           
           tags$p(strong('Download Plots and Data:'),'Once selections have been made and a plot or data table is visible, use these buttons to download the plot and the data used to generate the plot.'),
            tags$p(strong('To view these instructions'), 'at any time, visit the', tags$em('Instructions'), 'tab.')
                   )
  
})

###--------------Get Started Text----------------------####
output$GetStartedText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$p(strong(paste('Note this tutorial will disappear once you select a',input$CategorySelect,'. Selections can be made in any order.'))),
           tags$h4("1. Select a sector."),
           tags$p("Select whether to show results for catcher vessels, mothership vessels, catcher-processor vessels, or first receivers and shorebased processors."),
           tags$h4("2. Compare groups of vessels/processors or metrics"),
           tags$p('Choose whether to view a single metric for multiple vessel or processor groups or view multiple metrics for a single vessel or processor group.'), 
           tags$h4("3. Select general categories to view data by"),
           tags$p("First, select the vessel or processor grouping category (Fisheries, State, Homeport, Vessel length class, Production activities, Region, Processor size). 
                  Categories will depend upon the selected sector."),
           tags$p("Second, for catcher vessels, select whether to subset the data by whether", tags$a(href='whiting figure.png','vessels fished for whiting or not.', target='_blank'), "You can show results for all vessels, just vessels that fished for pacific 
                  whiting, and/or just vessels that did not fish for pacific whiting.
                  For first receivers and shorebased processors, select whether to show results for all processors, just processors that 
                  process whiting, and/or just processors that process non-whiting species. For motherships and catcher-processors, only 
                  the pacific whiiting category is available."),
           tags$h4(paste("4. Select one more more subgroups.")),
           tags$p(strong('Once this selection is made, this tutorial will disappear. This selection can be made last.'), 
                  paste('Multiple subgroups of the selected vessel or processor grouping category (such as fishery) can be selected if, in step 1, you chose to compare groups of vessels or processors. 
                        If you chose to compare metrics, only a single subgroup can be selected.')),
           tags$h4("5. Select an indicator category"),
           tags$p("This drop down menu divides metrics into three categories:",tags$em('Demographic,'), tags$em("Economic,"),'and',tags$em('Social and regional.'), 'The choice of metrics to be displayed depends on which indicator category is selected.'),
           tags$h4("6. Select a statistic and a metric"),
           tags$p('Which statistic is calculated depends on the metric. If a selected metric is not shown, try a different statistic.'),
           tags$h4('7. Select years')#,
          # tags$p('The number of years for which data is available and the choice to include activities in Alaskan fisheries depend upon the metric and sector selected.')
  )
})


###--------------- Case Study text:-------------------------####
#1st paragraph
output$CaseStudyp1 <- renderUI({
         tags$div(style = "margin: 15px 15px 30px; width: 90%",
           tags$p(tags$em('(All plots displayed can be generated using FISHEyE, see italics for directions on how to create figure.)')),
          tags$p("In the first five years of the catch share program, annual variable cost net revenue (VCNR; a measure of shorter-term operating profit) has generally been greater from trawl gear non-whiting 
                    groundfish activities than from fixed gear activities. As shown in the figure below, using the Performance Metrics module, this is the case whether comparing the median vessel (black line in plot 
                  below), or the range of VCNR (in grey), in all years except 2011. In 2015, the difference in VCNR between the two fishing activities was smaller than in previous years. The 
                  range (in grey) shows outcomes between the 25th and 75th percentiles (a statistical representation of vessels that make more than the lowest 25% of vessels and less than the highest 25%).",
                 "The 25th percentile of groundfish fixed gear with trawl endorsement VCNR was negative in 2013-2015, meaning that about 25% of vessels participating in this activity were not making an operating 
                 profit in their fixed gear with trawl endorsement operations (they may have profits in other activities, however).",
                 tags$em("Using the selection panel on the right, you can toggle between the median and mean outcomes (under 'Statistic'), which illustrates a similar trend."),
                 "Note that we do not compare 2009-2010; the outcomes of fixed gear vessels fishing in 2009-2010 are not directly comparable because they were fishing under the Nature Conservancy Exempted 
                  Fishing Permit. Total cost net revenue (TCNR), a longer-term measure of profitability that includes both fixed and variable costs, is also greater in trawl gear activities.", 
                tags$em("Use the selection panel on the right to display TCNR (under 'Economic Measures').")))
})
#2nd paragraph
    output$CaseStudyp6 <- renderUI({
      tags$div(style = "margin: 15px 15px 30px; width: 90%",
          tags$p("To more fully understand the differences between the two fisheries activities, we can also look at the amount of effort (days at sea) and per-day profit levels in each.", 
          tags$em("See the plot below or use the selection panel on the right. In the selection panel, click on the Demographic option under 'Select an indicator category', then select Days at Sea under
                    'Select a metric', and select Median under 'Statistic'. The figure will show in the uppermost plot on this page."),
          "Vessels spend more days fishing for groundfish with trawl gear in the catch share fishery than they do fishing with fixed gear.
          The distribution is also broader, meaning that there is greater variation in the number of days fished with trawl gear. At the 75th percentile, vessels spend about 75 days per year fishing 
          groundfish with trawl gear, compared to about 40 days with fixed gear."),
          tags$p( #
          
                 "Per day at sea, the median VCNR is again higher for trawl gear activities, with the exception of 2011, when sablefish prices were very high.", 
                 tags$em("To view VCNR per day, open the figure below or click on Economic under 'Select an indicator category' and select VCNR under 'Economic measures', and then select Median per vessel/day under 'Statistic'."), 
                  "However, the distribution of outcomes is much larger for fixed gear activities. This means that for trawl gear activities, the majority of vessels  (those vessels within the 25th to 75th percentiles)
                 are making between $2,000 and $5,000 of variable cost net revenue per day fishing (in 2015). For fixed gear activities, the vessels between the 25th and 75th percentile are making between -$300 and
                 $5,000 per day.")
    )
  })
#Third paragraph
    output$CaseStudyp2 <- renderUI({
        tags$div(style = "margin: 15px 15px 30px; width: 90%",
                  tags$p(tags$strong(tags$h4("How has net revenue from trawl gear non-whiting groundfish activities changed since catch share program implementation?"))),
                 tags$p("For  trawl gear non-whiting groundfish activities, the median VCNR", tags$em('(see uppermost plot)'), "has been relatively stable at around $100,000 per 
                   year and VCNR at the 25th percentile has also remained relatively stable. In contrast, TCNR has increased over the time 
period shown, although TCNR was negative for the lowest 25% of earners in some years",
                        tags$em('(click on TCNR in the panel on the right to see this figure in the uppermost plot above).'),"TCNR has increased more than VCNR because of 
consolidation, meaning that there has been a decrease in the number of vessels  fishing",
tags$em("(click on  Demographic under 'Select an indicator category' and then select the  Number of Vessels metric in the selection panel on the right to see this figure in the plot above).")," 
As the number of vessels decreases, fewer vessels are catching a larger proportion of the catch and incurring fewer fixed costs, leading to greater overall efficiency. In addition to 
consolidation, these vessels are fishing fewer days in the groundfish trawl fishery, which likely leads to lower costs and greater efficiency. Although revenue and costs have changed relatively little on an annual basis, revenue per day
of fishing has increased, meaning that the fleet is experiencing the same level of profitability despite less effort (fewer fishing days).", tags$em("To view this figure in the uppermost plot, click on Economic under 'Select an indicator 
                                                                                                                                                     category', Revenue under 'Economic measures', and Median per vessel/day under 'Statistic'.")
),

                  tags$p("There is considerable heterogeneity within the catch share fishery. For instance, in 2015 the median revenue per vessel within the trawl gear fishery ranged between $132,000 (25th percentile) and $657,000 (75th percentile). To view the heterogeneity within the fishery, we group vessels anually into three tiered categories (33.3rd, 66.6th and 100th percentiles)
                  based on revenue earned", tags$em('(see plot below).'), "For the top third of revenue earners, median net revenue (VCNR or TCNR) per vessel has increased over time.  
                  For the middle revenue earners, median VCNR has fluctuated around $100,000 per vessel per year and VCNR has slightly increased from $41,000 in 2009 to $70,000 in 2015. 
                  However, the lower third of revenue earners have not reported increased net revenue Median VCNR per vessel has fluctuated around $29,000 per year and median TCNR per vessel has fluctuated around $0 per year.
                         This plot shows that net revenue has increased more for the highest revenue earners than the lower revenue earners."), 
                  tags$em("This figure came from the Thirds Analysis in the "), 
tags$a(href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/",  " Net Revenue Explorer.", target="_blank"))
})
#  output$CaseStudyp3 <- renderUI({            
#        tags$div(style = "margin: 15px 15px 30px; width: 90%",tags$p(

#  })
#  output$CaseStudyp4 <- renderUI({            
#        tags$div(style = "margin: 15px 15px 30px; width: 90%",
                 #tags$p("For fixed gear with a trawl permit activities, there has been no clear trend in profitability (VCNR, TCNR) or 
                 # income potential (crew wage) since 2011.", 
                 # tags$em('See the first plot on this page or use then panel on the right to show these figures. Crew wage is under the Social and Regional Indicator Category.'), 
                 # "The high variability between years may reflect annual variation in sablefish prices or that this is a new 
                 #  fisheries activity."),
#                 tags$p("Net revenue has differed between revenue earners", tags$em('(Thirds Analysis, see plot above).'), 
#          "Vessels are divided annually into 33.3rd, 66.6th and 99.9th percentile groups based on revenue. 
#                  Vessels in the lowest revenue group have reported negative VCNR whereas vessels in the highest revenue group reported positive VCNR. TCNR was lower, as it takes fixed costs into 
#                  account as well. TCNR was also less variable between years compared with VCNR. The peak in 2011 and dip in 2013 observed in VCNR is not present in TCNR, this is because 
#                  greater fixed costs in 2011 offset greater revenue, and lower fixed costs in 2013 offset lower revenue. Similar to VCNR, TCNR was negative for lower earning vessels.")           
#  
#)
#    })
  output$CaseStudyp5 <- renderUI({            
    tags$div(style = "margin: 15px 15px 30px; width: 90%",
             tags$p("Changes in costs, which affect net revenue, can be further explored using the", tags$a(href='','Costs Explorer Application.', target="_blank"),
             "In the", tags$em('plot below,'), "we show the fleet-wide average expense per dollar of revenue for all fixed costs combined along with three individual fixed cost categories. 
             The statistic is calculated as the sum of the costs across the fleet divided by the sum of revenue across the fleet and is useful to evaluate how costs affect net revenue.
             Net revenue tends to be lower when fleet-wide average cost per dollar of revenue is higher.
             For the groundfish trawl gear fishery, all fixed costs (darkest blue line) have decreased over time. Fishing gear and other fixed costs have remained relatively stable over time. 
             On-board equipment (includes machinery and equipment not used to harvest or process fish) can include large costs such as motors and new vessels and has varied, and was highest in 2012. 
             This corresponds with a dip in TCNR in 2012. Variable costs (all or individual categories) fluctuated little over the time frame in which data is available, suggesting that changes in 
             net revenue have resulted primarily from changes in revenue and on-board equipment costs."       
             ))})
  

  output$CaseStudyFig2 <- renderUI({
    tags$img(src="TrawlThirds.png", height=325)
  })
  output$CaseStudyFig3 <- renderUI({  
    tags$img(src="TCNRTrawl.png", height=350)
  })
  
  output$CaseStudyFig3 <- renderUI({  
    tags$img(src="FixedTrawlThirds.png", height=320)
  })
  output$CaseStudyFig4 <- renderUI({  
    tags$img(src="TrawlCosts.png", height=325)
  })
  output$CaseStudyFig5 <- renderUI({  
    tags$img(src="TrawlDAS.png", height=525)
  })
  
##--------------- End Case Study text:-------------------------###
