
#
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

output$BlogText <- renderUI({
  
  tags$div(style = "margin-top: 15px;margin-bottom:0; width: 80%; ",
           h3("Bulletin Board"),
           tags$p("On this page we will provide information on updates and responses to questions received by email that are of general interest."),
           tags$hr()
  )
}) #        tags$br(),
#       ,
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
  tags$p(tags$br(),"We have been asked to provide percentiles as a measure of variance. All plots and tables now show the 25th and 75th percentiles when median is selected and the standard deviation when average is selected."),
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
                  You can select whether or not to show the variance around the average or median values.'),
            tags$br(),
           img(src="indicators_example2.png", height=300),  #  img(src="EconInd4.png", height=350),       
           tags$br(), 
           tags$br(),
             tags$p(strong('Show Data:'), 'The', tags$em('Show Data'), 'button displays a data table that can be filtered using the Search box or the boxes at the bottom of the table.'),
         
           
           tags$p(strong('Download Plots and Data:'),'Once selections have been made and a plot or data table is visible, use these buttons to download the plot and the data used to generate the plot.'),
            tags$p(strong('To view these instructions'), 'at any time, visit the', tags$em('Instructions'), 'tab.')
                   )
  
})

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
           tags$p("Second, for catcher vessels, select whether to show results for all vessels, just vessels that fished for pacific 
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


##--------------- Case Study text:-------------------------###

output$CaseStudy1 <- renderUI({
  tags$div(style = "margin: 15px 15px 30px; width: 90%",
           tags$p("Annual variable cost net revenue (VCNR; a measure of shorter-term operating profit) tends to be greater from trawl activities than from fixed gear activities. This is 
                  evident whether comparing the median (black line in plot below) or average (not shown) vessel or the range in VCNR (25th percentile-representation of a vessel that is 
                  making more than 25 percent of vessels, but less than 75 percent of vessels- and 75th percentile- representative of a vessel making more than 50% of vessels but no more 
                  than 75%- shown), VCNR has been relatively stable at around $50,000. Note, we do not show 2009-2010 as fixed gear levels are not directly comparable because of TNC EFP. 
                  Total cost net revenue (TCNR), a longer-term measure of profitability that includes both fixed and variable costs, is also greater in trawl gear activities."),
            tags$p(
                  "How as net revenue changed since catch share program implementation? For groundfish trawl activities, the median VCNR has been relatively stable at around $100,000 per 
                   year. Similarly, VCNR for lower earners (25th percentile but see THIRDS ANALYSIS) has remained relatively stable. In contrast, for upper earners (75th percentiles). If we 
                  divide vessels based on revenue earned annually, you will see that net revenue (VCNR or TCNR) has increased at the most rapid rate of the top third of revenue earners but 
                  that the lower revenue earners have shown improvements, particularly in TCNR."),
            tags$p(
                  "In contrast to VCNR, TCNR has increased for the entire distribution shown, although for the lower income earners, TCNR was negative in some years. Total cost net revenue 
                  has increased more than variable cost net revenue because of a decrease in the number of vessels participating in this activity or consolidation. As the number of vessels 
                  decreases, fewer vessels are catching a larger proportion of the catch, and incurring fewer fixed costs, leading to greater overall efficiency. In addition to 
                  consolidation, these vessels are fishing fewer days in the groundfish trawl fishery. Although revenue and costs have changed little on an annual basis, revenue per day
                   of fishing has increased, meaning that this fleet is experiencing the same level of profitability despite less effort (fewer fishing days). For fixed gear with a trawl 
                  permit activities, pre- and post-catch shares outcomes are not easily compared as pre-catch shares participation in the fixed gear fishery was through a TNC Exempted Fishing Permit program. 
                  Since 2011, there has been no clear trend in profitability (VCNR, TCNR) or income potential (crew wage). The high variability between years may reflect annual variation 
                  in sablefish prices or that this is a new fisheries activity."),
            tags$p(
                  "Although net revenue has been positive for the median and top-earning (75th percentile represented by upper greyed area) vessels, lower earning vessels (25th percentile 
                  is the lower grey band) reported negative VCNR. The variation in earnings across the fleet is even more evident when we divide vessels into groups based on annual revenue. 
                  Vessels with the lowest revenue tend to report negative VCNR whereas vessels with the highest revenue reported positive VCNR. TCNR was lower, as it takes fixed costs into 
                  account as well. TCNR was also less variable between years compared with VCNR. The peak in 2011 and dip in 2013 observed in VCNR is not present in TCNR, this is because 
                  greater fixed costs in 2011 offset greater revenue and lower fixed costs in 2013 offset lower revenue. Similar to VCNR, TCNR was negative for lower earning vessels. Net 
                  revenue per day shows largely the same pattern as net revenue per year, as days at sea per vessel in the fixed gear fishery has varied but with no clear directional trend.")           
  )
})
##--------------- End Case Study text:-------------------------###
