
#
output$ApplicationsText <- renderUI({
  tags$div(style="margin:15px 15px 30px; with: 60%",
           tags$br(),
  HTML('<a class="btn btn-primary", href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/"
                        style="height:37px;border-radius:25px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
       padding-bottom:10px"> FISHEyE Net Revenue Explorer</a>' )
  )
})

output$Email <- renderUI({
  
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           h3("Contact us"),
           tags$p("We look forward to receiving feedback and questions.", tags$br()),
           tags$p(h4("Please email us at", strong("nwfsc.fisheye@noaa.gov")),
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
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Jul</span><br />
                          <span class='day' style='font-size:16px;'>25</span><br />
                          <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
                     ),
           tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>New sectors added</span>")), 
           tags$p(tags$br(),"Two major updates have been made to this application. First, we have added two more sectors - motherships and catcher processors. We are currently working on first receivers and shorebased processors.
                  In addition, we revised the functionality of the application. You can now compare by vessel categories (fisheries, homeports, etc.) or by metrics. 
                  When comparing metrics, the metrics displayed will depend upon both the metrics chosen and the selected statistics."),
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
                  'in 2011. Select whether to view results for catcher vessels, motherships, catch-processors, or first receivers and shorebased processors. Metrics are grouped into three broad indicator categories: demographic, economic, and social. Information on how metrics were chosen can be found in the', tags$em('About'), 'tab.',
                  tags$strong('Information on the individual metrics can be found by clicking the blue information icon above the list of metrics'), 'or in the Definitions tab.' 
                  ),
           tags$p(strong("To get started, make at least one selection in each of the fields in the Control Panel.")),
           #          tags$br(),
           HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.8em; margin-top:15px; margin-bottom:-2px;font-size:11.5pt'>
                <b>Plot Options:</b></div>"),
           
          
           tags$p(strong('Show Plot(s):'),'The', tags$em('Show Plot(s)'), 'button returns the display to the plot output. Pre- and post- implementation of the Catch Share program is distinguished with shading. 
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
           tags$p(strong(paste('Note this tutorial will disappear once you select a',input$CategorySelect))),
           tags$h4("1. Compare groups of vessels or metrics"),
           tags$p(paste('Choose whether to view a single metric for multiple', #'
                      'vessel groups',#  ,input$CategorySelect, 
                      'or view multiple metrics for a single vessel group.'#,input$CategorySelect,'.'
                        )), 
           tags$h4("2. Select General categories to view data by"),
           tags$p("First, select the vessel grouping category (Fisheries, State, Homeport, Vessel length class). Not all categories will be available for all sectors.
                  Second, select whether to show results for all vessels, just vessels that fished for pacific whiting, and/or just vessels that did not fish for pacific whiting."),
           tags$h4(paste("3. Select one more more subgroups.")),
           tags$p(strong('Once this selection is made, this tutorial will disappear.'), paste('Multiple subgroups of the selected vessel grouping category (such as fishery) can be selected if, in step 1, you chose to compare groups of vessels. If you chose to compare metrics, only a single metric can be selected.')),
           tags$h4("4. Select an indicator category"),
           tags$p("This drop down menu divides metrics into three categories:",tags$em('Demographic,'), tags$em("Economic,"),'and',tags$em('Social and regional.'), 'The choice of metrics to be displayed depends on which indicator category is selected.'),
           tags$h4("5. Select a statistic and a metric"),
           tags$p('Which statistic is calculated depends on the metric. If a selected metric is not shown, try a different statistic.'),
           tags$h4('6. Select years and whether or not to include Alaskan fisheries activities'),
           tags$p('The number of years for which data is available and the choice to include activities in Alaskan depend upon the metric and sector selected.')
  )
})
