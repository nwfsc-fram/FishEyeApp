
##############################
#Contact us
###############################
output$Email <- renderUI({
  
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           h3("Contact us"),
           tags$p("We look forward to receiving feedback and questions.", tags$br()),
                  tags$p("Please email us at", strong("nmfs.nwfsc.fisheye@noaa.gov"),
                         # tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'nwfsc.fisheye@noaa.gov'),
                         tags$br()),
                  
#                  tags$p(
#                    "Lisa Pfeiffer", tags$br(),
#                    "Economist", tags$br(),
#                    "Northwest Fisheries Science Center", tags$br(),tags$br(),

#                    "Melanie Harsch", tags$br(), 
#                    "Contractor-ECS Federal, Inc.", tags$br(),
#                    "In support of NMFS", tags$br(),
#                    "Northwest Fisheries Science Center", tags$br(),

                    tags$br(),
                  tags$hr(),
                  
                  # "You can send comments and questions directly to us by clicking",
 #                 tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'contact us'), 'or by copying our email address', 
 #                 tags$em('nwfsc.fisheye@noaa.gov'), 'and using your favorite email program.',
                  tags$br(),
                  tags$br()
  )#)
})
######################################

####################
#Other applications
###############
output$ApplicationsText <- renderUI({
  tags$div(style="margin:15px 15px 30px; with: 60%",
           tags$br(),
           HTML('<a class="btn btn-primary", href="https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/"
                        style="height:37px;border-radius:25px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
       padding-bottom:10px"> FISHEyE Performance Metrics</a>' ),
           HTML('<a class="btn btn-primary", href="https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/Costs/"
                        style="height:37px;border-radius:25px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
       padding-bottom:10px"> FISHEyE Costs Explorer</a>' )
  )
})
##############################


output$BlogText <- renderUI({

tags$div(style = "margin-top: 15px;margin-bottom:0; width: 80%; ",
         h3("Bulletin Board"),
         tags$p("On this page we will provide information on updates and responses to questions received by email that are of general interest."),
         tags$hr()
)
  }) #        tags$br(),
  #       ,
##########################
##Blog
############################
output$BlogUpdates <- renderUI({  
  tags$div(style = "margin-top:0; padding-top:0;background-color:#F8F8E9;",
           tags$h3("Updates"),
           #Major update - adding other sectors
           
          tags$div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                 border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                 HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>May</span><br />
                      <span class='day' style='font-size:16px;'>15</span><br />
                      <span class='year' style='font-size:11px;line-height: 10px;'>2018</span>")
                 ),
       tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Data updated</span>")), 
       tags$p(tags$br(),"Data are periodically updated. We uploaded 2016 data today. Data were previously updated 1/30/17, 12/02/16, 5/25/16, 4/25/16, 3/7/16, 2/24/16, 2/15/16. 
              On February 24th, we added vessels that reported Alaska as their homeport. 
              These vessels are grouped into the Puget Sound port. Only their activities in a West Coast fishery are included; activities in Alaskan fisheries are not included."),
              tags$hr(),
    div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
       border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Oct</span><br />
            <span class='day' style='font-size:16px;'>12</span><br />
            <span class='year' style='font-size:11px;line-height: 10px;'>2017</span>")
           ),
           p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Bookmarking feature</span>")), 
           p(br(),"We have added a bookmarking feature. The bookmarking feature allows you to return to a previously saved state, share the output of a query on FISHEyE without downloading the plot, continue your data exploration at a later date without having to remember selections, etc. A url will be provided when you click the bookmark button on the top right hand corner of control panel. This url is specific to the selections you made in the control panel."),
           hr(),
           
           tags$div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                     border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Aug</span><br />
                 <span class='day' style='font-size:16px;'>07</span><br />
                 <span class='year' style='font-size:11px;line-height: 10px;'>2017</span>")
           ),
           tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Percentiles around the median</span>")), 
           tags$p(tags$br(),"We have been asked to provide percentiles as a measure of variance. All plots and tables now show the 25th and 75th percentiles when median is selected and the standard deviation when mean is selected."),
           tags$hr(),
           
          

           tags$div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
                 border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                     HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Dec</span><br />
                      <span class='day' style='font-size:16px;'>15</span><br />
                      <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
           ),
           tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Additional sectors added</span>")), 
           tags$p(tags$br(),"Data for motherships, catcher-processors, and first receivers and shorebased processors have been added."),
           tags$hr(),
           
             tags$div( class='date', style='height:45px;width:30px;font-family:Arial;font-weight:bold;background-color:#ffffff;text-align:center;border-top:1px solid #c0c0c0;
                   border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                   HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Feb</span><br />
                        <span class='day' style='font-size:16px;'>8</span><br />
                        <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
         ),
         tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Welcome to the Net Revenue Explorer!</span>")), 
         tags$p(tags$br(),
                "Welcome to the first application of FISHEyE, the Net Revenue Explorer. If you have any difficulties accessing or using this application, please contact us at nwfsc.fisheye@noaa.gov. 
                Your comments will help us improve performance of this application."),
                tags$br(),
        
        tags$hr()#, 
              
       )#,
})


###############################################
#Response to questions
###############################################
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
 tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>When will other sectors be added to the Net Revenue Explorer?</span>")), 
 tags$p(tags$br(),
        "They've been added! We have also added additional metrics to assess the effectiveness and outcomes of the catch share program.
        These metrics are available from the Performance Metrics application accessed through the", tags$a(href="https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye", 'FISHEyE homepage'),'.',
        tags$br()),
 
         tags$hr())
        
   
})

############################################
#Default Plot text
##############################################
output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Summary Plots and Data"),
           tags$p('Visualize', tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'Economic Data Collection (EDC)', target="_blank"), 
                  'summary statistics for revenue, costs, and net revenue of', 
                  tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/documents/EDC_Catcher_Vessel_Report_October_2016.pdf","catcher vessels", target="_blank"), 
                  '(both at-sea and shoreside),', 
                  tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/documents/EDC_Mothership_Report_October_2016.pdf","motherships,", target="blank"), 
                  tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/documents/Catcher_Processor_Report_October_2016.pdf", "catcher-processors,", target="blank"), 'and', 
                  tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/documents/EDC_First_Receiver_Shorebased_Processor_Report_October_2016.pdf", "first receivers and shorebased processors", target="blank"), 'that participate in the', tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", 'West Coast Groundfish Trawl Catch Share Program. ', target="_blank")),
            tags$p(strong("To get started, make at least one selection in each of the fields in the Control Panel.")),
 #          tags$br(),
           HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.8em; margin-top:15px; margin-bottom:-2px;font-size:11.5pt'>
                     <b>Plot Options:</b></div>"),
           
          # tags$p(strong('Plotting Options:', style="padding-bottom:0")),
           tags$ul(tags$li(tags$strong('Economic measures side-by-side:'), 'these plots show economic measures (revenue, costs, and/or net revenue) across years. 
                    A drop-down menu below', tags$em('Plot Options'), 'allows you  to switch between bar, point, and line plots.'), 
            tags$li(tags$strong('Composition of net revenue:'), 'these plots show net revenue (revenue minus costs) as stacked bar plots, in the form of Variable Cost Net Revenue (VCNR) 
                   or Total Cost Net Revenue (TCNR). VCNR is revenue minus variable costs and TCNR is revenue minus variable and fixed costs. 
                      A figure demonstrating how VCNR and TCNR are derived is shown below.')),  
           tags$br(),
          img(src="NetRevGraphic.png", height=450),  #  img(src="EconInd4.png", height=350),       
           tags$br(), 
           tags$br(),
          tags$p(strong('Show Plot(s):'),'The', tags$em('Show Plot(s)'), 'button returns the display to the plot output. Pre- and post- implementation of the catch share program is distinguished with shading.'),
            tags$p(strong('Show Data:'), 'The', tags$em('Show Data'), 'button displays a data table that can be filtered using the', tags$em('Search'), 'box or the boxes at the bottom of the table.'),
          
          
           tags$p(strong('Download Plots and Data:'),'Once selections have been made and a plot or data table is visible, use these buttons to download the plot and the data used to generate the plot.'),
           #tags$p(strong('Download Data:'),'Once selections have been made and a plot is visible, a button to download the data that was used to generate the plot(s) appears in the bottom of the Control Panel.'),
#           tags$p(strong('A note about net revenue:'),  'The', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC survey forms', target="_blank"), 'capture costs directly related to vessel fishing operations. They do not include other expenses 
#            such as vehicles or office costs that may be related to the fishing business. Therefore, the net revenue reported here is an overestimate of the true net revenue.'),
#           tags$br(),
           tags$p(strong('To view these instructions'), 'at any time, visit the', tags$em('Instructions'), 'tab.')
  )
 
})

##########################################
#Thirds text
#########################################
output$DefaultThirdsText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Fleet-wide Variability Analysis"),
       #    tags$p(em('')),
           tags$p(strong(em('Explore the variability within the catcher vessel fleet.'), tags$strong('To do this, we group vessels into three tiers based on the amount of revenue they earned in each year.')
              # Statistics and measures are then shown for each of the three tiers.'
                         )),
            tags$p(strong("To get started, make at least one selection in each of fields in the Control Panel.")),
            tags$p(strong('Background:'), 'Catcher vessels that participate in the', 
                  tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/catch_shares.cfm", target="_blank", "West Coast Groundfish Trawl Catch Share Program"), 
                    'span a very broad range in terms of the scale of their operations. To view the heterogeneity within the catcher vessel fleet while taking into account confidentiality rules,
                    we group the vessels into three tiered categories annually: top, middle, and lower revenue earners.   We then calculate the median, mean, or total of the selected 
                    statistic (per vessel, vessel/day, or vessel/metric ton) for vessels within each tiered category.'), 

           tags$br(),
              img(src="ExampThirds.png", height=375, width=750),tags$br(),tags$br(),tags$br(),tags$br(),
             # tags$p(strong('Download Plots:'), 'Once selections have been made and a plot if visible, a button to download the plot(s) appears at the bottom of the panel on the right.'),

           tags$p(strong('Show Plot(s):'), 'The', tags$em('Show Plot(s)'), 'button returns the display to the plot output. Results are shown if there are at least three vessels in a group. 
                  Pre- and post- implementation of the catch share program is distinguished with shading.'),
                  
  
           tags$p(strong('Show Data:'), 'The', tags$em('Show Data'), 'button displays a data table that can be filtered using the Search box or the boxes at the bottom of the table.'),                                     
           tags$p(strong('Download Plots and Data:'),'Once selections have been made and a plot or data table is visible, use these buttons to download the plot and the data used to generate the plot.'),
                 #These thirds are plotted as lines labeled: "Top Third", "Middle Third" and "Bottom Third". 
                 

           tags$p(strong("Note:"), 'Only a single class of the selected summary variable (fisheries, homeport, state, vessel length class) may be selected at a time.'),
           
           tags$p(strong('To view these instructions'), 'at any time, click the', tags$em('Instructions'), 'tab.')
  )
})