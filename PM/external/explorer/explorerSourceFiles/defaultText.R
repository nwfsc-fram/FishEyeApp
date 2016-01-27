
#
output$Email <- renderUI({
  
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           h3("Contact us"),
           tags$p("We look forward to receiving feedback and questions.", tags$br()),
                  tags$p("Please email us at", strong("nwfsc.fisheye@noaa.gov"),
                         # tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'nwfsc.fisheye@noaa.gov'),
                         tags$br()),
                  
                  tags$p(
                    "Lisa Pfeiffer", tags$br(),
                    "Economist", tags$br(),
                    "Northwest Fisheries Science Center", tags$br(),

                    tags$br(),tags$br(),
                    "Melanie Harsch", tags$br(), 
                    "Contractor-ECS Federal, Inc.", tags$br(),
                    "In support of NMFS", tags$br(),
                    "Northwest Fisheries Science Center", tags$br(),

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
         tags$div( class='date', style='height:45px;width:30px;font-family:Arial;font-weight:bold;background-color:#ffffff;text-align:center;border-top:1px solid #c0c0c0;
                   border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
                   HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Jan</span><br />
                        <span class='day' style='font-size:16px;'>12</span><br />
                        <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
         ),
         tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Welcome to the Performance Metrics page!</span>")), 
         tags$p(tags$br(),
                "Welcome to the Performance Metrics page. If you have any difficulties accessing or using this application, please contact us at nwfsc.fisheye@noaa.gov. 
                Your comments will help us improve performance of this application."),
                tags$br(),
         

        tags$hr()#, 
     #  tags$div( class='date', style='height:45px;width:30px; font-family:Arial; font-weight:bold;background-color:#ffffff;text-align:center; border-top:1px solid #c0c0c0;
     #            border-left:1px solid #c0c0c0;border-right:1px solid #c0c0c0;position:absolute;z-index:3;line-height: 13px;',
     #            HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Oct</span><br />
     #                 <span class='day' style='font-size:16px;'>28</span><br />
     #                 <span class='year' style='font-size:11px;line-height: 10px;'>2015</span>")
     #            ),
       
     #  tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>What are we currently working on?</span>")), 
     #  tags$p(tags$br(),"We are working on adding other sectors to the Net Revenue Explorer. We are also working on adding 
     #         additional metrics to assess the effectiveness and outcomes of the catch share program.",
     #         tags$hr())
              
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
                  HTML("<span class='month' style='text-transform:uppercase;font-size:11px;'>Jan</span><br />
                        <span class='day' style='font-size:16px;'>15</span><br />
                        <span class='year' style='font-size:11px;line-height: 10px;'>2016</span>")
 ),
 tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Place holder?</span>")), 
 tags$p(tags$br(),
        "Place holder.",
        tags$br()),
 
         tags$hr())
        
   
})


output$ApplicationsText <- renderUI({
  
  tags$div(style = "margin-top: 15px;margin-bottom:0; width: 80%; ",
           h3("FISHEyE Applications"),br(),
           HTML("<div>
                <p style='font-size:150%'><strong><a style='color:white; background-color:#1a5d99;  font-family:Cambria; border-radius:25px; padding:5px;' 
                href='https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer'> Net Revenue Explorer</a></strong></div>"),
           tags$p("Explore and download economic data pre- and post-Catch Share management."),
           tags$hr()
  )
}) #        tags$br(),


output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Summary Plots and Data"),
           tags$p('Visualize', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'Economic Data Collection (EDC)', target="_blank"), 
                  'summary statistics for revenue, costs, and net revenue of', tags$a(href="2012CatcherVessel.jpg","catcher vessels", target="_blank"), 
                  '(both at-sea and shoreside) that participate in the', tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", 'West Coast Groundfish Trawl Catch Share Program. ', target="_blank"), 
                  'To generate plots and analyses beyond what is provided in the Net Revenue Explorer, please download the data and analyze externally.'),
            tags$p(strong("To get started, make at least one selection in each of the fields in the panel on the right. A button above the plot output allows you to switch back and forth between viewing plots and the data.")),
          # tags$br(),
           HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.8em; margin-top:15px; margin-bottom:-2px;font-size:11.5pt'>
                     <b>Plotting Options:</b></div>"),
           
          # tags$p(strong('Plotting Options:', style="padding-bottom:0")),
           tags$ul(tags$li('Economic measures side-by-side: these plots show economic measures (revenue, costs, and/or net revenue) across years for different summary statistics. Click the', 
                      tags$em('Economic measures side-by-side'), 'option in the', tags$em('Plot Options'), 'field in the panel on the right. A drop-down menu below', tags$em('Plot Options'), 'allows you 
                      to switch between bar, point, and line plots.'), 
            tags$li('Composition of net revenue: these plots show revenue, costs, and net revenue (revenue minus costs). You can examine either Variable Cost Net Revenue (VCNR) 
                   or Total Cost Net Revenue (TCNR). VCNR is revenue minus variable costs and TCNR is revenue minus variable and fixed costs. 
                These plots are stacked bar plots. Click', tags$em('Composition of total cost net revenue'), 'or', tags$em('Composition
                      of variable cost net revenue'), ' in the', tags$em('Plot Options'), 'field in the panel on the right to see these plots. 
                      A figure demonstrating how VCNR and TCNR are derived is shown below.')),  
           tags$br(),
          img(src="NetRevGraphic.png", height=450),  #  img(src="EconInd4.png", height=350),       
           tags$br(), 
           tags$br(),
          tags$p(strong('Plot Output:'),'The Catch Share program began in 2011. In all plots, we distinguish between pre- and post- implementation of the Catch Share program with shading.'),
            tags$p(strong('Data Table:'), 'After clicking the', tags$em('Show Data'), 'button, the data will be displayed and can be filtered across the data table using the Search box or filtered within a column using the boxes at the bottom of the table.',
                  tags$br(),'We provide a measure of the variance around the average and median values. For average values, we report the standard deviation. For median values, we report the median absolute deviation.'),
          
          
           tags$p(strong('Download Plots and Data:'),'Once selections have been made and a plot or data table is visible, a button to download the plot(s) and the data used to generate the plot(s) appears at the bottom of the panel on the right.'),
           #tags$p(strong('Download Data:'),'Once selections have been made and a plot is visible, a button to download the data that was used to generate the plot(s) appears in the bottom panel on the right.'),
           tags$p(strong('A note about net revenue:'),  'The', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC survey forms', target="_blank"), 'attempt to capture only costs that are directly related to vessel fishing operations. They do not include other expenses 
            such as vehicles or office costs that may be related to the fishing business. Therefore, the net revenue reported here is an overestimate of the true net revenue.'),
           tags$br(),
           tags$p(strong('To view these instructions'), 'at any time visit the', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )
 
})

output$DefaultTableText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Data Table"),
           tags$p('View data used to generate the plot(s) created in the', tags$em('Visualize Data with Plots'), 'tab.'),
                  tags$p(strong('To get started, make at least one selection in each of the fields in the panel on the right.')), 
            tags$p('After a data table has been displayed, the data can be further filtered using the ',tags$em('Search'), 'box, 
                     or filtered within a column using the boxes at the bottom of the table.'),
           tags$p('We provide a measure of the variance around the average and median values. For average values, we report the standard deviation. For median values, we report the median absolute deviation.'),
                 tags$p( strong('Download Data: '), 'Once selections have been made, a button to download the data table appears at the bottom of the panel to the right.'),
           tags$br(),
           tags$p(strong('To view these instructions'), 'at any time, visit the ', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )    
})

output$DefaultTableTextThirds <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Data Table"),
           tags$p('View data used to generate the plot(s) created in the', tags$em('Fleet-wide Variability Analysis'), 'tab.'),
           tags$p(strong('To get started, make one selection in each of the fields in the panel on the right.')), 
           tags$p('After a data table has been displayed, the data can be further filtered using the ',tags$em('Search'), 'box, 
                     or filtered within a column using the boxes at the bottom of the table.'),
           tags$p('We provide a measure of the variance around the average and median values. For average values, we report the standard deviation. For median values, we report the median absolute deviation.'),
           tags$p( strong('Download Data: '), 'Once selections have been made, a button to download the data table appears at the bottom of the panel to the right.'),
           tags$br(),
           tags$p(strong('To view these instructions'), 'at any time visit the ', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )    
})


output$DefaultThirdsText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Fleet-wide Variability Analysis"),
           tags$p(em('Explore the variability within the catcher vessel fleet.')),
           tags$p('In order to explore the variability within the catcher vessel fleet, we group vessels into three tiers based on the amount of revenue they earn.
               Statistics and measures are then shown for each of the three tiers.'),
            tags$p(strong("To get started, make at least one selection in each of fields in the panel on the right. A button above the plot output allows you to switch back and forth between viewing plots and the data.")),
            tags$p(strong('Background:'), 'Catcher vessels that participate in the', 
                  tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/catch_shares.cfm", target="_blank", "West Coast Groundfish Trawl Catch Share Program"), 
                    'span a very broad range in term of the scale of their operations. 
                   For instance, the revenue earned ranges from around $10,000 to well over $1,500,000.  The purpose of the', tags$em('Fleet-wide Variability Analysis'), 'is to show how this heterogeneity 
                   relates to their economic performance. We do not show the economic performance for each individual vessel due to confidentiality rules, 
                    so we group the vessels into three tiered categories: top, middle, and lower revenue earners. We then calculate the median or average of  
                    the selected statistic (per vessel, vessel/day, or vessel/metric-ton) for vessels within each tiered category. This is done for each year separately.'), 
         
           tags$p('The Catch Share program was implemented in 2011. In all plots, we distinguish between pre- and post- implementation of the Catch Share program with shading.'),
           tags$br(),
              img(src="ExampThirds.png", height=375, width=750),tags$br(),tags$br(),tags$br(),tags$br(),
             # tags$p(strong('Download Plots:'), 'Once selections have been made and a plot if visible, a button to download the plot(s) appears at the bottom of the panel on the right.'),

              tags$p(strong('Data Table:'), 'After clicking the', tags$em('Show Data'), 'button, the data will be displayed and can be filtered across the data table using the Search box or filtered within a column using the boxes at the bottom of the table.'),                                     
           tags$p(strong('Download Plots and Data:'),'Once selections have been made and a plot or data table is visible, a button to download the plot(s) and the data used to generate the plot(s) appears at the bottom of the panel on the right.
                  Results are shown if there are at least three vessels in a group. Results are plotted as a dot plot if a single year is selected or a line plot if multiple years are selected.'),
                 #These thirds are plotted as lines labeled: "Top Third", "Middle Third" and "Bottom Third". 
                 

           tags$p(strong("Note:"), 'Only a single class of the selected summary variable (fisheries, homeport, state, vessel length class) may be selected at a time.'),
           
           tags$p(strong('To view these instructions'), 'at any time, click the', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )
})