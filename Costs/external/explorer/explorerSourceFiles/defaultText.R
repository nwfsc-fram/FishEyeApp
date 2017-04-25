
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

output$ApplicationsText <- renderUI({
  tags$div(style="margin:15px 15px 30px; with: 60%",
           tags$br(),
           HTML('<a class="btn btn-primary", href="https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/"
                        style="height:37px;border-radius:25px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
                padding-bottom:10px"> FISHEyE Net Revenue Explorer</a>' ),
           HTML('<a class="btn btn-primary", href="https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/PerformanceMetrics/"
                        style="height:37px;border-radius:25px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
                padding-bottom:10px"> FISHEyE Performance Metrics</a>' )
  )
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
                        <span class='day' style='font-size:16px;'>08</span><br />
                        <span class='year' style='font-size:11px;line-height: 10px;'>2017</span>")
         ),
         tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Welcome to the Net Revenue Explorer!</span>")), 
         tags$p(tags$br(),
                "Welcome to the Costs Explorer. If you have any difficulties accessing or using this application, please contact us at nwfsc.fisheye@noaa.gov. 
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
                        <span class='year' style='font-size:11px;line-height: 10px;'>2017</span>")
 ),
 tags$p(HTML("<span style='margin-left:60px;font-size:18px;font-weight:bold'>Check out other FISHEyE applications.</span>")), 
 tags$p(tags$br(),
        "This is the third FISHEyE application. The other applications can be access through the", tags$a(href="https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye", 'FISHEyE homepage'),'.',
        tags$br()),
 
         tags$hr())
        
   
})


output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Summary Plots and Data"),
           tags$p('Visualize', tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'Economic Data Collection (EDC)', target="_blank"), 
                  'summary statistics for costs (variable and fixed) of vessels and processors that participate in the', 
                   tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", 'West Coast Groundfish Trawl Catch Share Program. ', target="_blank"), 
                  'Select whether to view results for', tags$a(href="2012CatcherVessel.jpg","catcher vessels", target="_blank"), 
                  '(both at-sea and shoreside), motherships, catcher-processors, or first receivers and shorebased processes.'),
            tags$p(strong("To get started, make at least one selection in each of the fields in the Control Panel.")),
 #          tags$br(),
           HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.8em; margin-top:15px; margin-bottom:-2px;font-size:11.5pt'>
                     <b>Plot Options:</b></div>"),
           
          # tags$p(strong('Plotting Options:', style="padding-bottom:0")),
          
          tags$p(strong('Show Plot(s):'),'The', tags$em('Show Plot(s)'), 'button returns the display to the plot output. Pre- and post- implementation of the catch share program is distinguished with shading.'),
             tags$br(),
          img(src="CostsFig.png", height=450),  #  img(src="EconInd4.png", height=350),       
           tags$br(), 
           tags$br(),
          tags$p(strong('Show Data:'), 'The', tags$em('Show Data'), 'button displays a data table that can be filtered using the Search box or the boxes at the bottom of the table.'),
          
          
           tags$p(strong('Download Plots and Data:'),'Once selections have been made and a plot or data table is visible, use these buttons to download the plot and the data used to generate the plot.'),
           #tags$p(strong('Download Data:'),'Once selections have been made and a plot is visible, a button to download the data that was used to generate the plot(s) appears in the bottom of the Control Panel.'),
#           tags$p(strong('A note about net revenue:'),  'The', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC survey forms', target="_blank"), 'capture costs directly related to vessel fishing operations. They do not include other expenses 
#            such as vehicles or office costs that may be related to the fishing business. Therefore, the net revenue reported here is an overestimate of the true net revenue.'),
#           tags$br(),
           tags$p(strong('To view these instructions'), 'at any time, visit the', tags$em('Instructions'), 'tab.')
  )
 
})




output$GetStartedText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$p(strong(paste('Note this tutorial will disappear once you select a',input$CategorySelect,'. Selections can be made in any order.'))),
           tags$h4("1. Select a sector"),
           tags$p("Select whether to show results for catcher vessels, mothership vessels, catcher-processor vessels, or first receivers and shorebased processors."),
           tags$h4("2. Select general categories to view data by"),
           tags$p("First, select the vessel or processor grouping category (Fisheries, State of homeport, Homeport, Vessel length class, Production activities, Region, Processor size). 
                  Categories will depend upon the selected sector.",
                  br(),br(),
                  "Second, for catcher vessels, select to show results for all vessels, just vessels that fished for pacific whiting, 
                  or just vessels that did not fish for pacific whiting. For first receivers and shorebased processors, select to 
                  show results for all processors, just processors that processed whiting, or just processors that processed non-whiting 
                  species. For motherships and catcher-processors, only the Pacific whiiting category is available."),
           tags$h4(paste("3. Select one more more subgroups")),
           tags$p(strong('This tutorial will disappear once this selection is made. Selections can be made in any order.'), 
                  paste('Multiple subgroups of the selected vessel or processor grouping category (such as fishery) can be selected if, in step 1, you chose to compare groups of vessels or processors.')),
           tags$h4("4. Select a statistic"),
           tags$p('Three statistics are calculated: average, median, and fleet- or processor-wide total.'),
           tags$h4("5. Select a cost category"),
           tags$p("This drop down menu allows you to choose between showing:",tags$em('All cost categories,'), tags$em("Variable costs,"),'or',tags$em('Fixed costs.'), 
                  'Then select a cost to display.'),
           tags$h4('6. Select years')#,
           #tags$p('The choice to include activities in Alaskan fisheries depend upon the sector selected.')
  )
})