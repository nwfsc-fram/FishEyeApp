
#
output$Email <- renderUI({
  
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           h3("Contact us"),
           tags$p("We look forward to receiving feedback and questions. You can send comments and questions directly to us by clicking",
                  tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'contact us'), 'or by copying our email address', 
                  tags$em('nwfsc.fisheye.noaa.gov'), 'and using within your favorite email program.') 
  )
})

output$BlogText <- renderUI({

tags$div(style = "margin: 15px 15px 30px; width: 60%",
         h3("Bulletin Board"),
         tags$p(strong("On this page we will provide information on updates and responses to questions received by email that are of general interest.")),
         tags$br(),
         tags$h4("Updates and information"),
         tags$ul(
           tags$li("10.21.15  Started this blog"),
           tags$li("10.21.15  Added a", tags$em('clear all'), 'button'),
           tags$li("10.22.15  Plots are now on a 2 column grid. Height of plot output scales with the number of input variables.")
         ),
         tags$br(),
         tags$h4("Responses to questions"),
         tags$ul(
           tags$li("We look forward to receiving feedback and questions. Please send questions and feedback to", tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'nwfsc.fisheye@noaa.gov.'))
         )
)    
})


output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Visualize Data with Plots"),
           tags$p('Visualize', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'Economic Data Collection (EDC)', target="_blank"), 
                  'summary statistics for revenue, costs and net revenue of', tags$a(href="http://www.nwfsc.noaa.gov/news/features/infographics/Catcher%20Vessel.jpg","catcher vessels", target="_blank"), 
                  'that participate in the', tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", 'West Coast Groundfish Trawl Catch Share Program.', target="_blank")),
            tags$p(strong("To get started, make at least one selection in each of the fields in the panel on the right.")),
           
           HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.62em; margin-top:5px; margin-bottom:-2px;font-size:12pt'>
                     <b>Plotting Options:</b></div>"),
           
          # tags$p(strong('Plotting Options:', style="padding-bottom:0")),
           tags$li('Economic measures side-by-side: these plots show economic measures (revenue, costs, and/or net revenue) across years and fisheries. Click the', 
                      tags$em('Economic measures side-by-side'), 'option in the', tags$em('Plot Options'), 'field in the panel on the right. A drop down menu below', tags$em('Plot Options'), 'allows users 
                      to change between bar, point, and line plots.'), 
            tags$li('Composition of net revenue: these plots show costs relative to revenue and the difference between revenue and costs (net revenue). You can examine either Variable Cost Net Revenue (VCNR) 
                   or Total Cost Net Revenue (TCNR). VCRN is revenue minus variable costs and TCNR is revenue minus variable and fixed costs. 
                These plots are stacked bar plots. Click', tags$em('Composition of total cost net revenue'), 'or', tags$em('Composition
                      of variable cost net revenue'), ' in the', tags$em('Plot Options'), 'field in the panel on the right to see these plots. 
                      A figure demonstrating how VCNR and TCNR are derived is shown below.'),  
           tags$br(),
          img(src="EconInd3.png", height=350),  #  img(src="EconInd4.png", height=350),       
           tags$br(), 
           tags$br(),
           tags$p(strong('Plot Output:'),'The Catch Share program began in 2011. In all plots, we distinguish between pre- and post- implementation of the Catch Share program with shading.'),
           tags$p(strong('Download Plots and Data:'),'Once selections have been made and a plot is visible, a button to download the plot(s) and data used to generate the plot(s) appears at the bottom of the panel on the right.'),
           #tags$p(strong('Download Data:'),'Once selections have been made and a plot is visible, a button to download the data that was used to generate the plot(s) appears in the bottom panel on the right.'),
           tags$p(strong('A note about net revenue:'),  'The', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC forms', target="_blank"), 'attempt to capture only costs that are directly related to vessel fishing operations. They do not include other expenses 
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
                     or filtered within a column using the boxes at the bottom of the table. Data used in the', tags$em('Variability Analysis'), 'tab are not available at this time.'),
                 tags$p( strong('Download Data: '), 'Once selections have been made, a button to download the data table appears at the bottom of the panel to the right.'),
           tags$br(),
           tags$p(strong('To view these instructions'), 'at any time visit the ', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )    
})


output$DefaultThirdsText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Variability Analysis"),
           tags$p('In order to explore the variability within the catcher vessel fleet, we group vessels into three tiers based on the amount of revenue they earn.
               Statistics and measures are then shown for each of the three tiers.'),
            tags$p(strong("To get started, make at least one selection in each of fields in the panel on the right.")),
            tags$p(strong('Background:'), 'Catcher vessels that participate in the', 
                  tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/catch_shares.cfm", target="_blank", "West Coast Groundfish Trawl Catch Share Program"), 
                    'span a very broad range in term of the scale of their operations. 
                   For instance, the revenue earned ranges from around $10,000 to well over $1,500,000.  The purpose of the', tags$em('Variability Analysis'), 'is to show how this heterogeneity 
                   relates to their economic performance. We cannot show the economic performance for each individual vessel due to',
                   tags$a(href = "http://www.nwfsc.noaa.gov/research/divisions/fram/documents/Administration_Operations_Report_2014.pdf",'confidentiality rules', target="_blank"),
                    'so we group the vessels into three tiered categories: top revenue earners, middle revenue earners, and bottom revenue earners. We then calculate the average of 
                    the selected statistic (per vessel, vessel/day, or vessel/metric-ton) for vessels within each tiered category.'), 
         
                  tags$p(strong('Plot output:'), 'Results are shown if there are at least three vessels in each group. Results are plotted as a dot plot if a single year is selected or a line plot if multiple years are selected.',tags$br(), 
                                                'The Catch Share program was implemented in 2011. In all plots, we distinguish between pre- and post- implementation of the Catch Share program with shading.'),
              
               img(src="ExampThirds.png", height=400),
           tags$p(strong('Download Plots:'), 'Once selections have been made and a plot if visible, a button to download the plot(s) appears at the bottom of the panel on the right.'),

                    #These thirds are plotted as lines labeled: "Top Third", "Middle Third" and "Bottom Third". 
                 

           tags$p(strong("Note:"), 'Only a single class of the selected summary variable (fisheries, homeport, state, vessel length class) may be selected at a time. The', tags$em('Summed over all vessels'), 'statistic is not currently supported.'),
           
           tags$p(strong('To view these instructions'), 'at any time, click the', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )
})