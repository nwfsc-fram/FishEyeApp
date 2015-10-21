
# 
output$BlogText <- renderUI({

tags$div(style = "margin: 15px 15px 30px; width: 60%",
         tags$h3("Blog"),
         tags$p("Updates and information"),
         tags$p("Responses to questions received by email that are of general interest")
)    
})


output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Visualize Data with Plots"),
           tags$p('Visualize', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'Economic Data Collection (EDC)', target="_blank"), 'summary statistics for revenue, costs and net- revenue of catcher vessels that participate in the', tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", 'West Coast Groundfish Trawl Catch Share Program', target="_blank")),
            tags$p(strong("To get started, make at least one selection in each of the panels on the right.")),
           
           HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.62em; margin-top:5px; margin-bottom:-3px;font-size:12pt'>
                     <b>Plotting Options:</b></div>"),
           
          # tags$p(strong('Plotting Options:', style="padding-bottom:0")),
           tags$li('Compare economic measures (revenue, costs, and/or net revenue) across years or fisheries. Click the', 
                      tags$em('Compare economic measures side-by-side'), 'option under', tags$em('Plot Options'), 'in the panel on the right. A drop down menu below the', tags$em('Plot Options'), 'allows users 
                      to change between bar, point, and line plots.'), 
           tags$br(),
            tags$li('Composition of net revenue: these plots show costs relative to revenue and the difference between revenue and costs (net revenue). You can examine either Variable Cost Net revenue (VCNR) 
                   or Total Cost Net Revenue (TCNR). VCRN is revenue minus variable costs and TCNR is revenue minus variable and fixed costs. 
                These figures are stacked bar plots. Click the', tags$em('Composition of total cost net revenue'), 'or the', tags$em('Composition
                      of variable cost net revenue'), 'choice under', tags$em('Plot Options'), 'at the bottom of the panel on the right to see these figures.'),  
           tags$br(),
          img(src="EconInd3.png", height=350),  #  img(src="EconInd4.png", height=350),       
           tags$br(), 
           tags$br(),
           tags$p(strong('Plot Output:'),'The Catch Share Program began in 2011. In all figures, we distinguish between pre- and post- implementation of the catch share program with shading.'),
           tags$p(strong('Download Figures:'),'Once selections have been made and a plot is visible, a button to download the plot(s) appears in the bottom panel on the right.'),
           tags$p(strong('Download Data:'),'Once selections have been made and a plot is visible, a button to download the data that was used to generate the plot(s) appears in the bottom panel on the right.'),
           tags$p(strong('A note about net revenue:'),  'The', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC forms', target="_blank"), 'attempt to capture only costs that are directly related to vessel fishing operations, and do not include other expenses 
            such as vehicles or office costs that may be related to the fishing business. Therefore, the net revenue reported here is an overestimate of the true net revenue.'),
           tags$br(),
           tags$p(strong('To view these instructions'), 'at any time visit the', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )
 
})

output$DefaultTableText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Data Table"),
           tags$p('View data used to generate the figures created in the', tags$em('Visualize Data with Plots'), 'tab.'),
                  tags$p(strong('To get started, make at least one selection in each of the panels on the right.')), 
            tags$p('After a data table has been displayed, the data can be further filtered using the ',tags$em('Search'), 'box, 
                     or filter within a column using the boxes at the bottom of the table. Data used in the', tags$em('variability analysis'), 'tab are not available at this time.'),
                 tags$p( strong('A button to download the table can be found in the panel to the right.')),
           
           tags$p('To view these instructions again at any time visit the ', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )    
})


output$DefaultThirdsText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h3("Variability Analysis"),
           tags$p(strong("Objective:"), 'To show the variability of the catcher vessel fleet we group vessels into three tiers based on their revenue and show results for these tiers.'),
            tags$p(strong("To get started, make at least one selection in each of the panels on the right.")),
            tags$p(strong('Background:'), 'Catcher vessels that participate in the trawl catch share program span a very broad range in term of the scale of their operations. 
                   For instance, the revenue earned ranges from around $10,000 to well over $1,500,000.  The purpose of the Variability Analysis is to show how this heterogeneity 
                   relates to their economic performance. We cannot show the economic performance for each individual vessel due to confidentiality rules, so we group the vessels 
                   into three tiered categories: top revenue earners, middle revenue earners, and lower revenue earners. We then calculate the average of the selected economic measure 
                    (per vessel, vessel/day, or vessel/metric-ton) is calculated for vessels within each group.'), 
         
                  tags$p(strong('Plot output:'), 'Results are shown if there are at least three vessels in each group. Results are plotted as a dot plot if a single year is selected or a line plot if multiple years are selected.',tags$br(), 
                                                'The Catch Share Program was implemented in 2011. In all figures, we distinguish between pre- and post- implementation of the catch share program with shading.'),
              
              tags$p(strong('Download figures:'), 'Once selections have been made and a plot if visible, a button to download the plot(s) appears in the bottom panel on the right.'),

                    #These thirds are plotted as lines labeled: "Top Third", "Middle Third" and "Bottom Third". 
                  img(src="ExampThirds.png", height=400),

           tags$p(tags$strong("Note:"), 'Only a single class of the selected summary variable may be selected at a time. The', tags$em('summed over all vessels'), 'statistic is not currently supported.'),
           
           tags$p('To view these instructions again at any time, click the', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )
})