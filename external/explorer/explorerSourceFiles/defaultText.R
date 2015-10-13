
# 

output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h4("Plot data"),
           tags$p("Visualize summary statistics for net revenue of West Coast Catcher Vessels.",tags$br(),  tags$strong("To get started, make at least one selection in each of the panels on the right.")),
           
             tags$p(strong('Data plotting options:'),'Compare economic measures as a dot plot, line plot, or bar plot. To compare economic measures, click the', 
                      tags$em('Compare economic measures side-by-side'), 'option under', tags$em('Plot Options'), 'in the panel on the right. A drop down menu below the', tags$em('Plot Options'), 'allows users 
                      to change between bar, point, and line plots.', tags$br(),

            'We also provide two additional figures to visualize how the variable cost net revenue and total cost net revenue are derived. The variable cost net revenue is 
                      revenue minus variable costs and the total cost net revenue is revenue minus variable and fixed costs. The economic measures are preset for these figures 
                      and cannot be changed. In addition, these figures will only show as stacked bar plots. Click the', tags$em('derivation of total cost net revenue'), 'or the', tags$em('derivation
                      of variable cost net revenue'), 'choice under', tags$em('Plot Options'), 'in the panel on the right to see these figures.'),  
          #  img(src="EconomicIndicator2.png", height=350),   
                #   p('Other options to view the data are point and line graphs.'),
          img(src="EconInd3.png", height=350),  #  img(src="EconInd4.png", height=350),       
                   
           tags$p(strong('Plot Output:'),'The Catch Share Program began in 2011. In all figures, we distinguish between pre- and post- implementation of the catch share program with shading.'),
           tags$p(strong('Downloading figures:'),'Once selections have been made, a button to download the plot output will appear in the panel on the right. Options for changing the chart type can be in the panel on the right.'),
           tags$p('To view these instructions again at any time visit the', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )
 
})

output$DefaultTableText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h4("Data Table"),
           tags$p('View data used to generate the figures in the', tags$em('Visualize Data'), 'tab.', tags$br(),
                  tags$strong('To get started, make at least one selection in each of the panels on the right.')), 
            tags$p('After a data table has been displayed, the data can be further filtered using the ',tags$em('Search'), 'box, 
                     or filter within a column using the boxes at the bottom of the table. Data specific to the variability analysis are not provided at this time.'),
                 tags$p( strong('A button to download the table can be found in the panel to the right.')),
           
           tags$p('To view these instructions again at any time visit the ', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )    
})


output$DefaultThirdsText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h4("Variability Analysis"),
            tags$p(strong("To get started, make at least one selection in each of the panels on the right.")),
            tags$p(strong('Background:'), 'The variability analysis is useful for comparing changes in an economic measure over time between vessels with high and low revenue. The analysis is done by grouping vessels annually into three groups based on variable cost net revenue (top revenue earners, middle revenue earners, and the bottom revenue earners). Then the 
                        average (per vessel, vessel/day, or vessel/metric-ton) of the selected economic measure is computed across vessels within groups.'), 
         
                  tags$p(strong('Plot output:'), 'Results are shown if there are at least three vessels in each group. Results are plotted as a dot plot if a single year is selected or a line plot if multiple years are selected.',tags$br(), 
                                                'The Catch Share Program was implemented in 2011. In all figures, we distinguish between pre- and post- implementation of the catch share program with shading.'),
              
              tags$p(strong('Downloading figures:'), 'Once selections have been made, a button to download the plot output will appear in the panel on the right.'),

                    #These thirds are plotted as lines labeled: "Top Third", "Middle Third" and "Bottom Third". 
                  img(src="ExampThirds.png", height=400),

           tags$p(tags$strong("Note:"), 'Only a single class of the selected summary variable may be selected at a time. The', tags$em('summed over all vessels'), 'statistic is not currently supported.'),
           
           tags$p('To view these instructions again at any time visit the', tags$em('Instructions'), 'tab under the ', tags$em('About, Instructions, Definitions'), 'page.')
  )
})