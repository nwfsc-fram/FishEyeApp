tags$div(style = "margin: 15px; 15px; 30px; width: 60%",
         
         tags$h3('How to use FISHEyE'), 
          tags$p('To use this web application, make data selections on the', tags$em('Select Data'), 'panel on the right.
          Output will be automatically generated when each of the fields in the', tags$em('Select Data'), 'panel has at least
          one selection. The', tags$em('Select Data'), 'panel will only appear when you are on the',tags$em('Explore the data'), 'page.', tags$br(), 'A button to download the figure and data table can be found in the panel to the right.'),
          tags$p('The ', 
          tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", "Catch Share program"),
              'was implemented in 2011.  In all figures, we distinguish between pre- and post-implementation of the catch share program with shading.'),
           tags$ul(style="margin-top:15px;" ,
                   
           tags$li(tags$h4("Visualize Data")),
           tags$p('Visualize summary statistics for net revenue of West Coast Catcher Vessels.',tags$br(),
                # "To get started, make at least one selection in each of the panels on the left.",tags$br(),
                
                'Options for changing the plot type can be found in the same panel.', tags$br(), 
                'There are three plot options: 1) compare economic measures side-by-side, 2) derivation of the total cost net revenue, and 3) derivation of the variable cost net revenue. 
                The', tags$em('Compare economic measures side-by-side'), 'option is useful for comparing economic measures within or between years. The two derivation figures are useful 
                for understanding how costs (total and variable) affect net revenue.  A figure demonstrating how these two economic measures are derived is located on the', tags$em('Explore the data'), 
                'page under the', tags$em('Visualize Data'), 'tab.'),
          
          
              tags$li(tags$h4("Data Table")),
              tags$p('View data used to generate the figures on the', tags$em('Visualize Data'), 'tab', tags$br(),
                  'After a table has been displayed, the data can be further filtered using the', tags$em('Search'), 'box, or filter within a column using the boxes at the bottom of the table. 
                  Data specific to the variability analysis are not provided at this time.'),
          
              tags$li(tags$h4("Variability Analysis")),
              tags$p('The variability analysis is useful for comparing changes in an economic measure over time between vessels with high and low revenue. The analysis is done by 
                     grouping vessels annually into three groups based on variable cost net revenue (top revenue earners, middle revenue earners, and bottom revenue earners). Then the average (per vessel, 
                     vessel/day, or vessel/metric-ton) of the selected economic measure is computed across vessels within groups.', tags$br(), 
              'Results are shown if there are at least three vessels in each group. Results are plotted as a dot plot if a single year is selected or a line plot if multiple years are selected.', tags$br(),

                  tags$strong("Note:"), 'Only a single class of the selected summary variable may be selected at a time. The', tags$em('summed over all vessels'), 'statistic option is not currently supported.')
          )

)



# 
# 
# tags$div(style="margin-top:15px; width: 60%",
# tags$p("To generate output select data in the Select Data Panel.")
# tags$p('Select the above tabs to view different forms of output, including
#        a summary plot, table, and the thirds analysis.')
# ),
# 
# # PLOT
# tags$h3(tags$i("plot")),
#   p("View data summaries for the selected summary variables.")
# 
# # THIRDS
# tags$h3(tags$i("Thirds anlaysis")),
#   p("View how different."),
#   p("Vessels are ordered by Revenue and reported as top third, 
#     middle third and bottom third.")