tags$div(style = "margin: 15px; 15px; 30px; width: 60%",
         
         tags$h3('How to use this app'), 
          tags$p('To use this application, make data selections on the left "Select Data" panel.
          Output will be automatically generated when each of the fields has at least
          one selection. Several outputs can be found in the tabs located at the top of the app:'),
        
           tags$ul(style="margin-top:15px;" ,
            tags$li(tags$h4("Summary Plot")),
              tags$p('Visualize summary statistics for net revenue of West Coast Catcher Vessels.',tags$br(),
                    # "To get started, make at least one selection in each of the panels on the left.",tags$br(),
                     'Options for changing the chart type (bar, line, or point graphs) and a download button can be found below the plot output.'
              ),
          
              tags$li(tags$h4("Summary Table")),
              tags$p('View data used to create the Summary Plot.', tags$br(),
                  #   "To get started, make at least one selection in each of the panels on the left.
                     'After a table has been displayed, the data can be further filtered using the "Filter data by" box, 
                     or filter within a column using the boxes on the bottom of the table.',tags$br(),
                     'A download button can be found beneath the table.'),
          
              tags$li(tags$h4("Variability Analysis")),
              tags$p('The variability analysis calculates the average (per vessel, vessel/day, or vessel/metric-ton) of the selected economic measure for   
                    vessels grouped based on their annual revenue: top revenue earners, middle revenue earners, and the bottom revenue earners.  
                     Results are plotted as a dot plot if a single year is selected or a line plot if multiple years are selected. #These thirds are
                    # plotted as lines labeled: "Top Third", "Middle Third" and "Bottom Third". 
                    A download option can be found below the graph.'),
                     tags$p(tags$strong("Note:"), "This feature is still in development and has limited functionality.", tags$br(),
                    # 'The Summary Variable is limited to one selection and the Summary Statistic only supports the "Average" option.'
                    'Please note that only a single class of the selected summary variable may be selected at a time. The summary statistic only supports the "Average" option.')
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