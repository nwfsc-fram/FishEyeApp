tags$div(style = "margin: 15px; 15px; 30px; width: 60%",
         
         tags$h3('How to use this application'), 
          tags$p('To use this application, make data selections on the left "Select Data" panel.
          Output will be automatically generated when each of the fields in the "Select Data" panel has at least
          one selection. Several outputs can be found in the tabs located at the top of the application:'),
          tags$p('The ', 
          tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", "Catch Shares program"),
              'was implemented in 2011.  For all plots (summary plots and variability analysis plots),  
              if years prior to and after the implementation of the catch shares program are chosen, the  pre-catch shares years will be shaded.'),
           tags$ul(style="margin-top:15px;" ,
           tags$li(tags$h4("Visualize Data")),
           tags$p('Visualize summary statistics for net revenue of West Coast Catcher Vessels.',tags$br(),
                # "To get started, make at least one selection in each of the panels on the left.",tags$br(),
                'Options for changing the chart type (bar, line, or point graphs)can be found below the plot output. 
                A button to download the plot can be found in the panel to the left.',tags$br(),
          'With the "bar" graph option, you can compare selected economic measures side-by-sde, view the "Total Cost Net Revenue" figure (economic measures pre-defined),
                 or view the "Variable Cost Revenue" figure (economic measures pre-defined for this graph).  Other options to view the data are point and line graphs.'),
          
          
              tags$li(tags$h4("Data Table")),
              tags$p('View data used to create the Summary Plot.', tags$br(),
                  #   "To get started, make at least one selection in each of the panels on the left.
                     'After a table has been displayed, the data can be further filtered using the "Search" box, 
                     or filter within a column using the boxes at the bottom of the table.',tags$br(),
                     'A button to download the data table can be found in the panel on teh left.'),
          
              tags$li(tags$h4("Variability Analysis")),
              tags$p('The variability analysis calculates the average (per vessel, vessel/day, or vessel/metric-ton) of the selected economic measure for   
                    vessels grouped based on their annual revenue: top revenue earners, middle revenue earners, and the bottom revenue earners.  
                     Results are plotted as a dot plot if a single year is selected or a line plot if multiple years are selected.', tags$br(),
                    #'These thirds are plotted as lines labeled: "Top Third", "Middle Third" and "Bottom Third".' 
                   'A button to download the plot can be found in the panel on the left.'),
                     tags$p(tags$strong("Note:"), "This feature is still in development and has limited functionality.", tags$br(),
                    # 'The Summary Variable is limited to one selection and the Summary Statistic only supports the "Average" option.'
                    'Please note that only a single class of the selected summary variable may be selected at a time. Only the "Average" statistic option is supported.')
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