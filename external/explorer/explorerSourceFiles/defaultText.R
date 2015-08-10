
# 

output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h4("Plot data"),
           tags$p("Visualize summary statistics for net revenue of West Coast Catcher Vessels. "),
                   img(src="Example.png", height=400),
            tags$p('With the "bar" graph option, you can compare selected economic measures side-by-sde, view the "Total Cost Net Revenue" figure (economic measures pre-defined),
                  or view the "Variable Cost Revenue" figure (figure shown; economic measures pre-defined for this graph).  Other options to view the data are point and line graphs.'),
                 
             tags$p('The Catch Shares Program was implemented in 2011.  If years prior to and after the implementation of the catch shares program are chosen, the 
                   pre-catch shares years will be shaded.'),
                  
             tags$p(strong("To get started, make at least one selection in each of the panels on the left."),tags$br(),
                  "Once selections have been made, a button to download the plot output will appear in the panel on the left. 
                  Options for changing the chart type can be found beneath the plot."),
           
           tags$p('To view these instructions again at any time visit the "Instructions" tab.')
  )
 
})

output$DefaultTableText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h4("Data Table"),
           tags$p('View data used to create the Summary Plot.', tags$br(),
                  'To get started, make at least one selection in each of the panels on the left. After a data table has been displayed, the data can be further filtered using the "Search" box, 
                     or filter within a column using the boxes at the bottom of the table. '),
                 tags$p( strong('A button to download the table can be found in the panel to the left.')),
           
           tags$p('To view these instructions again at any time visit the "Instructions" tab.')
  )    
})


output$DefaultThirdsText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h4("Variability Analysis"),
           tags$p('The variability analysis calculates the average (per vessel, vessel/day, or vessel/metric-ton) of the selected economic measure for
                    vessels grouped based on their annual revenue: top revenue earners, middle revenue earners, and the bottom revenue earners.  
                     Results are plotted as a dot plot if a single year is selected or a line plot if multiple years are selected. 
                     To get started, make at least one selection in each of the panels on the left'),  
                    #These thirds are plotted as lines labeled: "Top Third", "Middle Third" and "Bottom Third". 
                  img(src="ExampThirds.png", height=400),
                  tags$p('The Catch Shares Program was implemented in 2011.  If years prior to and after the implementation of the catch shares program are chosen, the 
                   pre-catch shares years will be shaded.') ,
                  
           tags$p(strong("To get started, make at least one selection in each of the panels on the left."),tags$br(),
                  "Once selections have been made, a button to download the plot output will appear in the panel on the left."),
           
           tags$p(tags$strong("Note:"), "This feature is still in development and has limited functionality.", tags$br(),
                  'Please note that only a single class of the selected summary variable may be selected at a time. The summary statistic only supports the "Average" summary statistic options.'),
           
           tags$p('To view these instructions again at any time visit the "Instructions" tab.')
  )
})