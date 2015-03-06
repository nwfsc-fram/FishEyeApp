
# 

output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h4("Summary Plot"),
           tags$p("Create bar, line and point graphs for summary statistics of West Coast Catcher Vessels.",tags$br(),
                  "To get started, make at least one selection in each of the panels on the left.",tags$br(),
                  "Options for changing the chart type and a download button can be found below the plot output."),
           
           tags$p('To view these instructions again at any time visit the "Instructions" tab')
  )
})


output$DefaultTableText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h4("Summary Table"),
           tags$p("View a table of data used to create the Summary Plot.", tags$br(),
                  "After a table has been displayed, you can search for data in the upper right search box, 
                  or search within a column using the boxes on the bottom of the table.",tags$br(),
                  "A download button can be found beneath the table."),
           
           tags$p('To view these instructions again at any time visit the "Instructions" tab')
  )    
})


output$DefaultThirdsText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin: 15px 15px 30px; width: 60%",
           tags$h4("Thirds Analysis"),
           tags$p('Create a graph that splits vessels up into three groups based on their annual Revenue. These thirds are
                     plotted as lines labeled: "Top Third", "Middle Third" and "Bottom Third". These correspond
                     to the top revenue earners, the middle revenue earners and the bottom revenue earners. A download option
                     can be found below the graph'),
           tags$p(tags$strong("Note:"), "This feature is still in development and has limited functionality.", tags$br(),
                  'The Summary Variable is limited to one selection and the Summary Statistic only supports the "Mean" option.'),
           
           tags$p('To view these instructions again at any time visit the "Instructions" tab')
  )
})