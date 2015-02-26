output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
    tags$div(style = "margin-top: 15px",
      p("Plot the Net Revenue summary statistics of the
    West Coast Groundfish fishery"),
        tags$ol(
          tags$li("Choose data in the 'Select Data' pane"),
            tags$ul(tags$i("Multiple Summarize By selections with
                    be shown as side-by-side comparisons")),
          tags$li("Plot and tabular output can be found under their respective tabs"),
          tags$li("Plot options are found in the pane below the output"),
          tags$li("Plot and table download buttons can be found in the Plot Options
      pane")
        )
    )    
})

output$DefaultThirdsText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin-top: 15px",
    p("Plot the variation in performance for vessels in a selected summary variable."),
    p("Vessels are ordered by Revenue and reported as groups of three vessels
      for the highest revenue, middle revenue and lowest revenue."),
    tags$ol(
      tags$li("Choose data in the 'Select Data' pane"),
      tags$li("Plot options are found in the pane below the output"),
      tags$li("Plot and table download buttons can be found in the Plot Options
              pane"),
      tags$li(tags$strong("Currently only supports one summary variable selection")),
      tags$li(tags$strong("Currently only supports the mean summary statistic"))
    )
  )    
})


output$DefaultTableText <- renderUI({
  if(PermitPlot()) return()
  tags$div(style = "margin-top: 15px",
           p("Display the summary table for the Net Revenue summary statistics of the
      WestCoast Groundfish fishery."),
           tags$ol(
             tags$li("Choose data in the 'Select Data' pane"),
             tags$li("Plot and tabular output can be found under their respective tabs"),
             tags$li("Plot options are found in the pane below the output"),
             tags$li("Plot and table download buttons can be found in the Plot Options
              pane")
           )
  )    
})