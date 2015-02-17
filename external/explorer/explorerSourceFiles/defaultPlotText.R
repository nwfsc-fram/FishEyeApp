
output$DefaultPlotText <- renderUI({
  if(PermitPlot()) return()
#   HTML('
# <div style="position: absolute; left: 15px; top: 50px; text-align: left;
# width: 40%">
#   <p style="text-align:justify">Plot the Net Revenue summary statistics of the
#     WestCoast Groundfish operations. 
#     <ul><strong>Output Includes</strong><ul>
#       <li>Revenue</li>
#       <li>Variable cost</li>
#       <li>Total cost</li>
#       <li>Variable cost net revenue</li>
#       <li>Total cost net revenue</li>
#     </ul>
#     <i>For definitions of the above measures see the definitions tab</i>
#   </p>
# </div>
# ')
# #   if(PermitPlot()) return()
# #   HTML(
#     "Test
#     Line 2
#     Line 3"
# #   )
# # #         
    tags$div(
      p("Plot the Net Revenue summary statistics of the
    WestCoast Groundfish operations."),
        tags$ol(
          tags$li("Choose data in the Select Data pane"),
          tags$li("Press the Plot Data button at the bottom of the Select Data pane"),
          tags$li("Plot and tabular output can be found under their respective tabs"),
          tags$li("Plot options are found in the pane below the output"),
          tags$li("Plot and table download buttons can be found in the Plot Options
      pane")
        )
    )    
})