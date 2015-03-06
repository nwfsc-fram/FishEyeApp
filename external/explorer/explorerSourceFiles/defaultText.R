
  output$DefaultPlotText <- renderUI({
    if(PermitPlot()) return()
    tags$div(style = "margin-top: 15px",
             p("Choose data in the 'Select Data' panel"),
             p(tags$i("Multiple Summary variables will 
                      be shown as side-by-side comparisons"))
    )    
  })
  
  output$DefaultThirdsText <- renderUI({
    if(PermitPlot()) return()
    tags$div(style = "margin-top: 15px",
             p("Choose data in the 'Select Data' panel")
    )
  })
  
  
  output$DefaultTableText <- renderUI({
    if(PermitPlot()) return()
    tags$div(style = "margin-top: 15px",
             p("Choose data in the 'Select Data' panel")
    )    
  })
