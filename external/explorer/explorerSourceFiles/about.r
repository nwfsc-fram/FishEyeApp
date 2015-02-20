tags$div(style = "margin-top: 15px; width: 60%",
  tags$p("Welcome to the FISHeries Economics Explorer (FISHEyE) 
      Net Revenue Explorer. This application displays
      net revenue summary statistics for vessels that participated in the",
      tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html",
        "West Coast Groundfish Trawl Catch Share program."), "This app alows for
      the comparison of net revenue figures across different summary variables."),
  
  tags$p(tags$strong("How to use this app:"), tags$br(),
         "To use this application make data selection on the left Select Data panel.
          A variety of outputs can be found under the different tabs located at the top
          of the application, including plots and tables."),
  
  tags$hr(),
    
  tags$p("This R Shiny application is currently under developement.", tags$br(),
      "All data used in this application is subject to change and come with no
      guarantee of being correct."),
  
  tags$hr(),
  
  tags$p(style = "margin-top: 15px", "Contact:"),
  tags$ul(
    tags$li(style = "list-style-type: none", "Blair Vanderlugt"), 
    tags$li(style = "list-style-type: none", "Contractor-ECS Federal"),
    tags$li(style = "list-style-type: none", "In support of NMFS"),
    tags$li(style = "list-style-type: none", "blair.vanderlugt@noaa.gov")
  )
)

