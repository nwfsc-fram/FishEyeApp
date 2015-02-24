tags$div(style = "margin-top: 15px; width: 60%",
  tags$p("Welcome to the FISHeries Economics Explorer (FISHEyE) 
      Net Revenue Explorer. This application displays
      net revenue summary statistics for vessels that participated in the",
      tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html",
        "West Coast Groundfish Trawl Catch Share program."), "This app alows for
      the comparison of net revenue figures across different summary variables."),
  
  tags$hr(),
  
  tags$p(tags$strong("How to use this app:"), tags$br(),
         "To use this application make data selections on the left Select Data panel.
          Output will automatically be generated once each of the fields have at least
          one selection.
          A variety of outputs can be found under the different tabs located at the top
          of the application, including plots and tables."),
  tags$p(tags$strong("A note on confidentiality:"), "Data confidentiality requirements
         limit the availability of some data. Where possible, data that is suppressed
         due to data confidentiality will be indicated with a 'Suppressed' message. This
         is to differentiate suppressed confidential data from structurally missing data 
         (feature in development).More information on data confidentiality requirements 
         can be found in the",
         tags$a(href = "http://www.nwfsc.noaa.gov/research/divisions/fram/documents/Administration_Operations_Report_2014.pdf",
                "EDC Administration and Operations Report")),
  
  tags$hr(),
    
  tags$p("This R Shiny application is currently under developement.", tags$br(),
      "All data used in this application is subject to change and come with no
      guarantee of being correct."),
  
  tags$hr(),
  
  tags$p(style = "margin-top: 15px", "Contact:"),
  tags$ul(
    tags$li(style = "list-style-type: none", "Blair Vanderlugt"), 
    tags$li(style = "list-style-type: none", "Contractor-ECS Federal, Inc."),
    tags$li(style = "list-style-type: none", "In support of NMFS"),
    tags$li(style = "list-style-type: none", "Northwest Fisheries Science Center"),
    tags$li(style = "list-style-type: none", "Seattle, WA"),
    tags$li(style = "list-style-type: none", "blair.vanderlugt@noaa.gov")
  )
)

