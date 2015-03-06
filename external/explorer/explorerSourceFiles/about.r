tags$div(style = "margin: 15px; 15px; 30px; width: 60%",
         tags$p("Welcome to the FISHeries Economics Explorer (FISHEyE) 
      Net Revenue Explorer.",
                tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html",
                       "West Coast Groundfish Trawl Catch Share program."), "This app alows for
      the comparison of net revenue figures across different summary variables."),
         
         tags$hr(),
         tags$h3("A note on confidentiality"), 
         p('Data confidentiality requirements
         limit the availability of some data. Where possible, data that is suppressed
         due to data confidentiality will be indicated with a "Suppressed" message. This
         is to differentiate suppressed confidential data from structurally missing data 
         (feature in development).More information on data confidentiality requirements 
         can be found in the',
           tags$a(href = "http://www.nwfsc.noaa.gov/research/divisions/fram/documents/Administration_Operations_Report_2014.pdf",
                  "EDC Administration and Operations Report")),
         
         tags$hr(),
         
         tags$h3("Disclaimer"),
         tags$p("This R Shiny application is currently under developement.", tags$br(),
                "All data used in this application is subject to change and come with no
      guarantee of being correct."),
         
         tags$hr(),
         
         tags$h3("Contact"),
         tags$p(
           "Blair Vanderlugt", tags$br(), 
           "Contractor-ECS Federal, Inc.", tags$br(),
           "In support of NMFS", tags$br(),
           "Northwest Fisheries Science Center", tags$br(),
           "blair.vanderlugt@noaa.gov")
)
