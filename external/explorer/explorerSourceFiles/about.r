tags$div(style = "margin: 15px; 15px; 30px; width: 60%",
         tags$p("Welcome to the FISHeries Economics Explorer (FISHEyE) Net Revenue Explorer for the ",
                tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html",
                       "West Coast Groundfish Trawl Catch Share program."), "This application allows for
                the comparison of net revenue figures across different summary variables for catcher vessels. Other sectors that 
                participate in the catch shares program will be added in the future, including motherships, catcher processor fleets, and shorebased trawl fleets."),
         tags$p('FISHEyE is interactive. Information on how to use FISHEyE is available in the "Instructions", tab.  Information on variables in the dataset can
                be found in the "Definitions" tab.'),
         
         tags$hr(),
         tags$h3("A note on confidentiality"), 
         p('Data confidentiality requirements
         limit the availability of some data. Where possible, data that is suppressed
         due to data confidentiality will be indicated with a "Suppressed" message. This
         is to differentiate suppressed confidential data from structurally missing data 
         (data points do not exist). More information on data confidentiality requirements 
         can be found in the',
           tags$a(href = "http://www.nwfsc.noaa.gov/research/divisions/fram/documents/Administration_Operations_Report_2014.pdf",
                  "EDC Administration and Operations Report.")),
         
         tags$hr(),
         
         tags$h3("Disclaimer"),
         tags$p("This R Shiny application is currently under developement.", tags$br(),
                "All data used in this application is subject to change and come with no
      guarantee of accuracy."),
         
         tags$hr(),
         
         tags$h3("Contact"),
         tags$p(
           "Melanie Harsch", tags$br(), 
           "Contractor-ECS Federal, Inc.", tags$br(),
           "In support of NMFS", tags$br(),
           "Northwest Fisheries Science Center", tags$br(),
           "melanie.harsch@noaa.gov")
)

