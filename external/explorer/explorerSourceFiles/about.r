tags$div(style = "margin: 15px; 15px; 30px; width: 60%",
         tags$p("Welcome to the FISHeries Economics Explorer (FISHEyE) Net Revenue Explorer for the ",
                tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html",
                       "West Coast Groundfish Trawl Catch Share program."), 'The West Coast Groundfish Trawl Fishery adopted a Catch Share program in 2011. 
                FISHEyE allows users to explore the economic impact of the Catch Share program. As part of the program, participants of the fishery are required to complete', 
                tags$a(href='http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm', 'EDC Forms'), 'as stated in', 
                tags$a(href="http://www.ecfr.gov/cgi-bin/text-idx?SID=06f0c396e52e564ce22a048aa910f49f&node=50:13.0.1.1.1.4.1.5&rgn=div8",'regulation 50 CFR 660.114.'), 'Data used in FISHEyE come these forms and', tags$a(href="http://pacfin.psmfc.org/",'PacFIN.'), 
                'Please see below for a note on confidentiality.'), 
          tags$p('FISHEyE Net Revenue Explorer allows for the comparison of net revenue figures across different summary variables for catcher vessels. Other sectors that participate 
                  in the catch shares program will be added in the future, including motherships, catcher processor fleets, and first receivers and shorebased processors. Additional 
                  metrics to assess the effectiveness and outcomes of the Catch Share program are also in development.'),
         tags$p('FISHEyE is interactive. Information on how to use FISHEyE is available in the', tags$em("Instructions"), 'tab under the', tags$em('About, Instructions, Definitions'), 'page.
                Information on variables in the dataset can
                be found in the', tags$em("Definitions"), 'tab under the', tags$em('About, Instructions, Definitions'), 'page.'),
         
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

