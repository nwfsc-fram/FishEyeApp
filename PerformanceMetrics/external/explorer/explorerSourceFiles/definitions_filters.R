tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         tags$div(
           h3("Filters"),
           p('Please note that availability of filters will vary between sectors.'),
           tags$br(),
           h4("Vessel type"),
           p("We include the option to summarize data by three types of vessels: 1) All vessels: the fleet as a whole, 2) Whiting vessels: vessels that fished for Pacific whiting, 
                     including those that fished for whiting and non-whiting, and 3) Non-whiting vessels: vessels that participate in the non-whiting groundfish sector of the limited entry trawl fishery."),
           tags$img(src='WhitingFigure.png', height=250),
           p("The purpose of separating the whiting from the non-whiting vessels is that the whiting vessels tend to be larger and catch a higher volume of fish. In addition, 
                            total allowable catch for Pacific whiting has more annual variation than total annual catch for species targeted in the non-whiting groundfish sector. Note that 
                            there are several fisheries where vessels only participated in whiting or non-whiting fisheries."),
           p("There is only one type of mothership and catcher-processor because they only species they process if Pacific whiting."),
           p("Where possible, data are reported for all vessels combined, whiting vessels, and non-whiting vessels. If there are fewer than three vessels in either the 
                            whiting or non-whiting category, we only report the “All vessels” category. More information on data confidentiality requirements can be found in the",
             tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/documents/Administration_Operations_Report_2014.pdf", 'EDC Administration and Operations Report.', target="_blank")
           ),
           tags$br(),
           h4("Processor type"),
           p("We include the option to summarize data by three types of processors: 1) All processors: processed catch share fish purchases, 2) Whiting processors: processed Pacific whiting, 
             and 3) Non-whiting processors: did not process whiting, but processed other non-whiting species."),
           tags$img(src='WhitingFR.png', height=175),
           tags$br(),
           h4("Include Alaskan fisheries activities"),
           p("For some metrics (e.g., Fishery diversification, Number of fisheries), you may choose whether or not to include activities in Alaskan fisheries."),
           tags$br(),
           h4("Years"),
           p("The catch share program began in 2011. A mandatory",
             tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'Economic Data Collection (EDC) program', target="_blank"),
             "began in 2009. Results are available from 2009 onwards for metrics that require EDC data. For metrics that do not require EDC data, results are available from 2004 onwards. 
                     You can use the slider bar to select the appropriate range of years.")
         ))