tags$div(style = "margin: 15px; 15px;30px; 30px; width: 60%",
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
                     <h3>About Net Revenue Explorer</h3></div>"),
       #  tags$h3('About Net Revenue Explorer', style="border-top:10px; padding-top:10px; margin-top:10px"),
         tags$p("Welcome to FISHeries Economics Explorer (FISHEyE) Net Revenue Explorer for the ",
                tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html",
                       "West Coast Groundfish Trawl Catch Share Program.", target="_blank"), 'The Catch Share program began in 2011.  
                FISHEyE allows users to explore economic data pre and post catch share management. As part of the Catch Share program, participants of the fishery are required to complete', 
                tags$a(href='http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm', 'EDC Forms', target="_blank"), 'as stated in', 
                tags$a(href="http://www.ecfr.gov/cgi-bin/text-idx?SID=06f0c396e52e564ce22a048aa910f49f&node=50:13.0.1.1.1.4.1.5&rgn=div8",'regulation 50 CFR 660.114.', target="_blank"), 'Data used in FISHEyE comes from these forms and', 
                tags$a(href="http://pacfin.psmfc.org/",'PacFIN.', target="_blank"), 
                'Please see the note about the confidentiality of this data below.'), 
          tags$p('FISHEyE Net Revenue Explorer allows for the comparison of revenue, costs, and net revenue across years for different summary statistics. Net Revenue Explorer currently focuses on', 
                 tags$a(href="http://www.nwfsc.noaa.gov/news/features/infographics/Catcher%20Vessel.jpg","catcher vessels.", target="_blank"), 
                  'Other sectors', tags$a(href="http://www.nwfsc.noaa.gov/news/features/infographics/Catcher%20Processor.jpg", '(catcher-processors,', target="_blank"), 
                  tags$a(href="http://www.nwfsc.noaa.gov/news/features/infographics/Mothership.jpg", 'motherships,', target="_blank"), 'and ', 
                  tags$a(href="http://www.nwfsc.noaa.gov/news/features/infographics/First%20Receivers%20_%20Shorebased%20Processors.jpg",'first receivers and shorebased processors)', target="_blank"),
                  ' that participate in the Catch Share program will be added in the future. Additional 
                  metrics to assess the effectiveness and outcomes of the Catch Share program are also in development.'),
         tags$p('FISHEyE is user driven and interactive. Information on how to use FISHEyE is available in the', tags$em("Instructions"), 'tab under the', tags$em('About, Instructions, Definitions'), 'page.
                Information on the variables in the dataset and definitions of the statistics and measures used are 
                found in the', tags$em("Definitions"), 'tab under the', tags$em('About, Instructions, Definitions'), 'page.'),
         
         tags$hr(),
         tags$h3("A note about confidentiality"), 
         p('Data confidentiality requirements do not allow us to show individual observations.
         We therefore aggregate or summarize the data to protect individual confidentiality.  
          In some cases, this limits our ability to show some data. Where possible, data that is suppressed
         due to data confidentiality will be indicated with a "Suppressed" message. This
         is to differentiate suppressed confidential data from missing data 
         (data points that do not exist). More information on data confidentiality requirements can be found in the',
           tags$a(href = "http://www.nwfsc.noaa.gov/research/divisions/fram/documents/Administration_Operations_Report_2014.pdf",
                  "EDC Administration and Operations Report.", target="_blank")),
         
         tags$hr(),
         tags$h3("Please visit the following websites for more information on:"),
            tags$a(href="http://www.pcouncil.org/groundfish/background/","Groundfish management", target="_blank"),tags$br(),            
            tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/catch_shares.cfm", "West Coast Groundfish Trawl Catch Share Program", target="_blank"),tags$br(),
            tags$a(href="http://www.ecfr.gov/cgi-bin/text-idx?c=ecfr&SID=234ccb2b8ba3f03ac41139a5c253d147&rgn=div8&view=text&node=50:13.0.1.1.1.4.1.5&idno=50","Mandatory data collection",  target="_blank"),tags$br(),
            tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_reports.cfm","Economic Data Collection Reports",target="_blank"),tags$br(),
            tags$a(href="http://www.nwfsc.noaa.gov/news/features/infographics/index.cfm", "Sectors participating in the Catch Share program", target="_blank"),tags$br(),
            tags$a(href="http://pacfin.psmfc.org/","Pacific Fisheries Information Network (PacFIN)", target="_blank"),tags$br(),
            tags$a(href="http://www.nwfsc.noaa.gov/index.cfm","Northwest Fishery Science Center (NWFSC)", target="_blank"), tags$br(),
            tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/index.cfm", "Economic and Social Science Research Program at the NWFSC", target="_blank"),
         tags$hr(),
         tags$h3("Disclaimer"),
         tags$p("This web application is currently under developement.", tags$br(),
                "All data used in this application is subject to change and come with no guarantee of accuracy."),
         
         tags$hr(),
         
         tags$h3("Contact"),
         tags$p(
           "Melanie Harsch", tags$br(), 
           "Contractor-ECS Federal, Inc.", tags$br(),
           "In support of NMFS", tags$br(),
           "Northwest Fisheries Science Center", tags$br(),
           tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'nwfsc.fisheye.noaa.gov')),
       tags$hr(),
       tags$h3("Acknowledgements"),
       tags$p("There are numerous individuals to thank for their contributions to developing FISHEyE. 
            We thank the Northwest Fisheries Science Center (NWFSC) economists, Scientific Data Management staff, and Information Technology staff. 
              We thank staff at PacFIN and AKFIN. Finally and very importantly, we thank the members of the West Coast fishing industry.")
)

