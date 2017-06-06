tags$div(style = "margin: 15px; 15px;30px; 30px; width: 60%",
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
                     <h3>About</h3></div>"),
       #  tags$h3('About Net Revenue Explorer', style="border-top:10px; padding-top:10px; margin-top:10px"),
         tags$p(tags$br(),"Welcome to FISHeries Economics Explorer (FISHEyE) Performance Metrics page for the ",
                tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", "West Coast Groundfish Trawl Catch Share Program,", target="_blank"),
               'which began in 2011. The catch share program consists of cooperatives for the at-sea mothership (including catcher vessels and motherships) and catcher-processor fleets,
                and an individual fishing quota (IFQ) program for the shorebased trawl fleet (catcher vessels, and first receivers and shorebased processors).'),
       tags$br(),
       
            tags$p('The catch share program was instituted with several goals, including:'),
       tags$ul(tags$li('Provide for a viable, profitable, and efficient groundfish fishery;'),
               tags$li('Increase operational flexibility;'), 
               tags$li('Minimize adverse effects on fishing communities and other fisheries to the extent practical;'), 
               tags$li('Promote measurable economic and employment benefits through the seafood catching, processing, distribution, and support sectors of the industry;')),  
tags$p('FISHEyE Performance Metrics provides metrics to assess the effectiveness and outcomes of the catch share program. The metrics are not intended to be interpreted as causal (i.e., X caused Y change), 
       directional (i.e., "up is good, down is bad"), nor normative (i.e., a particular trend is "good" or "bad"), but, rather, to facilitate the examination of trends and changes.'),
                
               tags$br(),
                
                tags$p('Data used in FISHEyE comes from',tags$a(href='http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm', 'EDC survey forms', target="_blank"),
                       'and Pacific Fisheries Information Network', tags$a(href="http://pacfin.psmfc.org/",'(PacFIN).', target="_blank"), 
                       'As part of the catch share program, participants of the fishery are required to complete', 
                tags$a(href='http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm', 'EDC survey forms', target="_blank"), 'as stated in', 
                tags$a(href="http://www.ecfr.gov/cgi-bin/text-idx?SID=06f0c396e52e564ce22a048aa910f49f&node=50:13.0.1.1.1.4.1.5&rgn=div8",'regulation 50 CFR 660.114.', target="_blank"), 
                'Data collection began in 2009, two years prior to implementing the catch share program. All vessels that participate in the catch share program must report 
                data for all fisheries they participate in, including non-catch share fisheries.'), 
               
         tags$p('FISHEyE Performance Metrics is user driven and interactive. Information on how to use FISHEyE Performance Metrics is available in the', tags$em("Instructions"), 'tab.
                Information on the variables in the dataset and definitions of the statistics and metrics used are found in the', tags$em("Definitions"), 'tab.'),
         tags$p('The metrics provided are not an exhaustive list of potential performance metrics. They were chosen based on their ability to present meaningful information about the fishery 
                that can be tracked over time, as well as data availability. Additional information can be explored using the summary data available from',
                tags$a(href='https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/NetRevExplorer/', 'FISHEyE Net Revenue Explorer.', target="_blank")),
         
         tags$hr(),
         tags$h3("A note about confidentiality"), 
         p('Data confidentiality requirements do not allow us to show individual observations. Therefore, we aggregate or summarize the data to protect individual confidentiality.'),
         p('Data queries that would display confidential data are not plotted or made available to download. In these cases, a message will appear below the figure to indicate that data 
           are suppressed due to confidentiality. This message will help you to differentiate suppressed confidential data from data points that do not exist.'),
         p("There are some cases where there are not enough vessels to differentiate between results summed across all vessels, whiting vessels, and non-whiting vessels. This occurs when there 
           is less than three vessels at either the whiting or non-whiting level. When this happens, we suppress results that would reveal confidential data and indicate that we have done so in a 
           message below the plot and in the data table. More information on data confidentiality requirements can be found in the",
         tags$a(href = "http://www.nwfsc.noaa.gov/research/divisions/fram/documents/Administration_Operations_Report_2014.pdf","EDC Administration and Operations Report.", target="_blank")),
         
         tags$hr(),
         tags$h3("Please visit the following websites for more information on:"),
           # tags$p("General information about West Coast Groundfish Management"),
            HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 2.35em; margin-top:5px; margin-bottom:10px;'>
                <a href='http://www.pcouncil.org/groundfish/background/' target='_blank'>Pacific Fishery Management Council </a><br>          
                <a href='http://www.westcoast.fisheries.noaa.gov/' target='_blank'>West Coast Regional Office </a><br>
                <a href='http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/' target='_blank'>West Coast Groundfish Trawl Catch Share Program </a><br>
                <a href='http://www.nwfsc.noaa.gov/index.cfm' target='_blank'>Northwest Fishery Science Center (NWFSC)</a></div>"),              
            
            tags$ul(tags$li(tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/index.cfm", "Economic and Social Science Research (ESSR) Program at the NWFSC", target="_blank")),  
            tags$li(tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm","The Economic Data Collection (EDC) Program", target="_blank"),
            tags$ul(#tags$li( "EDC General Information")),
            tags$li(tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_reports.cfm","EDC Reports",target="_blank")),       
            tags$li(tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm","EDC Forms",target="_blank")),      
            tags$li(tags$a(href="http://www.nwfsc.noaa.gov/news/features/infographics/index.cfm", "Infographic describing the sectors participating in the catch share program", target="_blank"))))),
            
      HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 2.35em; margin-top:5px; margin-bottom:10px;'>
           <a href='http://pacfin.psmfc.org/' target='_blank'>Pacific Fisheries Information Network (PacFIN) </a><br>
          <a href='http://calcomfish.ucsc.edu/regulation_main.asp' target='_blank'>Commercial Regulations Database - a database containing federal commercial groundfish regulations since 1983 </a></div>"),


         tags$hr(),
         tags$h3("Disclaimer"),
         tags$p("The data used in this application are periodically updated and subject to change.See the blog post for information on when data were last updated."),
         
         tags$hr(),
         
 #        tags$h3("Contact"),
#         tags$p("Please email us at", strong("nwfsc.fisheye@noaa.gov"),
#        # tags$a(href="mailto:nwfsc.fisheye@noaa.gov?subject=FISHEyE", 'nwfsc.fisheye@noaa.gov'),
#         tags$br()),
         
#         tags$p(
#           "Melanie Harsch", tags$br(), 
#           "Contractor-ECS Federal, Inc.", tags$br(),
#           "In support of NMFS", tags$br(),
#           "Northwest Fisheries Science Center", tags$br(),
#           "nwfsc.fisheye@noaa.gov", tags$br(),
#           tags$br(),
#           "Todd Lee", tags$br(),
#           "ESSR Program Manager", tags$br(),
#           "Northwest Fisheries Science Center", tags$br(),
#           "nwfsc.fisheye@noaa.gov", tags$br()),
#       tags$hr(),
       tags$h3("Acknowledgements"),
       tags$p("There are numerous individuals to thank for their contributions in developing FISHEyE. We thank the Northwest Fisheries Science Center (NWFSC) 
              economists and application developers, Scientific Data Management staff, and Information Technology staff. We thank PacFIN for providing landings
              information. NMFS Office of Science and Technology and NMFS Office of Sustainable Fisheries provided support. Numerous individuals reviewed the application 
              and we thank them for their helpful comments and suggestions. Finally and very importantly, we thank the members of the West Coast fishing industry who 
              have supplied information to the EDC Program.",
         tags$br(),
         tags$br(),
         tags$br(),
         tags$br())
         )      


