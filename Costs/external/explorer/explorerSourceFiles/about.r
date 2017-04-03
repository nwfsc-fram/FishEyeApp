#Text for about page

tags$div(style = "margin: 15px; 15px;30px; 30px; width: 60%",
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
                     <h3>About</h3></div>"),
       #  tags$h3('About Net Revenue Explorer', style="border-top:10px; padding-top:10px; margin-top:10px"),
         tags$p(tags$br(),"Welcome to FISHeries Economics Explorer (FISHEyE) Costs Explorer for the ",
                tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html","West Coast Groundfish Trawl Catch Share Program,", target="_blank"), 'which began in 2011.  
                       The catch share program consists of cooperatives for the at-sea mothership (including catcher vessels and motherships) and catcher-processor fleets, and an individual 
                      fishing quota (IFQ) program for the shorebased trawl fleet (catcher vessels, and first receivers and shorebased processors). FISHEyE allows you to explore economic data pre- and post-catch share management.'),
               
                tags$p('Data used in FISHEyE come from', 
                tags$a(href='https://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm', 'EDC survey forms', target="_blank"),'and
                Pacific Fisheries Information Network', tags$a(href="http://pacfin.psmfc.org/",'(PacFIN).', target="_blank"),
                'As part of the catch share program, participants of the fishery are required to complete', tags$a(href='https://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm', 'EDC survey forms', target="_blank"),
                'as stated in', 
                tags$a(href="http://www.ecfr.gov/cgi-bin/text-idx?SID=06f0c396e52e564ce22a048aa910f49f&node=50:13.0.1.1.1.4.1.5&rgn=div8",'regulation 50 CFR 660.114', target="_blank"), 
                'for all fisheries they participate in, including non-catch share fisheries. Data collection began in 2009, two years prior to implementing the catch share program. 
                FISHEyE Costs Explorer allows for the evaluation of variable and fixed costs for',
                  tags$a(href="2012CatcherVessel.jpg","catcher vessels,", target="_blank"), tags$a(href="2012CatcherProcessor.jpg", 'catcher-processors,', target="_blank"), 
                  tags$a(href="2012Mothership.jpg", 'motherships,', target="_blank"), 'and ', 
                  tags$a(href="2012FirstRecieversShorebasedProcessors.jpg",'first receivers and shorebased processors.', target="_blank")),
               
                 
         tags$p('FISHEyE Costs Explorer is user-driven and interactive. Information on how to use FISHEyE Costs Explorer is available in the', tags$em("Instructions"), 'tab or the', 
                tags$em('Get started'),'sub-tab under', tags$em('Summary Plots and Data.'),
                'Information on the variables in the dataset and definitions of the statistics and measures used are 
                found in the', tags$em("Definitions"), 'tab. The plots and analyses are provided to aid exploration of the data. Further analyses can be done by downloading the data.'),
         
         tags$hr(),
         tags$h3("A note about confidentiality"), 
         p('Data confidentiality requirements do not allow us to show individual observations.
         Therefore, we aggregate or summarize the data to protect individual confidentiality.', tags$br(), tags$br(), 
         'Data queries that would display confidential data are not plotted or made available to download. In these cases, a message will appear that indicates which data are suppressed
         due to confidentiality. This message will help you to differentiate suppressed confidential data from data points that do not exist.',
         br(),br(),
         'More information on data confidentiality requirements can be found in the',
         tags$a(href = "https://www.nwfsc.noaa.gov/research/divisions/fram/documents/Administration_Operation_Report_October_2016.pdf",
         "EDC Administration and Operations Report.", target="_blank")),
         tags$hr(),
          tags$h3('A note about the timing when data become available:'), 
          p('EDC forms are submitted by September 1st each year for the previous fiscal year of the participant (i.e., FY2011 data were received in September 2012), 
                allowing participants to', tags$em('close their books'), 'and file taxes before completing their EDC forms. The QA/QC process requires approximately six months. This means that 2011 EDC data were available in March 2013; 
              however, there is one additional complication. Participants submit data by fiscal year which varies by company and may not correspond exactly to calendar year.
              Although data are uploaded into FISHEyE after the QA/QC process has been completed, the data are not considered finalized until the following year once the complete set of data have been received and processed. 
              As a result, calendar year 2014 are not finalized until Spring 2017.'),
      tags$hr(),
         tags$h3("Please visit the following websites for more information on:"),
           # tags$p("General information about West Coast Groundfish Management"),
            HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 2.35em; margin-top:5px; margin-bottom:10px;'>
                <a href='http://www.pcouncil.org/groundfish/background/' target='_blank'>Pacific Fishery Management Council </a><br>          
                <a href='http://www.westcoast.fisheries.noaa.gov/' target='_blank'>West Coast Regional Office </a><br>
                <a href='http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/' target='_blank'>West Coast Groundfish Trawl Catch Share Program </a><br>
                <a href='https://www.nwfsc.noaa.gov/index.cfm' target='_blank'>Northwest Fishery Science Center (NWFSC)</a></div>"),              
            
            tags$ul(tags$li(tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/index.cfm", "Economic and Social Science Research (ESSR) Program at the NWFSC", target="_blank")),  
            tags$li(tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm","The Economic Data Collection (EDC) Program", target="_blank"),
            tags$ul(#tags$li( "EDC General Information")),
            tags$li(tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_reports.cfm","EDC Reports",target="_blank")),       
            tags$li(tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm","EDC Forms",target="_blank")),      
            tags$li(tags$a(href="https://www.nwfsc.noaa.gov/news/features/infographics/index.cfm", "Infographic describing the sectors participating in the catch share program", target="_blank"))))),
            
            tags$a(href="http://pacfin.psmfc.org/","Pacific Fisheries Information Network (PacFIN)", target="_blank"),tags$br(),

       tags$br(),
       
         tags$hr(),
         tags$h3("Disclaimer"),
         tags$p("The data used in this application are periodically updated and subject to change."),
         
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
       tags$p("There are numerous individuals to thank for their contributions in developing FISHEyE. We thank Dr. Todd Lee for realizing FISHEyE. We thank the Northwest Fisheries Science Center (NWFSC) 
              economists and application developers, Scientific Data Management staff, and Information Technology staff. We thank PacFIN for providing landings
              information. NMFS Office of Science and Technology and NMFS Office of Sustainable Fisheries provided support. Numerous individuals reviewed the application 
              and we thank them for their helpful comments and suggestions. Finally, and very importantly, we thank the members of the West Coast fishing industry who 
              have supplied information to the EDC Program.",
         tags$br(),
         tags$br(),
         tags$br(),
         tags$br())
         )      


