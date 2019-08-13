# Text for definitions page

tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         # HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
         #      <h3>Definitions</h3></div>",
         #      '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Definitions <br> in new browser tab</a>'
         # ),
                 tags$div(
                   h3("Filters"),
                   p('Please note that availability of filters will vary between sectors.'),
                   tags$br(),
                   h4("Fisheries"),
                    p('FISHEyE uses the same fisheries definitions as those used by the',
                      tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'EDC Program', target="_blank"), 
                      'EDC program to characterize the',
                      tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish/", 'US West Coast Groundfish fishery.', target="_blank"),
                      'Many vessels that participate in the catch share program also participate in other fisheries such as Dungeness crab and pink shrimp or Alaskan fisheries. 
                      The fisheries are defined as follows:'),
                   tags$img(src='Fisheries_table.jpg', height=400),
                   tags$br(),
                   p('Complete information is unavailable for the Alaskan portion of vessels fishing operations, but information about the role of 
                     Alaskan fisheries can be explored using the following Vessel characteristics metrics:'),
                   tags$ul(
                     tags$li("Number of fisheries"),
                     tags$li("Proportion of revenue"),
                     tags$li("Revenue diversification")),
                   tags$br(),
                   h4("Production activities"),
                   p("For shorebased processors, information on the EDC form is collected at the species level (e.g. fish production information), not the 
                     fishery level like the catcher vessels."),
                    tags$ul(
                      tags$li(strong("Pacific whiting:"),'Pacific whiting.'),
                      tags$li(strong("Non-whiting groundfish:"),'Arrowtooth flounder; Dover sole; English sole; Lingcod; Pacific sanddab; 
                              Petrale sole; Rex sole; Rockfish; Sablefish (black cod); Thornyheads; Sharks, skates, and rays.'),
                      tags$li(strong("Other species:"), "Coastal pelagics (including sardines and mackerel); Crab; Enchinoderms (including sea urchins and sea cucumbers); 
                              California halibut; Pacific halibut; Pacific herring; Salmon; Squid; Sturgeon; Tuna; Other shellfish; Other species.")
                       ), tags$br(),             
                    h4('Homeport'),
                   p("The",
                     tags$a(href="http://127.0.0.1:4612/map.pdf", 'homeport', target="_blank"), 
                     "reported by each vessel on the",
                     tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC survey form', target="_blank"), 
                     "aggregated to port groups. Vessels that report their homeport to be in Alaska are included in the Puget Sound port."),
                   tags$br(),
                    h4('State of homeport'),
                    p('The state corresponding to each homeport. Vessels that report their homeport to be in Alaska are included in Washington State.'),
                   tags$br(),
                   h4('Region'),
                   p('The region corresponding to each processor. For first receivers and shorebased processors, two regions are recognized: Washington and Oregon and California.'),
                   tags$br(),
                   h4("Vessel length class"),
                   p("Three categories of vessel length representing the range of catcher vessel length: large vessels (> 80 ft), medium vessels (> 60 ft, <= 80 ft), and small vessels (<= 60 ft)."),
                   tags$br(),
                   h4("Processor size"),
                   p("First receivers and shorebased processors are grouped into three size classes based on the average number of workers: large (> 200 workers), medium (100 - 200 workers), and 
                     small (< 100 workers). Processor size was determined by examining the maximum weekly number of production workers employed by a processor (as reported on the EDC form) for 
                     each year. Then the weekly maximum was averaged across all EDC years to place processors in one size category for all years."),
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
                   h4("Include Alaskan fisheries activities"),
                   p("For some metrics (e.g., Fishery diversification, Number of fisheries), you may choose whether or not to include activities in Alaskan fisheries."),
                   tags$br(),
                   h4("Years"),
                   p("The catch share program began in 2011. A mandatory",
                     tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'Economic Data Collection (EDC) program', target="_blank"),
                     "began in 2009. Results are available from 2009 onwards for metrics that require EDC data. For metrics that do not require EDC data, results are available from 2004 onwards. 
                     You can use the slider bar to select the appropriate range of years.")
                   )
                   )