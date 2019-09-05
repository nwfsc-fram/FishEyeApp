# Text for definitions page

tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         # HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
         #      <h3>Definitions</h3></div>",
         #      '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Definitions <br> in new browser tab</a>'
         # ),
                 tags$div(
                   h3("Participant category"),
                   p('Please note that availability of filters will vary between sectors.'),
                   tags$br(),
                   h4("Fisheries"),
                    p('FISHEyE uses the same fisheries definitions as those used by the',
                      tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'EDC Program', target="_blank"), 
                      'to characterize the',
                      tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish/", 'US West Coast Groundfish fishery.', target="_blank"),
                      'Many vessels that participate in the catch share program also participate in other fisheries such as Dungeness crab and pink shrimp or Alaskan fisheries. 
                      The fisheries are defined as follows:'),
                   tags$img(src='Fisheries_table.jpg', height=400),
                   tags$br(),
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
                   tags$br()
                   )
                   )