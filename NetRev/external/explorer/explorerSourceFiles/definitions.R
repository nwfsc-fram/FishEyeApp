# Text for definitions page

tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
                     <h3>Definitions</h3></div>",
              '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Definitions <br> in new browser tab</a>'
         ),
         #tags$h3('Definitions'),
         tags$div(
    p("The variables and their definitions used in 
      this application are from the", tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/documents/EDC_Catcher_Vessel_Report_October_2016.pdf", '2014 Economic Data Collection (EDC) report. ', target="_blank")#, 
#      " A copy of this report and reports from other years can be found on the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_reports.cfm", "EDC reports webpage. ", target="_blank"), 
#      " Updated EDC reports that include the 2013 and 2014 data will be available in early 2016. For an overview of catcher vessels activities, vessel characteristics, and vessel economic
#      performance measures, please visit this ",
#      tags$a(href="2012CatcherVessel.jpg", 'infographic.', target="_blank")) 
  ),  
  tags$div(style= "margin-top: 15px;",
           
   h4("Summary Variables"),
     tags$ul(
       tags$li(strong("Fisheries:"),'The Net Revenue Explorer uses the same fisheries definitions as those used by the', 
                tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", target="_blank", 'EDC program'), 'to characterize the', 
                tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish/", 'US West Coast Groundfish fishery. ', target="_blank"), 
               ' Many vessels that participate in the catch share program also participate in other fisheries such as Dungeness crab or Alaskan fisheries. 
                The Net Revenue Explorer currently shows data for catcher vessels (both at-sea and shoreside).'),
         p(tags$i('Catch share fisheries:'),' Pacific whiting (includes at-sea and shoreside Pacific whiting); 
                At-sea Pacific whiting; 
                Shoreside Pacific whiting;
                Groundfish fixed gear with trawl endorsement;
                Groundfish with trawl gear (includes groundfish DTS with trawl endorsement, non-whiting, non-DTS trawl with trawl endorsement, and non-whiting midwater trawl);
                DTS (Dover, Thornyhead, and Sablefish) trawl with trawl endorsement;
                Non-whiting, Non-DTS trawl with trawl endorsement (e.g., petrale sole, rockfish, and other flatfish);
                Non-whiting midwater trawl (uses midwater trawl gear to target pelagic rockfish such as widow rockfish and yellowtail rockfish).'),
         p(tags$i('Non-catch Share fisheries:'), 
                'Groundfish fixed gear with fixed gear endorsement;
                Crab; Shrimp; Other fisheries (e.g., tuna, salmon, and halibut).'),
        
       tags$li(strong("Production activities:"),'For shorebased processors, information on the EDC form  is collected at the species level (e.g. fish production information), not the fishery level like the catcher vessels.'),
       p(tags$i('Pacific whiting:'),' Pacific whiting.'),
       p(tags$i('Non-whiting groundfish:'),' Arrowtooth flounder; Dover sole; English sole; Lingcod; Pacific sanddab; Petrale sole; Rex sole; Rockfish; Sablefish (black cod); Thornyheads; Sharks, skates, and rays.'),
       p(tags$i('Other species:'),' Coastal pelagics (including sardines and mackerel); Crab; Enchinoderms (including sea urchins and sea cucumbers);
         California halibut; Pacific halibut; Pacific herring; Salmon; Squid; Sturgeon; Tuna; Other shellfish; Other species.'),
       
       tags$li(strong("Homeport:"), "The", tags$a(href="map.pdf", "homeport", target="_blank"), "reported by each vessel on the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", "EDC survey form", target="_blank"), "aggregated to port groups."), 
       tags$li(strong("State of homeport:"), "The state corresponding to each homeport. Vessels that fished off the West coast but report their homeport to be in Alaska are included. These vessels are included in the Puget Sound port."),
       tags$li(strong("Region:"), "The region corresponding to each processor. For first receivers and shorebased processors, two regions are recognized:", tags$em('Washington and Oregon'), 'and', tags$em("California.")),
       
       tags$li(strong("Vessel length class:"),"Three categories of vessel length representing the range of catcher vessel length: large vessels (> 80 ft), medium vessels (> 60 ft, <= 80 ft), and small vessels (< 60 ft)."),
       tags$li(strong("Processor size:"),"First receivers and shorebased processors are grouped into three size classes based on the average number of workers: large (> 200 workers), medium (100 - 200 workers), and small (< 100 workers).
                              Processor size was determined by examining the maximum weekly number of production workers employed by a processor (as reported on the EDC form) for each year. Then the
                              weekly maximum was averaged across all EDC years to place processors in one size category for all years.")
     ),  tags$br(),             

    h4("Statistics"),
  tags$ul(
    
    tags$li(strong("Median or Average")),
    tags$ul(
      tags$li(strong("Per vessel or processor:"),"Median or Average for all vessels or processors that participated in the catch share program."),
    tags$li(strong("Per vessel per day:"),
      "Median or Average per day for all vessels that participated in the catch share program."),
    tags$li(strong("Per vessel or processor per metric-ton:"),
      "Median or Average per metric ton of fish caught or processed for all vessels or processors that participated in the catch share program.")),tags$br(),
    tags$li(strong("Fleet-wide and industry-wide total:"),"Summed over all vessels or processors that participated in the catch share program.") ),tags$br(),
    tags$ul(tags$p(tags$strong("What is the purpose of using median, average, and total values when looking at the data?"), "The median and average provide information about a representative vessel or processor; 
           however, they do it in different ways.  The ",  tags$strong("median"), "means that half of the vessels or processors have a larger result than the median, and half are smaller.  The ", tags$strong("average "),"
            is the sum of the values divided by the number of responses.  If the data do not have many extreme responses in a particular direction, the median and average will be very similar.  However, if the data are ", 
           tags$em('skewed'), "by extreme responses, then the median is a better measure for a typical vessel or processor.  The ", tags$strong("total"), "measures how the entire fleet or processors are doing, rather than a representative vessel or processor."))
    ,tags$br(),

      h4("Economic Measures"),
      tags$ul(tags$em("Values reported in inflation adjusted 2015 dollars.")),
      tags$ul(
        tags$li(strong("Revenue:"),    
          "Revenue is the total revenue generated by participation in the selected fisheries. Values reported in inflation adjusted 2014 dollars."
          ),      
        tags$li(strong("Variable cost:"),
          "Vary with the level of participation in a fishery and generally include items such as fuel and crew payments.", 
             tags$ul("See Table 9.1 of the", tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/documents/EDC_Catcher_Vessel_Report_October_2016.pdf", '2014 EDC report', target="_blank"), "for the full listing of 
             variable cost categories.")),
        tags$li(strong("Fixed cost:"),
           "Do not vary with the level of fishing participation or do not vary as directly as variable costs with the level of participation during a given year. 
             The", tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'EDC program', target="_blank"), "collects four types of fixed costs:",
             tags$ul('1) New and used vessel and on-board equipment (includes all electronics, safety equipment, and machinery not used to harvest fish)'), 
             tags$ul('2) Fishing gear (includes nets, doors, traps, pots, cables, and fishing machinery used for the West Coast fisheries)'), 
             tags$ul('3) Processing equipment (includes any equipment used to process or head and gut fish on board the vessel) and'),
             tags$ul('4) Other fixed costs such as moorage, insurance, and lease permits.')),
        tags$li(strong("Variable cost net revenue (VCNR):"),
           "Revenue minus variable costs.", tags$br(), "VCNR is a measure of the operating profit of the average vessel."
             ),
        tags$li(strong("Total cost net revenue (TCNR):"),
          'Revenue minus variable and fixed costs. Over many years, TCNR is a measure of long-term profitability.',tags$br(),
          'In any given year, a vessel may have a large fixed cost expense (such as a new engine) which may lead to a negative or unusually low TCNR.'),
        tags$p(strong('A note about net revenue:'),  'The', tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC forms', target="_blank"), 'capture costs that are directly related to vessel fishing operations, and do not include other expenses 
            such as vehicles or office costs that may be related to the fishing business. Therefore, the net revenue reported here is an overestimate of the true net revenue.')
  ),
  tags$br(),
h4("Fished in Alaska (AK)"),
  tags$ul(
         tags$p("Vessels that fished in Alaska (versus vessels that fished solely off the West Coast) can be filtered from the data. Vessels that fished in Alaska tend to be longer (>90') than
              vessels that only fished off the West Coast (65' on average). Vessels that fished in Alaska also tend to participate in different 
            fisheries than vessels that fished solely off the West Coast."), 
      tags$p("If this box", tags$strong('is not selected,'), "then only vessels that participated solely in the West Coast fishery will be included. If this box", tags$strong('is selected,'), 
              "then all vessels, including those that participated in Alaskan fisheries, will be included.", tags$strong("Data from their activity in Alaskan fisheries are never included."),
             "Please note that there are some cases where deselecting this button would reveal confidential data. The results shown may
      include vessels that fished in Alaskan fisheries. See the confidentiality section under the", tags$em('About'), "tab for more information.")
  ), tags$br(),

h4("Fished for Pacific whiting"),
tags$ul(
  tags$p("Vessels that also fished for Pacific whiting (versus vessels that did not fish for Pacific whiting) can be filtered from the data. Vessels that fished for Pacific whiting tend to be larger and catch a higher volume of fish.
         In addition, total allowable catch for Pacific whiting has more annual variation than total annual catch for species targeted in the non-whiting groundfish sector."), 
  tags$p("If this box", tags$strong('is not selected,'), "then only vessels that did not fish for Pacific whiting will be included. If this box", tags$strong('is selected,'), 
         "then all vessels, including those that also fished for Pacific whiting, will be included.", 
         "Please note there are some cases where deselecting this button would reveal confidential data.  The results shown may 
         include vessels that fished for Pacific whiting. See the confidentiality section under the", tags$em('About'), "tab for more information.")
), tags$br(),

h4("Additional selections for First Receivers and Shorebased Processors"),
tags$ul(
  tags$p(tags$strong('Show data for'),'selects whether to show results for all processors that turn in EDC forms of the subset of processors that purchased groundfish caught in the catch share program.'),
  tags$p(tags$strong('Show data summed across these production activities'), 'is relevant to the regional and processor size summary variables. Select whether to show economic data associated with all production (groundfish and other species), just groundfish production, or just other species production.')
  ), tags$br(),

  h4("Variables in the Data Table"),
 tags$ul(
   tags$li(strong('Year:'), 'Calendar year over which fishing activities occurred.'),
   tags$li(strong('Summary variables:'),  'Summary variables (fisheries, homeport, state, vessel length class) are used to group vessels to better understand the economic performance of particular segments of the fleet.'),
   tags$li(strong('Economic measure:'), 'See above for a description of the economic measures.'),
   tags$li(strong('Number of vessels/processors:'), 'The number of observations for each displayed statistic.'),
   tags$li(strong('Statistic:'), 'See above for further description of the statistics.'),
   tags$li(strong('Value:'), 'Value of the economic measures for the statistic.'),
   tags$li(strong('Fished in Alaska:'), 'Fished in Alaskan fisheries (Vessels included or Vessels not included).'),
   tags$li(strong('Fished for whiting:'), 'Fished for Pacific whiting (Vessels included or Vessels not included).'),
   tags$li(strong('Fisheries Category:'), 'This variable is relevant for when vessels are grouped by homeport, state, or vessel length class. It indicates whether the reported value of the selected economic 
           measure includes activity in all fisheries or only in the catch share fisheries or the non-catch share fisheries.'),
   tags$li(strong('Production Category:'), 'This variable is relevant for first receivers and shorebased processors. It indicates whether the reported value of the selected economic 
           measure includes  all production or only groundfish production or other species production.'),
   tags$li(strong('Variance:'), 'A measure of the spread of values. We report the standard deviation (SD) when vessel averages are selected and the median absolute deviation (MAD) when vessel medians are selected.'),
   tags$li(strong('Data summed across:'), 'For first receivers and shorebased processors, we show data summed across all processors or processors that process catch share species.'),
   tags$li(strong('Thirds:'), 'This variable is only relevant for the', tags$em('Fleet-wide Variability Analysis.'), 'Vessels are grouped annually into three tiered categories based on revenue: top, middle, and lower revenue earners.'),
   tags$p(tags$br(),
          tags$br(),
          tags$br(),
          tags$br())
  )
))
)
