# Text for definitions page

tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
                     <h3>Definitions</h3></div>",
              '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Definitions <br> in new browser tab</a>'
         ),
         #tags$h3('Definitions'),
         tags$div(
    p("The variables and their definitions used in 
      this application are from the", tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_reports.cfm", '2014 Economic Data Collection (EDC) reports. ', target="_blank")#, 
#      " A copy of this report and reports from other years can be found on the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_reports.cfm", "EDC reports webpage. ", target="_blank"), 
#      " Updated EDC reports that include the 2013 and 2014 data will be available in early 2016. For an overview of catcher vessels activities, vessel characteristics, and vessel economic
#      performance measures, please visit this ",
#      tags$a(href="2012CatcherVessel.jpg", 'infographic.', target="_blank")) 
  ),  
  tags$div(style= "margin-top: 15px;",
           
   h4("Summary Variables"),
     tags$ul(
       tags$li(strong("Fisheries:"),'The Costs Explorer uses the same fisheries definitions as those used by the', 
                tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", target="_blank", 'EDC program'), 'to characterize the', 
                tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish/", 'US West Coast Groundfish fishery. ', target="_blank"), 
               ' Many vessels that participate in the catch share program also participate in other fisheries such as Dungeness crab or Alaskan fisheries.'),
         p(tags$i('Catch share fisheries:'),' At-sea Pacific whiting; 
                Shoreside Pacific whiting;
                Groundfish fixed gear with trawl endorsement;
                DTS (Dover, Thornyhead, and Sablefish) trawl with trawl endorsement;
                Non-whiting, Non-DTS trawl with trawl endorsement (e.g., petrale sole, rockfish, and other flatfish);
                Non-whiting midwater trawl (uses midwater trawl gear to target pelagic rockfish such as widow rockfish and yellowtail rockfish).'),
         p(tags$i('Non-catch share fisheries:'), 
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
       
       tags$li(strong("Vessel length class:"),"Three categories of vessel length representing the range of catcher vessel length: large vessels (> 80 ft), medium vessels (60 ft - 80 ft), and small vessels (< 60 ft)."),
       tags$li(strong("Processor size:"),"First receivers and shorebased processors are grouped into three size classes based on the average number of workers: large (> 200 workers), medium (100 - 200 workers), and small (< 100 workers).
                              Processor size was determined by examining the maximum weekly number of production workers employed by a processor (as reported on the EDC form) for each year. Then the
                              weekly maximum was averaged across all EDC years to place processors in one size category for all years.")
     ),  tags$br(),             

    h4("Statistics"),
  tags$ul(
    
    tags$li(strong("Median or Average")),
    tags$ul(
      tags$li(strong("Per vessel or processor:"),"Median or Average for all vessels or processors that participated in the catch share program."),
#      tags$li(strong("Per vessel per day:"),
#      "Median or Average per day for all vessels that participated in the Catch Share program."),
#    tags$li(strong("Per vessel or processor per metric-ton:"),
#      "Median or Average per metric ton of fish caught or processed for all vessels or processors that participated in the Catch Share program.")),tags$br(),
    tags$li(strong("Fleet-wide and industry-wide total:"),"Summed over all vessels or processors that participated in the catch share program.") ),tags$br(),
    tags$ul(tags$p(tags$strong("What is the purpose of using median, average, and total values when looking at the data?"), "The median and average provide information about a representative vessel or processor; 
           however, they do it in different ways.  The ",  tags$strong("median"), "means that half of the vessels or processors have a larger result than the median, and half are smaller.  The ", tags$strong("average "),"
            is the sum of the values divided by the number of responses.  If the data do not have many extreme responses in a particular direction, the median and average will be very similar.  However, if the data are ", 
           tags$em('skewed'), "by extreme responses, then the median is a better measure for a typical vessel or processor.  
           The ", tags$strong("total"), " is the sum of the costs over all participants. It is a measure of fleet-wide or industry-wide performance, rather than a measure of individual vessel or processor performance."))
          ),
    tags$br(),

      h4("Costs Measures"),
      tags$ul(tags$em("Values reported in inflation-adjusted 2015 dollars.")),
      tags$ul('Quota earnings and costs are currently excluded from these table due to data limitations. It is not possible to convert quota data to the calendar year format like the rest of the cost categories.
              Additionally, many quota trades are non-cash transactions and therefore cannot be included in the calculations.'),
       tags$ul('Please note that the availability of cost measures will vary by sector.'),

      img(src="CostMeasTable.png", height=550),
      
#tags$ul(
#      tags$li(strong("Variable cost: vary with the level of fishery participation, and generally include items such as fuel and crew compensation.")),
#              tags$ul(
#              tags$li(strong('Buyback fees')),
#              tags$li(strong('Captain')),
#              tags$li(strong('Cost recovery fees')),
#              tags$li(strong('Crew')),
#              tags$li(strong('Fish purchases')),
#              tags$li(strong('Freight:'), ' Reported separately for first receivers and shorebased processors. Included in',tags$em('Other variable costs'),'for the other sectors'),
#              tags$li(strong('Fuel')),
#              tags$li(strong('Labor:'), ' Includes wages, bonuses, benefits, payroll taxes, and unemployment insurance.'),
#              tags$li(strong('Monitoring')),
#              tags$li(strong('Non-processing crew')),
#              tags$li(strong('Off-site freezing & storage')),
#              tags$li(strong('Observers')),
#              tags$li(strong('Packing materials:'), ' Reported separately for first receivers and shorebased processors. Included in',tags$em('Other variable costs'),'for the catcher-processors and motherships'),
#              tags$li(strong('Processing crew')),
#              tags$li(strong('Utilities:'), 'Includes expenses on electricity, natural gas, nitrogen gas, propane gas, water, and sewer, waste, and byproduct disposal.'),
#              tags$li(strong('Other variable costs:'), ' Includes food, ice, supplies, travel, offloading, trucking of fish, fishing association dues, fish taxes, freight.')
#              ),
#      tags$li(strong("Fixed costs: do not vary as directly with the level of fishery participation, and generally include items such as vessel capital improvements")),
#        tags$ul(
#          tags$li(strong('Capitalized expenditures:'), 'Includes costs that are depreciated over a number of years.'),
          #tags$li('Equipment:   Includes all electronics, safety equipment, and machinery not used to harvest fish, but not fishing gear or processing equipment.'),
#          tags$li(strong('Fishing gear:'),  ' Includes nets, doors, traps, pots, cables, and fishing machinery used for the West Coast fisheries.'),
#          tags$li(strong('On-board equipment:'),   ' Includes all electronics, safety equipment, and machinery not used to harvest fish, but not fishing gear or processing equipment.'),
#          tags$li(strong('Processing equipment:'),  ' Includes any equipment used to process or head and gut fish on-board the vessel. For shorebased processors, includes repair and maintenance costs.'),
#          tags$li(strong('Other fixed costs:'),  ' Includes insurance, moorage, lease of vessel, and license fees')
#        )
#      ),
tags$br(),
tags$ul(strong("A note about total costs."),em(' Please note that there are a variety of costs that are associated with operating a vessel or processing facility that are not requested on the form because it is difficult to determine the 
        share of the cost associated with the vessel or facility. These costs include items that can be used for activities other than fishing, or are too difficult to allocate to a particular 
        vessel or facility in a multi-vessel company. These expenses include office space, vehicles, storage of equipment, professional fees, and marketing. In general, the EDC form aims to capture 
        costs that are directly related to vessel or facility maintenance and fishing or processing operations, and not costs that are related to activities or equipment off the vessel or beyond 
        the facility. For these reasons, the aggregated measures of costs presented here underestimate the true costs of operating a business.')),
   tags$br(),

h4("Pacific Whiting Vessel Category"),
tags$ul(
  tags$p("For the catcher vessel sector, the selected cost measure is calculated at three levels: 1) for the fleet as a whole, 2) for vessels that fished for Pacific whiting, and 3) for vessels that participate in the non-whiting groundfish sector of the limited entry trawl fishery. 
         Vessels that fished for both whiting and non-whiting are included in the whiting vessel category. The purpose of separating the whiting from the non-whiting vessels is that the whiting vessels tend to be larger and catch a higher volume of fish. In addition, total allowable catch for Pacific whiting has more annual variation than total annual catch for species targeted in the non-whiting groundfish sector."),
  tags$p('This division is not appropriate for the mothership and catcher-processor sectors. For these sectors, only Pacific whiting is shown.'),
  tags$p('There are some cases where there are not enough vessels to separate the vessels into the whiting and non-whiting vessel categories. This occurs when there is less than three vessels at either the whiting or non-whiting level. When this happens, we only report the "All vessels" category and indicate that we have done so in a message below the plot and in the data table. More information on data confidentiality requirements can be found in the EDC Administration and Operations Report.')
  ), tags$br(),                   


  h4("Variables In The Data Table"),
 tags$ul(
   tags$li(strong('Year:'), 'Calendar year over which fishing activities occurred.'),
   tags$li(strong('Summary variables:'),  'Summary variables (fisheries, homeport, state, vessel length class) are used to group vessels to better understand the costs and expenses for particular segments of the fleet.'),
   tags$li(strong('Costs categories:'), 'See above for a description of the costs measures.'),
   tags$li(strong('Number of vessels/processors:'), 'The number of observations for each displayed statistic.'),
   tags$li(strong('Statistic:'), 'See above for further description of the statistics.'),
   tags$li(strong('Value:'), 'Value of the cost measures for the statistic.'),
   tags$li(strong('Fisheries category:'), 'This variable is relevant for when vessels are grouped by homeport, state, or vessel length class. It indicates whether the reported value of the 
          selected cost measure includes activity in all fisheries or only in the catch share fisheries or the non-catch share fisheries.'),
   tags$li(strong('Production category:'), 'This variable is relevant for first receivers and shorebased processors. It indicates whether the reported value of the selected cost 
           measure includes  all production or only groundfish production or other species production.'),
   tags$li(strong('Variance:'), 'A measure of the spread of values. We report the standard deviation (SD) when vessel averages are selected and the median absolute deviation (MAD) when vessel medians are selected.'),
   tags$li(strong('Data summed across:'), 'For first receivers and shorebased processors, we show data summed across all processors or processors that process catch share species.'),
   tags$p(tags$br(),
          tags$br(),
          tags$br(),
          tags$br())
  )
))
)
