tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
                     <h3>Definitions</h3></div>",
              '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Definitions <br> in new browser tab</a>'
         ),
         #tags$h3('Definitions'),
         tags$div(
    p("The variables and their definitions used in 
      this application are from the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/documents/EDC_Catcher_Vessel_Report_2015.pdf", '2012 Economic Data Collection (EDC) report. ', target="_blank")#, 
#      " A copy of this report and reports from other years can be found on the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_reports.cfm", "EDC reports webpage. ", target="_blank"), 
#      " Updated EDC reports that include the 2013 and 2014 data will be available in early 2016. For an overview of catcher vessels activities, vessel characteristics, and vessel economic
#      performance measures, please visit this ",
#      tags$a(href="2012CatcherVessel.jpg", 'infographic.', target="_blank")) 
  ),  
  tags$div(style= "margin-top: 15px;",
           
   h4("Summary Variables"),
     tags$ul(
       tags$li(strong("Fisheries:"),'The Net Revenue Explorer uses the same fisheries definitions as those used by the', 
                tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", target="_blank", 'EDC program'), 'to characterize the', 
                tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish/", 'US West Coast Groundfish fishery. ', target="_blank"), 
               ' Many vessels that participate in the Catch Share program also participate in other fisheries such as Dungeness crab or Alaskan fisheries. 
                The Net Revenue Explorer currently shows data for catcher vessels (both at-sea and shoreside).'),
         p(tags$i('Catch Share fisheries:'),' At-sea Pacific whiting; 
                Shoreside Pacific whiting;
                Groundfish fixed gear with trawl endorsement;
                DTS (Dover, Thornyhead, and Sablefish) trawl with trawl endorsement;
                Non-whiting, Non-DTS trawl with trawl endorsement (e.g., petrale sole, rockfish, and other flatfish).'),
         p(tags$i('Non-catch Share fisheries:'), 
                'Groundfish fixed gear with fixed gear endorsement;
                Crab; Shrimp; Other fisheries (e.g., tuna, salmon, and halibut).'),
        
       tags$li(strong("Vessel length class:"),"Three categories of vessel length representing the range of catcher vessel length: large vessels (> 80 ft), medium vessels (> 60 ft, <= 80 ft), and small vessels (< 60 ft)."),
       tags$li(strong("Homeport:"), "The", tags$a(href="map.pdf", "homeport", target="_blank"), "reported by each vessel on the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", "EDC survey form", target="_blank"), "aggregated to port groups."), 
       tags$li(strong("State:"), "The state corresponding to each homeport.")
      ),  tags$br(),             

    h4("Statistics"),
  tags$ul(
    
    tags$li(strong("Median or Average")),
    tags$ul(
      tags$li(strong("Per vessel:"),"Median or Average for all vessels that participated in the Catch Share program."),
    tags$li(strong("Per vessel per day:"),
      "Median or Average per day for all vessels that participated in the Catch Share program."),
    tags$li(strong("Per vessel per metric-ton:"),
      "Median or Average per metric ton of fish caught for all vessels that participated in the Catch Share program.")),tags$br(),
    tags$li(strong("Fleet-wide total:"),"Summed over all vessels that participated in the Catch Share program.") ),tags$br(),
    tags$ul(tags$p(tags$strong("What is the purpose of using median, average, and fleet-wide total values when looking at the data?"), "The median and average provide information about a representative vessel; 
           however, they do it in different ways.  The ",  tags$strong("median"), "means that half of the vessels have a larger result than the median, and half are smaller.  The ", tags$strong("average "),"
            is the sum of the values divided by the number of responses.  If the data do not have many extreme responses in a particular direction, the median and average will be very similar.  However, if the data are ", 
           tags$em('skewed'), "by extreme responses, then the median is a better measure for a typical vessel.  The ", tags$strong("fleet-wide total"), "measures how the entire fleet is doing, rather than a representative vessel."))
    ,tags$br(),

      h4("Economic Measures"),
      tags$ul(
        tags$li(strong("Revenue:"),    
          "Revenue is the total revenue generated by participation in the selected fisheries."
          ),      
        tags$li(strong("Variable cost:"),
          "Vary with the level of participation in a fishery and generally include items such as fuel and crew payments.", 
             tags$ul("See Table 9.1 of the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/documents/EDC_Catcher_Vessel_Report_2015.pdf", '2012 EDC report', target="_blank"), "for the full listing of 
             variable cost categories.")),
        tags$li(strong("Fixed cost:"),
           "Do not vary with the level of fishing participation or do not vary as directly as variable costs with the level of participation during a given year. 
             The", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'EDC program', target="_blank"), "collects four types of fixed costs:",
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
        tags$p(strong('A note about net revenue:'),  'The', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC forms', target="_blank"), 'capture costs that are directly related to vessel fishing operations, and do not include other expenses 
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

  h4("Variables in the Data Table"),
 tags$ul(
   tags$li(strong('Year:'), 'Calendar year over which fishing activities occurred.'),
   tags$li(strong('Summary variables:'),  'Summary variables (fisheries, homeport, state, vessel length class) are used to group vessels to better understand the economic performance of particular segments of the fleet.'),
   tags$li(strong('Economic measure:'), 'See above for a description of the economic measures.'),
   tags$li(strong('Number of vessels:'), 'The number of observations for each displayed statistic.'),
   tags$li(strong('Statistic:'), 'See above for further description of the statistics.'),
   tags$li(strong('Value:'), 'Value of the economic measures for the statistic.'),
   tags$li(strong('Fished in Alaska:'), 'Fished in Alaskan fisheries (Vessels included or Vessels not included).'),
   tags$li(strong('Fished for whiting:'), 'Fished for Pacific whiting (Vessels included or Vessels not included).'),
   tags$li(strong('Fisheries Category:'), 'This variable is relevant for when vessels are grouped by homeport, state, or vessel length class. It indicates whether the reported value of the selected economic 
           measure includes activity in all fisheries or only in the Catch Share fisheries or the non-Catch Share fisheries.'),
   tags$li(strong('Variance:'), 'A measure of the spread of values. We report the standard deviation (SD) when vessel averages are selected and the median absolute deviation (MAD) when vessel medians are selected.'),
   tags$li(strong('Thirds:'), 'This variable is only relevant for the', tags$em('Fleet-wide Variability Analysis.'), 'Vessels are grouped annually into three tiered categories based on revenue: top, middle, and lower revenue earners.'),
   tags$p(tags$br(),
          tags$br(),
          tags$br(),
          tags$br())
  )
))
)
