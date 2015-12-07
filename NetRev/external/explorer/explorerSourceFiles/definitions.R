tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
                     <h3>Definitions</h3></div>",
              '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Definitions <br> in new browser tab</a>'
         ),
         #tags$h3('Definitions'),
         tags$div(
    p("The variables and their definitions used in 
      this application are from the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/documents/EDC_Catcher_Vessel_Report_2015.pdf", '2012 Economic Data Collection (EDC) report. ', target="_blank"), 
      " A copy of this report and reports from other years can be found on the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_reports.cfm", "EDC reports webpage. ", target="_blank"), 
      " Updated EDC reports that include the 2013 and 2014 data will be available in early 2016. For an overview of catcher vessels activities, vessel characteristics, and vessel economic
      performance measures, please visit this ",
      tags$a(href="2012CatcherVessel.jpg", 'infographic.', target="_blank")) 
  ),  
  tags$div(style= "margin-top: 15px;",
           
   h4("Summary Variables"),
     tags$ul(
       tags$li("Fisheries:"),
         tags$p('The Net Revenue Explorer uses the same fisheries definitions as those used by the', 
                tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", target="_blank", 'EDC program'), 'to characterize the', 
                tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish/", 'US West Coast Groundfish fishery.', target="_blank"),   'The',
                tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/", 'West Coast Groundfish Trawl Catch Share Program', target="_blank"), 
              'was implemented in 2011. EDC data prior to and after the implementation of the Catch Share program are included. 
               The Catch Share program consists of cooperatives for the at-sea mothership (including
                catcher vessels and motherships) and catcher-processor fleets, 
                and an individual fishing quota (IFQ) program for the shorebased trawl fleet (catcher vessels, and first receivers and shorebased processors).
                Many vessels that participate in the Catch Share program also participate in other fisheries such as crab or Alaskan fisheries. 
                The Net Revenue Explorer currently shows data for catcher vessels (both at-sea and shoreside). We are working to add the other sectors.'),
         p(tags$i('Catch Share fisheries:'),' At-sea Pacific whiting; 
                Shoreside Pacific whiting;
                Groundfish fixed gear with trawl endorsement;
                DTS (Dover, Thornyhead, and Sablefish) trawl with trawl endorsement;
                Non-whiting, Non-DTS trawl with trawl endorsement.'),
         p(tags$i('Non-catch share fisheries:'), 
                'Groundfish fixed gear with fixed gear endorsement;
                Crab; Shrimp; Other fisheries (e.g., tuna, salmon, and halibut.)'),
        
       tags$li("Vessel length class:"),
         tags$p("Three categories of vessel length representing the range of catcher vessel length. 
                The categories are large vessels (> 80 ft), medium vessels (> 60 ft, <= 80 ft), and small vessels (< 60 ft)."),
       tags$li("Homeport:"),
         tags$p("The", tags$a(href="map.pdf", "homeport", target="_blank"), "reported by each vessel on the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", "EDC survey form", target="_blank"), "aggregated to point groups.", 
       tags$li("State:"),
         tags$p("The state corresponding to each homeport.")
      )),  tags$br(),             

    h4("Statistics"),
  tags$ul(
    
    tags$li("Median or Average"),
    tags$ul(
      tags$li("Per vessel:"),
      tags$p("Median or Average for all vessels that participated in the Catch Share program."),
    tags$li("Per vessel per day:"),
      tags$p("Median or Average per day for all vessels that participated in the Catch Share program."),
    tags$li("Per vessel per metric-ton:"),
      tags$p("Median or Average per metric ton of fish caught for all vessels that participated in the Catch Share program.")),
    tags$li("Fleet-wide total:"),
    tags$ul(tags$p("Total for all vessels that participated in the Catch Share program.")),
    tags$p("What is the purpose of using median, average, and fleet-wide total values when looking at the data? The median and average both attempt to provide information about the results for a representative vessel, 
           however they do it in different ways.  The",  tags$strong("median"), "means that half of the vessels have a larger result than the median, and half are smaller.  The", tags$strong("average,"),"or mean, 
           is the sum of the values divided by the number of responses.  If the data do not have many extreme responses in a particular direction, the median and mean will be very similar.  However, if the data are", 
           tags$em('skewed'), "by extreme responses, then the median is a better measure of the result for a typical vessel.  The total provides a measure of the fleet as a whole.  The", tags$strong("fleet-wide total"), "is used to measure 
           how the entire fleet is doing, rather than a representative vessel.")
    ),tags$br(),

      h4("Economic measures"),
      tags$ul(
        tags$li("Revenue:"),    
          tags$p("Revenue is the total revenue generated by participation in the selected fisheries."
          ),      
        tags$li("Variable cost:"),
          tags$p("Variable costs vary with the level of participation
             in a fishery and generally include items such as fuel and crew payments. 
             See Table 9.1 of the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/documents/EDC_Catcher_Vessel_Report_2015.pdf", '2012 EDC report', target="_blank"), "for the full listing of 
             variable cost categories."),
        tags$li("Fixed cost:"),
           tags$p("Fixed costs do not vary with the level of fishing participation or do not vary as directly as variable costs with the level of participation during a given year. 
             The", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'EDC program', target="_blank"), "collects four types of fixed costs: 
             1) New and used vessel and on-board equipment (includes all electronics, safety equipment, and machinery not used to harvest fish); 
             2) Fishing gear (includes nets, doors, traps, pots, cables, and fishing machinery used for the West Coast fisheries); 
             3) Processing equipment (includes any equipment used to process or head and gut fish on board the vessel); and
             4) Other fixed costs such as moorage, insurance, and lease permits."),
        tags$li("Variable cost net revenue (VCNR):"),
           tags$p("VCNR is revenue minus variable costs (costs that vary with the level of participation, such as crew, fuel, buyback fees, observers, and other variable costs). VCNR is a measure of the operating profit of the average vessel."
             ),
        tags$li("Total cost net revenue (TCNR):"),
          tags$p("TCNR is revenue minus variable and fixed costs (costs that do not vary with the level of participation, such as equipment and gear purchases). TCNR is a longer-term measure of profitability. In any given year, a vessel may have a large fixed cost expense (such as a new engine which may lead to a negative or unusually low TCNR)."),
        tags$p(strong('A note about net revenue:'),  'The', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC forms', target="_blank"), 'attempt to capture only costs that are directly related to vessel fishing operations, and do not include other expenses 
            such as vehicles or office costs that may be related to the fishing business. Therefore, the net revenue reported here is an overestimate of the true net revenue.')
  ),
  tags$br(),
h4("Fished in AK"),
  tags$ul(
         tags$p("Vessels that fished in Alaska (versus vessels that fished solely off the West Coast) can be filtered from the data. Vessels that fished in Alaska tend to be longer (>90') than
              vessels that only fished off the West Coast (65' on average). Vessels that fished in Alaska also tend to participate in different 
            fisheries than vessels that fished solely off the West Coast."), 
      tags$p("If this box", tags$strong('is not selected,'), "then only vessels that participated solely in the West Coast fishery will be included. If this box", tags$strong('is selected,'), 
              "then all vessels, including those that participated in Alaskan fisheries, will be included.", tags$strong("Data from their activity in Alaskan fisheries are never included."),
             "Please note that there are some cases where there are not enough observations of vessels that either 1) fished solely in the West Coast fisheries or
      2) also fished in Alaska. When this occurs, we show results for both groups combined, regardless of whether or not you selected the", tags$em("Include vessels that fished in AK"), "button.")
  ), tags$br(),
  h4("Variables in the Data Table"),
 tags$ul(
   tags$li('Year: Calendar year over which fishing activities occurred.'),
   tags$li('Summary variables:  Summary variables (fisheries, homeport, state, vessel length class) are used to group vessels to better understand the economic performance of particular segments of the fleet. See above for a description of the summary 
          variables.'),
   tags$li('Economic measure: See above for a description of the economic measures.'),
   tags$li('N: The number of observations for each displayed statistic.'),
   tags$li('Statistic: See above for further description of the statistics.'),
   tags$li('Value: Value of the economic measures for the statistic.'),
   tags$li('FishAK: Fished in Alaskan fisheries (Vessels included or Vessels not included).'),
   tags$li('Fisheries Category: This variable is relevant for when vessels are grouped by homeport, state, or vessel length class. It indicates whether the reported value of the selected economic 
           measure includes activity in all fisheries or only in the catch share fisheries or the non-catch share fisheries.'),
   tags$li('Variance: A measure of the spread of values. We report the standard deviation when vessel averages are selected and the median absolute deviation when vessel medians are selected.'),
   tags$li('Thirds: This variable is only relevant for the', tags$em('Fleetwide Variability Analysis.'), 'Vessels are grouped into three tiered categories based on revenue: top, middle, and lower revenue earners. This is done for each year separately.'),
   tags$p(tags$br(),
          tags$br(),
          tags$br(),
          tags$br())
  )
))

