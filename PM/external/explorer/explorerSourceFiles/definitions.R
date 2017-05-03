# Text for definitions page

tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
              <h3>Definitions</h3></div>",
              '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Definitions <br> in new browser tab</a>'
         ),
                 tags$div(
           p("The variables and their definitions used in 
             this application are from the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/documents/EDC_Catcher_Vessel_Report_2015.pdf", '2012 Economic Data Collection (EDC) report. ', target="_blank")#, 
                       ),  
           tags$div(style= "margin-top: 15px;",
                    
                    h4("Compare groups of vessels/processors or metrics"),
                    tags$ul(p('Select whether to compare groups of vessels or processors based on selected summary variables (see below) or by metrics. For the mothership and catcher-processor sectors, the only summary variable available is At-sea Pacific whiting.')#,
                             #p('When comparing metrics, selected metrics may not all be displayed. This is because the displayed metrics depend upon the statistic selected. For some metrics, it is not appropriate to calculate all statistics. 
                             #  For instance, the Gini coefficient and Herfindahl-Hirschman Index are indices. We cannot calculate an annual average or median value. 
                              # ')
                             ),
                    
                    h4("Summary Variables"),
                    p('Please note that availability of summary variables will vary between sectors.'),
                    tags$ul(
                      tags$li(strong("Fisheries:"),'The Performance Metrics application uses the same fisheries definitions as those used by the', 
                              tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", target="_blank", 'EDC program'), 'to characterize the', 
                              tags$a(href="http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish/", 'US West Coast Groundfish fishery. ', target="_blank"), 
                              ' Many vessels that participate in the catch share program also participate in other fisheries such as Dungeness crab or Alaskan fisheries. 
                              Please note that data from Alaskan fisheries are included in the combined non-catch share fisheries category when viewing results for homeport, state,
                              and vessel length class. When viewing fisheries,activities from Alaskan fisheries are included in the ', tags$em('All non-catch share fisheries combined'), ' category. Sample size is insufficient to show results for this fishery alone.'),
                      p(tags$i('Catch Share fisheries:'),' Pacific whiting (includes at-sea and shoreside Pacific whiting); At-sea Pacific whiting; 
                        Shoreside Pacific whiting;
                        Groundfish fixed gear with trawl endorsement;
                        Groundfish with trawl gear (includes groundfish DTS with trawl endorsement, non-whiting, non-DTS trawl with trawl endorsement, and non-whiting midwater trawl);
                        DTS (Dover, Thornyhead, and Sablefish) trawl with trawl endorsement;
                        Non-whiting, Non-DTS trawl with trawl endorsement (e.g., petrale sole, rockfish, and other flatfish),
                        Non-whiting mid-water trawl (uses midwater trawl gear to target pelagic rockfish such as widow rockfish and yellowtail rockfish).'),
                      p(tags$i('Non-catch Share fisheries:'), 
                        'Groundfish fixed gear with fixed gear endorsement;
                        Crab; Shrimp; Other fisheries (e.g., tuna, salmon, and halibut), Alaskan fisheries (not shown separately).'),
                      tags$li(strong("Production activities:"),'For shorebased processors, information on the EDC form  is collected at the species level (e.g. fish production information), not the fishery level like the catcher vessels.'),
                      p(tags$i('Pacific whiting:'),' Pacific whiting.'),
                      p(tags$i('Non-whiting groundfish:'),' Arrowtooth flounder; Dover sole; English sole; Lingcod; Pacific sanddab; Petrale sole; Rex sole; Rockfish; Sablefish (black cod); Thornyheads; Sharks, skates, and rays.'),
                      p(tags$i('Other species:'),' Coastal pelagics (including sardines and mackerel); Crab; Enchinoderms (including sea urchins and sea cucumbers);
                       California halibut; Pacific halibut; Pacific herring; Salmon; Squid; Sturgeon; Tuna; Other shellfish; Other species.'),
                      tags$li(strong("Homeport:"), "The", tags$a(href="map.pdf", "homeport", target="_blank"), "reported by each vessel on the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", "EDC survey form", target="_blank"), "aggregated to port groups.
                               Vessels that fished of the West coast but report their homeport to be in Alaska are included. These vessels are included in the Puget Sound port."), 
                      tags$li(strong("State of homeport:"), "The state corresponding to each homeport. Vessels that fished off the West coast but report their homeport to be in Alaska are included. These vessels are included in Washington State."
                              ),
                      tags$li(strong("Region:"), "The region corresponding to each processor. For first receivers and shorebased processors, two regions are recognized:", tags$em('Washington and Oregon'), 'and', tags$em("California.")
                      ),
                      tags$li(strong("Vessel length class:"),"Three categories of vessel length representing the range of catcher vessel length: large vessels (> 80 ft), medium vessels (> 60 ft, <= 80 ft), and small vessels (< 60 ft)."),
                      tags$li(strong("Processor size:"),"First receivers and shorebased processors are grouped into three size classes based on the average number of workers: large (> 200 workers), medium (100 - 200 workers), and small (< 100 workers).
                              Processor size was determined by examining the maximum weekly number of production workers employed by a processor (as reported on the EDC form) for each year. Then the
                              weekly maximum was averaged across all EDC years to place processors in one size category for all years.")
                       ), tags$br(),             
                     
                    h4('Indicators'),
                    tags$ul(
                      tags$li(strong('Demographic:'), 'Characteristics of the vessels in the fleet that may vary over time. Changes in the demographic metrics may indicate changes such as fleet consolidation, efficiency, productivity, specialization, or diversification.'),
                      tags$li(strong('Economic:'),'Metrics within the economic indicators section describe the potenital economic impacts of the catch share fishery on the West Coast. Changes may be indicative of changes in total allowable catch (TAC), fish or product prices, costs or cost structure, profitability, productivity, and diversification.'),
                      tags$li(strong('Social and Regional:'),'Distribution of benefits and costs among individuals, groups, and communities. Indicators describe potential impact on individuals (i.e., wages) and communities (i.e. share of landings by state).')
                    ),
                    h4('Metrics'),
                    p('Please note that availability of metrics will vary between sectors.'),
                    
                    tags$ul(h4(tags$em('Demographic')),
                            tags$li(strong('Days at sea: '),
                                    'The number of days at sea may indicate specialization, efficiency, or consolidation.'),
                            tags$li(strong('Exponential Shannon Index: '),
                                    'Measures the income diversification of a vessel across revenue sources. A larger number corresponds to increased diversification. Changes may indicate specialization or diversification.'),
                            tags$li(strong('Fishery participation: '),
                                    'Count of fisheries (defined above) that vessels participated in. Changes may indicate specialization or diversification.'),
                            tags$li(strong('Gini coefficient: '),
                                    'Measures the degree of catch share revenue concentration among vessels. A value of zero would represent all vessels earning the same revenue, and a value of one would represent one vessel earning all of the revenue. The value of the Gini coefficient can be affected by fleet consolidation and specialization.'),
                            tags$li(strong('Number of species processed: '),
                                    'Count of the number of species processed. Changes may indicate specialization or diversification.'),
                            tags$li(strong('Number of vessels or processors: '),
                                    'Number of vessels actively participating (i.e., had an active permit and non-zero revenue).'),
                            tags$li(strong('Proportion of revenue from catch share fishery: '),
                                    "The proportion of a vessels total revenue that comes from fish caught in the limited entry/catch share fishery. Metric measures how reliant vessels are on revenue from the limited entry/catch shares fishery."),
                            tags$li(strong('Vessel length: '),
                                    'The length (ft) of vessels reflects the mix of active participants in the fishery in each year.')
                                               ),
                    tags$br(),
                    
                    tags$ul(h4(tags$em("Economic")),tags$em("Values reported in inflation adjusted 2014 dollars."),
                    
                      tags$li(strong("Revenue:"),    
                              "Revenue is the total revenue generated by participation in the selected fisheries. This metric captures changes in total harvest and ex-vessel prices."
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
                    
                    tags$ul(h4(tags$em('Social and Regional')),
                            tags$li(strong('Crew wage per day: '),'Daily wage paid to a crewmember operating in the limited entry/catch shares fishery.'),
                            tags$li(strong('Hourly compensation:'), 'Hourly compensation paid to a processing worker in the West Coast groundfish fishery.'),
                            tags$li(strong('Number of positions or workers: '),'Number of positions (including captain and crew or workers for the First Receivers and Shorebased Processors sector) is a lower bound for employment in the fishery. The metric is affected by positions per vessel and the number of vessels fishing.'),
                            tags$li(strong('Revenue per crew-day: '),'Revenue divided by crew day, where crew days are calculated as days at sea multipled by number of crew per vessel. This metric is a measure of productivity (in terms of revenue generation) of a crew member.'),
                            tags$li(strong('Seasonality: '),'The date (day of year, Jan. 1 = 1) on which 50% of the total volume of catch was landed in the fishery. Metric measures broad-scale changes in the seasonality of fishing for catch shares fish. It can also indicate changes in total allowable catch (TAC); it may take the fleet longer to catch a higher TAC/ACL.'),
                            tags$li(strong('Share of landings by state: '),'Share of landings (deliveries) by all vessels, by whiting vessels, and by non-whiting groundfish vessels in each state or at-sea. Shares are in terms of revenue. When selecting the ',tags$em('State'), 'summary variable, this metric shows the share of landings in each state for vessels that homeport in the selected state. This selection highlights that vessels may deliver fish in multiple states.')
                            ),
                    tags$p('Additional metrics that address economic input/output impacts are available in the Economic Performance Metrics for the West Coast Groundfish Trawl Catch Share Program report.  These indicators are the total impacts on income and employment of the West Coast Groundfish Trawl Catch Share fishery.
                           Total impacts include, direct, indirect, and induced employment effects, as measured by IO-PAC model.'),
                    tags$br(),
                    h4("Statistics"),
                    tags$p(strong('The statistic provided depends upon the metric selected. '),'For instance, the average or median per day or per metric-ton is only available for economic metrics.'),
                    tags$ul(
                      
                      tags$li(strong("Median or Average")),
                      tags$ul(
                        tags$li(strong("Per vessel:"),"Median or Average for all vessels that participated in the catch share program."),
                        tags$li(strong("Per vessel per day:"),
                                "Median or Average per day for all vessels that participated in the catch share program."),
                        tags$li(strong("Per vessel per metric-ton:"),
                                "Median or Average per metric ton of fish caught for all vessels that participated in the catch share program.")
                        ),tags$br(),
                      tags$li(strong("Fleet-wide total:"),"Summed over all vessels that participated in the catch share program.") 
                      ),tags$br(),
                    tags$ul(tags$p(tags$strong("What is the purpose of using median, average, and fleet-wide total values when looking at the data?"), "The median and average provide information about a representative vessel; 
                                   however, they do it in different ways.  The ",  tags$strong("median"), "means that half of the vessels have a larger result than the median, and half are smaller.  The ", tags$strong("average "),"
                                   is the sum of the values divided by the number of responses.  If the data do not have many extreme responses in a particular direction, the median and average will be very similar.  However, if the data are ", 
                                   tags$em('skewed'), "by extreme responses, then the median is a better measure for a typical vessel.  The ", tags$strong("fleet-wide total"), "measures how the entire fleet is doing, rather than a representative vessel.")
                            ),tags$br(),
                    
                    h4("Pacific whiting vessel category"),
                    tags$ul(
                      tags$p("For the catcher vessel sector, the selected metric is calculated at three levels: 1) for the fleet as a whole, 2) for vessels that fished for Pacific whiting, and 3) for vessels that participate in the non-whiting groundfish sector of the limited entry trawl fishery. 
                             Vessels that fished for both whiting and non-whiting are included in the whiting level. The purpose of separating the whiting from the non-whiting vessels is that the whiting vessels tend to be larger and catch a higher volume of fish. In addition, total allowable catch for Pacific whiting has more annual variation than total annual catch for species targeted in the non-whiting groundfish sector. 
                             Note that there are several fisheries where vessels only participated in whiting or non-whiting fisheries."),
                      tags$p('This division is not appropriate for the mothership and catcher-processor sectors. For these sectors, only pacific whiting is shown.'),
                      tags$p('There are some cases where there are not enough vessels to separate the vessels into the whiting and non-whiting vessel categories. This occurs when there is less than three vessels at either the whiting or non-whiting level. When this happens, we only report the "All vessels" category and indicate that we have done so in a message below the plot and in the data table. More information on data confidentiality requirements can be found in the EDC Administration and Operations Report.')
                      ), tags$br(),                   
                    
                    h4("Include Alaskan fisheries activities"),
                    tags$ul(
                      tags$p('For some metrics, you may choose whether or not to include activities in Alaskan fisheries. If this box', tags$strong('is selected,'),
                             'then activities in Alaskan fisheries will be included.' )
                      ), tags$br(),
                    
                   h4('Years'),
                   tags$ul('The catch share program began in 2011. A mandatory',
                           tags$a(href='https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm', 'Economic Data Collection (EDC) program ', target="_blank"),
                           'began in 2009. Results are available from 2009 onwards for metrics that require EDC data. For metrics that do not require EDC data, 
                           results are available from 2004 onwards. For these metrics, click the box to select years prior to 2009.'
                           ),tags$br(),
                    
                    h4("Variables in the Data Table"),
                    tags$ul(
                      tags$li(strong('Year:'), 'Calendar year over which fishing activities occurred.'),
                      tags$li(strong('Summary variables:'),  'Summary variables (fisheries, homeport, state, vessel length class) are used to group vessels to better understand the economic performance of particular segments of the fleet.'),
                      tags$li(strong('Metric:'), 'See above for a description of the individual metrics. This column is not included when an economic indicator is selected.'),
                      tags$li(strong('Economic measure:'), 'See the economic metric section above for a description of the economic measures.'),
                      tags$li(strong('Number of vessels/processors:'), 'The number of observations for each displayed statistic.'),
                      tags$li(strong('Statistic:'), 'See above for further description of the statistics.'),
                      tags$li(strong('Value:'), 'Value of the metric or economic measure for the selected statistic.'),
                      tags$li(strong('Alaskan fisheries:'), 'Include activities in Alaskan fisheries  (Vessels included or Vessels not included).'),
                      tags$li(strong('Data summed across:'), 'For catcher vessels, we show data summed across all vessels, vessel that participated in the non-whiting groundfish sector of the limited entry trawl fishery, vessels that fished for Pacific whiting. Vessels that fished for both whiting and non-whiting are included in the whiting level.
                                                              For first receivers and shorebased processors, we show data summed across all processors, processors that process whiting species, and processors that process non-whiting species.'),
                      tags$li(strong('Fisheries Category:'), 'This variable is relevant for when vessels are grouped by homeport, state, or vessel length class. It indicates whether the reported value of the selected metric or economic 
                                      measure includes activity in all fisheries or only in the catch share fisheries or the non-catch share fisheries.'),
                      tags$li(strong('Production Category:'), 'This variable is relevant for when processors are grouped by region or processor size. It indicates whether the reported value of the selected metric or economic 
                                      measure includes all production or only groundfish production or other species production.'),
                      tags$li(strong('Variance:'), 'A measure of the spread of values. We report the standard deviation (SD) when vessel averages are selected and the median absolute deviation (MAD) when vessel medians are selected.'),
                      tags$li(strong('Delivery location:'), 'This variable is only relevant for the', tags$em('Share of landings by state'), 'metric. This variable indicates location of landings.'),
                      tags$p(tags$br(),
                             tags$br(),
                             tags$br(),
                             tags$br())
                    )
                    ))
                    )
