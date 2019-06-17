# Text for definitions page

tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         # HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
         #      <h3>Definitions</h3></div>",
         #      '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Definitions <br> in new browser tab</a>'
         # ),
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
                      tags$li(strong("Vessel length class:"),"Three categories of vessel length representing the range of catcher vessel length: large vessels (> 80 ft), medium vessels (> 60 ft, <= 80 ft), and small vessels (<= 60 ft)."),
                      tags$li(strong("Processor size:"),"First receivers and shorebased processors are grouped into three size classes based on the average number of workers: large (> 200 workers), medium (100 - 200 workers), and small (< 100 workers).
                              Processor size was determined by examining the maximum weekly number of production workers employed by a processor (as reported on the EDC form) for each year. Then the
                              weekly maximum was averaged across all EDC years to place processors in one size category for all years.")
                       ), tags$br(),             
                     
                    h4('Indicators'),
                    tags$ul(
                      tags$li(strong('Vessel or processor characteristics:'), 'Characteristics of the vessels in the fleet or the processors that process groundfish. The statistic may change as a result of either a change made to the vessels or processors and/or by changes in which vessels/processors participate. Metrics in this category provide information about fleet consolidation, efficiency, productivity, specialization, or diversification.'),
                      tags$li(strong('Economic:'),'Metrics within the economic indicators section describe the potential economic impacts of the catch share fishery on the West Coast. Changes may be indicative of changes in total allowable catch (TAC), fish or product prices, costs or cost structure, profitability, productivity, and diversification.'),
                      tags$li(strong('Labor:'),'Distribution of benefits and costs among crewmembers. Indicators describe potential impact on individuals (i.e., wages) and employment (i.e. number of positions).'),
                      tags$li(strong('Cost:'),'Variable and fixed costs presented by sub-category. Please note that the availability of cost measures will vary by sector.'),
                      tags$li(strong('Impacts:'), 'Income and employment impacts for the West Coast overall.'),
                      tags$li(strong('Other:'),'Broad collection of metrics that extend beyond the other types of indicators and describe effort, productivity, inequality, and the temporal and spatial distribution of landings.')
                    ),
                    h4('Metrics'),
                    p('Please note that availability of metrics will vary between sectors.'),
                    #Erin- Review definitions for vessel replacement value, vessel market value and vessel horsepower.
                    tags$ul(h4(tags$em('Vessel or processor characteristics')),
                            tags$li(strong('Number of vessels or processors: '),
                                    'Number of vessels fishing or number of processing companies processing.'),
                            tags$li(strong('Vessel length: '),
                                    'Length of vessels in feet.'),
                            tags$li(strong('Vessel replacement value: '),
                                    'Estimate of what it would cost to replace the current vessel with a new vessel based on the most recent marine survey for the vessel.'),
                            tags$li(strong('Vessel market value: '),
                                   'Estimate of what the vessel could be sold for in its current condition based on the most recent marine survey for the vessel.'),
                            tags$li(strong('Vessel horsepower: '),
                                    'Horsepower of main engines.'),
                            tags$li(strong('Number of fisheries: '),
                                    'Count of fisheries (defined above) that vessels participated in. Changes may indicate specialization or diversification.'),
                            tags$li(strong('Number of species purchased: '),
                                    'Count of the number of species purchased. Changes may indicate specialization or diversification.'),
  HTML('<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Species </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="1"><td colspan="1" style="border-bottom: 1px solid;"><strong>Pacific whiting</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Pacific whiting </td>
  </tr>
  <tr grouplength="1"><td colspan="1" style="border-bottom: 1px solid;"><strong>Non-whiting groundfish</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Arrowtooth flounder, Dover sole, Lingcod, English sole, Petrale sole, Rockfish, Sablefish, Rex sole, Sharks, skates and rays, Sanddab, Thornyheads </td>
  </tr>
  <tr grouplength="1"><td colspan="1" style="border-bottom: 1px solid;"><strong>Other</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> California halibut, Coastal pelagics, Crab, Echinoderms, Nonspecies specific product, Other species, Pacific halibut, Pacific herring, Squid, Salmon, Other shellfish, Tuna, Shrimp, Sturgeon </td>
  </tr>
</tbody>
</table>'),
                            tags$li(strong('Proportion of revenue from catch share fishery: '),
                                    "The proportion of a vessels total revenue that comes from fish caught in the limited entry/catch share fishery. Metric measures how reliant vessels 
                                    are on revenue from the limited entry/catch shares fishery."),
                            tags$li(strong('Revenue diversification: '),
                                    'Measures the income diversification of a vessel across revenue sources calculated using Exponential Shannon Index. A larger number corresponds to increased diversification. Changes may indicate specialization or diversification.')
                                               ),
                    tags$br(),
                    
                    tags$ul(h4(tags$em("Economic")),tags$em("Values reported in inflation adjusted", currentyear, "dollars."),
                    
                      tags$li(strong("Revenue:"),    
                              "Revenue is the total revenue generated by participation in the selected fisheries. This metric captures changes in total harvest and ex-vessel prices."
                      ),      
                      tags$li(strong("Variable cost:"),
                              "Vary with the level of participation in a fishery and generally include items such as fuel and crew payments.", 
                              tags$ul("See Table 9.1 of the", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/documents/EDC_Catcher_Vessel_Report_2015.pdf", 
                                      '2012 EDC report', target="_blank"), "for the full listing of variable cost categories.")),
                      tags$li(strong("Fixed cost:"),
                              "Do not vary with the level of fishing participation or do not vary as directly as variable costs with the level of participation during a given year. 
                              The", tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm", 'EDC program', target="_blank"), 
                              "collects four types of fixed costs:",
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
                    #Erin- Review definitions for number of crew-days, annual crew wage, and wages per dollar revenue
                    tags$ul(h4(tags$em('Labor')),
                            tags$li(strong('Number of crew: '),
                                    'Number of crew is a lower bound for employment in the fishery. For catcher-processors and motherships this value includes processing and non-processing crew. The metric is affected by positions per vessel and the number of vessels fishing.'),
                            tags$li(strong('Number of workers: '),'Number of positions (including workers for the First Receivers and Shorebased Processors sector) is a lower bound for employment in the processing sector.'),
                            tags$li(strong('Number of crew-days: '),
                                    'Measure of effort calculated by the number of days at sea multiplied by the number of crew.'),
                            tags$li(strong('Annual crew wage: '),
                                    'Annual crew wage per crewmember.'),
                            tags$li(strong('Crew wage per day: '),
                                    'Daily wage paid to a crewmember operating in the limited entry/catch shares fishery.'),
                            tags$li(strong('Hourly compensation:'), 
                                    'Hourly compensation paid to a processing worker in the West Coast groundfish fishery.'),
                            tags$li(strong('Wages per dollar revenue: '),
                                    'Total cost of crew wages per dollar of ex-vessel revenue.'),
                            tags$li(strong('Revenue per crew-day: '),
                                    'Revenue divided by crew-day, where crew-days are calculated as days at sea multipled by number of crew per vessel. This metric is a measure of productivity (in terms of revenue generation) of crew.')
                            ),
                            #tags$li(strong('Revenue per crew-day: '),'Revenue divided by crew-days, where crew-days are calculated as days at sea multipled by number of crew per vessel. This metric is a measure of productivity (in terms of revenue generation) of a crew member.'),        
                    tags$br(),
                    tags$ul(h4(tags$em('Cost')),
                            tags$li('Quota earnings and costs are currently excluded due to data limitations. 
                      It is not possible to convert quota data to the calendar year format like the rest of the cost categories. 
                              Additionally, many quota trades are non-cash transactions and therefore cannot be included in the calculations.'),
                            tags$li(strong("A note about total costs."),' There are a variety of costs that are associated with operating a vessel or processing facility that are not requested on the form because it is difficult to determine the 
        share of the cost associated with the vessel or facility. These costs include items that can be used for activities other than fishing, or are too difficult to allocate to a particular 
        vessel or facility in a multi-vessel company. These expenses include office space, vehicles, storage of equipment, professional fees, and marketing. In general, the EDC form aims to capture 
        costs that are directly related to vessel or facility maintenance and fishing or processing operations, and not costs that are related to activities or equipment off the vessel or beyond 
        the facility. For these reasons, the aggregated measures of costs presented here underestimate the true costs of operating a business.')
                    ),
                    tags$br(),
                    
                    tags$ul(h4(tags$em('Impacts')),
                            tags$li('Please refer to this document for more information about impacts: https://www.nwfsc.noaa.gov/assets/25/1620_08012011_142237_InputOutputModelTM111WebFinal.pdf'),
                            tags$li(strong('Income impacts: '),
                                    'West Coast-wide income generated by the selected vessels from activities in the selected fishery.'),
                            tags$li(strong('Employment impacts: '),
                                    'Number of jobs generated by the selected vessels from activities in the selected fishery on the West Coast overall.')
                    ),
                    tags$br(),
                            
                    tags$ul(h4(tags$em('Other')),
                            tags$li(strong('Days at sea: '),
                                    'The number of days at sea may indicate specialization, efficiency, or consolidation.'),
                            tags$li(strong('Fuel use per day: '), 
                                    'Fuel use per day, alongside days at sea and speed while fishing, provides information about effort per day and fishing behavior.'),
                            tags$li(strong('Speed while fishing: '), 
                                    "Speed while fishing, alongside days at sea and fuel use per day, provides information about effort per day and fishing behavior."),
                            tags$li(strong('Gini coefficient: '),
                                    'Measures the degree of catch share revenue concentration among vessels. A value of zero would represent all vessels earning the same revenue, and a value of one would represent one vessel earning all of the revenue. The value of the Gini coefficient can be affected by fleet consolidation and specialization.'),
                            tags$li(strong('Share of landings by state: '),
                                    'Share of landings (deliveries) by all vessels, by whiting vessels, and by non-whiting groundfish vessels in each state or at-sea. Shares are in terms of revenue. When selecting the ',tags$em('State'), 'summary variable, this metric shows the share of landings in each state for vessels that homeport in the selected state. This selection highlights that vessels may deliver fish in multiple states.'),
                            tags$li(strong('Seasonality: '),
                                    'The date on which 50% of the total volume of catch was landed in the fishery. Metric measures broad-scale changes in the seasonality of fishing for catch shares fish. There are many potential drivers, including changes in total allowable catch (TAC), where take the fleet longer to catch a higher TAC/ACL.')
                            ),
                          
                    tags$br(),
                            
                    h4("Statistics"),
                    tags$p(strong('The statistic provided depends upon the metric selected. '),'For instance, the mean or median per day or per metric ton is only available for economic metrics.'),
                    tags$ul(
                      
                      tags$li(strong("Median or Mean")),
                      tags$ul(
                        tags$li(strong("Per vessel or processor:"),"Median or Mean for all vessels or processors that participated in the catch share program."),
                        tags$li(strong("Per vessel or processor per day:"),
                                "Median or Mean per day for all vessels or processors that participated in the catch share program."),
                        tags$li(strong("Per vessel or processors per metric ton:"),
                                "Median or Mean per metric ton of fish caught or processed for all vessels or processors that participated in the catch share program.")
                        ),tags$br(),
                      tags$li(strong("Fleet-wide total:"),"Summed over all vessels or processors that participated in the catch share program. For rates measurements (per day at sea or metric ton), we show the fleet-wide or industry-wide average.
                              The fleet-wide average is the economic value summed over all vessels or processors divided by days at sea or metric tons summed over all vessels or processors.") 
                      ),tags$br(),
                    tags$ul(tags$p(tags$strong("What is the purpose of using median, mean, and fleet-wide total values when looking at the data?"), "The median and mean provide information about a representative vessel; 
                                   however, they do it in different ways.  The ",  tags$strong("median"), "means that half of the vessels have a larger result than the median, and half are smaller.  The ", tags$strong("mean "),"
                                   is the sum of the values divided by the number of responses.  If the data do not have many extreme responses in a particular direction, the median and mean will be very similar.  However, if the data are ", 
                                   tags$em('skewed'), "by extreme responses, then the median is a better measure for a typical vessel.  The ", tags$strong("fleet-wide total"), "measures how the entire fleet is doing, rather than a representative vessel.")
                            ),tags$br(),
                    
                    h4("Pacific whiting vessel category"),
                    tags$ul(
                      tags$p("For the catcher vessel sector, the selected metric is calculated at three levels: 1) for the fleet as a whole, 2) for vessels that fished for Pacific whiting, and 3) for vessels that participate in the non-whiting groundfish sector of the limited entry trawl fishery. 
                             Vessels that fished for both whiting and non-whiting are included in the whiting level."),
                             tags$img(src='WhitingFigure.png', height=250),
                              tags$p("The purpose of separating the whiting from the non-whiting vessels is that the whiting vessels tend to be larger and catch a higher volume of fish. In addition, total allowable catch for Pacific whiting has more annual variation than total annual catch for species targeted in the non-whiting groundfish sector. 
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
                      tags$li(strong('Variance:'), 'A measure of the spread of values. We report the standard deviation (SD) when vessel/processor mean are selected and upper and lower quartiles (25th/75th percentiles) when vessel/processor medians are selected.'),
                      tags$li(strong('Delivery location:'), 'This variable is only relevant for the', tags$em('Share of landings by state'), 'metric. This variable indicates location of landings.'),
                      tags$p(tags$br(),
                             tags$br(),
                             tags$br(),
                             tags$br())
                    )
                    ))
                    )

# code for list of species/species groups
# mutate(dbGetQuery(framdw, "select * from EDC_FR_REVENUE where fullcode not like 'RVOTHR%'"), Grouping = factor(FISHERY_GROUP, levels = c('Pacific whiting', 'Non-whiting groundfish', 'Other'))) %>%
#     rename(`Species group` = EDCSPID) %>%
#     select(-FISHERY_GROUP) %>% 
#     subset(!is.na(`Species group`) & !grepl('Bycatch', `Species group`)) %>%
# distinct(Grouping, `Species group`) %>% arrange(Grouping) %>% 
#   group_by(Grouping) %>%
#   summarise(Species = toString(`Species group`)) %>%
#   select(Species) %>%
#   kable(row.names = F) %>%
#   kable_styling("striped", full_width = F) %>%
#   group_rows("Pacific whiting", 1, 1) %>%
#   group_rows("Non-whiting groundfish", 2, 2) %>%
#   group_rows("Other", 3, 3)

