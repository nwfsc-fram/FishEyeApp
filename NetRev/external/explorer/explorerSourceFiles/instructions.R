#Code in this file is for instructions page.

tags$div(style = "margin: 15px; 15px; 30px; width: 60%",
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
                     <h3>Instructions</h3></div>",
              '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Instructions <br> in new browser tab</a>'
         ),
         #tags$h3('Instructions'), 
          tags$p('To use Net Revenue Explorer, make data selections in each of the fields in the Control Panel (this panel will only appear when you are on the',tags$em('Explore the data'), 'page).
          Output will be automatically generated when each of the fields in the Control Panel has at least one selection.', tags$br(), tags$br(),
          'A button to download the plot(s) and data table can be found at the bottom of the Control Panel. Download the data and analyze externally to generate plots and analyses beyond what is 
          provided in the Net Revenue Explorer.',
          tags$br(),tags$br(),'To switch between viewing plots and data table, use the button above the plot or data output.'),
           tags$ul(style="margin-top:15px;" ,
                   
           tags$li(tags$h4("Summary Plots and Data")),
           tags$p('Visualize', tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm",'Economic Data Collection (EDC)', target="_blank"), 'summary statistics for revenue, costs, and net revenue of catcher vessels (both at-sea and shoreside), motherships, catcher processors, and first receivers and shorebased processors that participate in the',
                  tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/catch_shares.cfm", 'catch share program. ', target="_blank")),
                 
                tags$strong('Plots'),tags$p('Plotting options can be found at the bottom of the Control Panel. There are three plot options:', tags$br(),
                
                 tags$ul('1) Economic measures side-by-side,'), tags$ul('2) Composition of Total Cost Net Revenue, and'), tags$ul('3) Composition of Variable Cost Net Revenue.'), tags$br(),
                'The', tags$em('Economic measures side-by-side'), 'option is useful for comparing economic measures (revenue, costs, net revenue) across years. 
                A drop-down menu below the', tags$em('Plot Options '),  'allows you to switch between bar, point, and line plots.'), 
           
                tags$p('The composition of net revenue plots show revenue, costs, and net revenue (revenue minus costs). You can examine either Variable Cost Net Revenue (VCNR) or 
                Total Cost Net Revenue (TCNR).  VCNR is revenue minus variable costs and TCNR is revenue minus variable and fixed costs.
                These plots are stacked bar plots. A figure demonstrating how VCNR and TCNR are derived is located on the', tags$em('Explore the Data'), 'page under the', tags$em('Summary Data and Plots'), 'tab.'),
                tags$strong("Data Table"),
              tags$p('View data used to generate the plots.', tags$br(), 'After a table has been displayed, the data can be filtered using the', tags$em('Search'), 'box, or filtered within a column using the boxes at the bottom of the table.'),
              tags$br(),
           
 
                   tags$li(tags$h4("Fleet- and Industry-wide Variability Analysis")),
              tags$em('Visualize the variability within the selected sector.'),
              tags$p('Participants in the catch share program span a very broad range in terms of the scale of their operations. 
                For instance, the revenue earned ranges from around $10,000 to well over $1.5 million. To view the heterogeneity within the selected sector while taking into account confidentiality 
                     rules, we group the vessels or processors into three tiered categories annually: top, middle, and lower revenue earners.   We then calculate the median, mean, or total of the selected statistic 
                     (per vessel or processor, vessel/day, or vessel or processor/metric ton) for vessels or processors within each tiered category. 
                     This analysis is only available for the Catcher Vessel and First Receivers and Shorebased Processors sectors.', tags$br()),
                tags$strong('Plots'), tags$p('Results are shown if there are at least three vessels or processors in a group. Results are plotted as a dot plot if a single year is selected or a line plot if multiple years are selected.'),
                tags$strong('Data Table'),tags$p('View data used to generate the plots.', tags$br(),
                                                 'After a table has been displayed, the data can be further filtered using the', tags$em('Search'), 'box, or filtered within a column using the boxes at the bottom of the table.', tags$br(),tags$br(),

                  tags$strong("Note:"), 'Only a single class of the selected summary variable (fisheries, production category, homeport, state or homeport, region, vessel length class, processor size) may be selected at a time.',
              tags$br(),
              tags$br(),
                
            
              tags$br())
              
          ),
    tags$p(strong('A note about net revenue:'),  'The', tags$a(href="https://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC survey forms', target="_blank"), 
   'capture costs that are directly related to vessel fishing operations. Other expenses 
    such as vehicles or office costs that may be related to the fishing business are not included. Therefore, the net revenue reported here is an overestimate of the true net revenue.'),
  tags$br()
)



# 
# 
# tags$div(style="margin-top:15px; width: 60%",
# tags$p("To generate output select data in the Select Data Panel.")
# tags$p('Select the above tabs to view different forms of output, including
#        a summary plot, table, and the thirds analysis.')
# ),
# 
# # PLOT
# tags$h3(tags$i("plot")),
#   p("View data summaries for the selected summary variables.")
# 
# # THIRDS
# tags$h3(tags$i("Thirds anlaysis")),
#   p("View how different."),
#   p("Vessels are ordered by Revenue and reported as top third, 
#     middle third and bottom third.")