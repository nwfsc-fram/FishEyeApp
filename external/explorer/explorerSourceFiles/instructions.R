tags$div(style = "margin: 15px; 15px; 30px; width: 60%",
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
                     <h3>Instructions</h3></div>",
              '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Instructions <br> in new browser tab</a>'
         ),
         #tags$h3('Instructions'), 
          tags$p('To use Net Revenue Explorer, make data selections in each of the fields in the panel on the right 
          (this panel will only appear when you are on the',tags$em('Explore the data'), 'page).
          Output will be automatically generated when each of the fields in the panel has at least one selection.', tags$br(), 
          'A button to download the plot(s) and data table can be found at the bottom of the panel. To generate plots and analyses beyond what is provided in the Net Revenue Explorer, please download the data and analyze externally.'),
          tags$p('The ', 
          tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", "West Coast Groundfish Trawl Catch Share Program ", target="_blank"),
              'was implemented in 2011.  In all plots, we distinguish between pre- and post-implementation of the Catch Share program with shading.'),
           tags$ul(style="margin-top:15px;" ,
                   
           tags$li(tags$h4("Visualize Data with Plots")),
           tags$p('Visualize', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm",'Economic Data Collection (EDC)', target="_blank"), 'summary statistics for revenue, costs, and net revenue of catcher vessels (both at-sea and shoreside) that participate in the',
                  tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/catch_shares.cfm", 'Catch Share program. ', target="_blank"),tags$br(),tags$br(),
                
                'Plotting options can be found at the bottom of the panel on the right (panel only shown when on the', tags$em('Explore the Data'), 'page).
                There are three plot options: 1) economic measures side-by-side, 2) composition of total cost net revenue, and 3) composition of variable cost net revenue. 
                The', tags$em('Economic measures side-by-side'), 'option is useful for comparing economic measures (revenue, costs, net revenue) across years for different summary statistics. 
                Click the', tags$em('Economic measures side-by-side'), 'option under',  tags$em('Plot Options'), 'in the right-hand panel that will appear when you are on the',
               tags$em('Explore the Data'), 'page. A drop-down menu below the', tags$em('Plot Options '),  'allows you to switch between bar, point, and line plots.'), 
                tags$p('The two composition of net revenue plots show revenue, costs, and net revenue (revenue minus costs). You can examine either Variable Cost Net Revenue (VCNR) or Total Cost Net Revenue (TCNR). 
                VCNR is revenue minus variable costs and TCNR is revenue minus variable and fixed costs. These plots are stacked bar plots. 
                To see these plots, click the', tags$em('Composition of total cost net revenue'), 'or the', tags$em('Composition of variable cost net revenue'), 
                'choice under', tags$em('Plot Options'), 'in the right-hand panel that will appear when you are on the', tags$em('Explore the Data'), 'page.
                A figure demonstrating how VCNR and TCNR are derived is located on the', tags$em('Explore the Data'), 'page under the', tags$em('Visualize Data with Plots'), 'tab.'),
                tags$p(strong('A note about net revenue:'),  'The', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC survey forms', target="_blank"), 
            'attempt to capture only costs that are directly related to vessel fishing operations. Other expenses 
            such as vehicles or office costs that may be related to the fishing business are not included. Therefore, the net revenue reported here is an overestimate of the true net revenue.'),
              tags$br(),
     
                    tags$li(tags$h4("Data Table")),
              tags$p('View data used to generate the plots created in the', tags$em('Visualize Data with Plots'), 'tab and', tags$em('Fleetwide Variability Analysis'), 'tab.', tags$br(),
                  'We provide a measure of the variance around the average and median values. For average values, we report the standard deviation. For median values, we report the median absolute deviation.', tags$br(),
              'After a table has been displayed, the data can be further filtered using the', tags$em('Search'), 'box, or filtered within a column using the boxes at the bottom of the table.'),
              tags$br(),
      
                   tags$li(tags$h4("Fleetwide Variability Analysis")),
              tags$em('Visualize the variability within the catcher vessel fleet.'),
              tags$p('Catcher vessels that participate in the Catch Share program span a very broad range in term of the scale of their operations. For instance, the revenue earned ranges from around $10,000 to well over $1,500,000.  
                  The purpose of the', tags$em('Fleetwide Variability Analysis'), 'is to show how this heterogeneity relates to their economic performance. We cannot show the economic performance for each individual vessel due to confidentiality rules
                  so we group the vessels into three tiered categories: top, middle, and lower revenue earners.  
                      We then calculate the median or average of the selected statistic (per vessel, vessel/day, or vessel/metric-ton) for vessels within each tiered category. This is done for each year separately.', tags$br(),tags$br(), 
                  'Results are shown if there are at least three vessels in a group. Results are plotted as a dot plot if a single year is selected or a line plot if multiple years are selected.', tags$br(),tags$br(),

                  tags$strong("Note:"), 'Only a single class of the selected summary variable (fisheries, homeport, state, vessel length class) may be selected at a time.',
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br())
              
          )

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