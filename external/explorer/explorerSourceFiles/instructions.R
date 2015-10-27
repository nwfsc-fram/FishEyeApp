tags$div(style = "margin: 15px; 15px; 30px; width: 60%",
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
                     <h3>Instructions</h3></div>"),
         #tags$h3('Instructions'), 
          tags$p('To use Net Revenue Explorer, make data selections in each of the fields in the panel on the right. 
          The panel to make data selections will only appear when you are on the',tags$em('Explore the data'), 'page.
          Output will be automatically generated when each of the fields in the panel has at least one selection.', tags$br(), 
          'A button to download the plot(s) and data table can be found at the bottom of the panel.'),
          tags$p('The ', 
          tags$a(href = "http://www.westcoast.fisheries.noaa.gov/fisheries/groundfish_catch_shares/index.html", "West Coast Groundfish Trawl Catch Share Program ", target="_blank"),
              'was implemented in 2011.  In all plots, we distinguish between pre- and post-implementation of the Catch Share program with shading.'),
           tags$ul(style="margin-top:15px;" ,
                   
           tags$li(tags$h4("Visualize Data with Plots")),
           tags$p('Visualize', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/overview.cfm",'Economic Data Collection (EDC)', target="_blank"), 'summary statistics for revenue, costs and net revenue of catcher vessels that participate in the',
                  tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/catch_shares.cfm", 'Catch Share program. ', target="_blank"),tags$br(),
                
                'Plotting options can be found at the bottom of the panel on the right.', tags$br(), 
                'There are three plot options: 1) economic measures side-by-side, 2) composition of total cost net revenue, and 3) composition of variable cost net revenue. 
                The', tags$em('Economic measures side-by-side'), 'option is useful for comparing economic measures (revenue, costs, net revenue) across years or fisheries. 
                Click the', tags$em('Economic measures side-by-side'), 'option under', tags$em('Plot Options'), 'field in the panel on the right. A drop down menu below the', tags$em('Plot Options '),  'allows users to change between bar, point, and line plots.', tags$br(),
                'The two composition of net revenue plots show costs relative to revenue and the difference between revenue and costs (net revenue). You can examine either Variable Cost Net Revenue (VCNR) or Total Cost Net Revenue (TCNR). 
                VCRN is revenue minus variable costs and TCNR is revenue minus variable and fixed costs. These plots are stacked bar plots. 
                Click the', tags$em('Composition of total cost net revenue'), 'or the', tags$em('Composition of variable cost net revenue'), 
                'choice under', tags$em('Plot Options'), 'in the panel on the right to see these plots.', tags$br(),
                'A figure demonstrating how VCNR and TCNR are derived is located on the', tags$em('Explore the data'), 'page under the', tags$em('Visualize Data with Plots'), 'tab.'),
                tags$p(strong('A note about net revenue:'),  'The', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC forms', target="_blank"), 'attempt to capture only costs that are directly related to vessel fishing operations, and do not include other expenses 
            such as vehicles or office costs that may be related to the fishing business. Therefore, the net revenue reported here is an overestimate of the true net revenue.'),
              tags$br(),
              tags$li(tags$h4("Data Table")),
              tags$p('View data used to generate the plots created in the', tags$em('Visualize Data'), 'tab.', tags$br(),
                  'After a table has been displayed, the data can be further filtered using the', tags$em('Search'), 'box, or filtered within a column using the boxes at the bottom of the table. 
                  Data used in the', tags$em('Variability Analysis'), 'tab are not available at this time.'),
              tags$br(),
              tags$li(tags$h4("Variability Analysis")),
              tags$p('Catcher vessels that participate in the Catch Share program span a very broad range in term of the scale of their operations. For instance, the revenue earned ranges from around $10,000 to well over $1,500,000.  
                  The purpose of the', tags$em('Variability Analysis'), 'is to show how this heterogeneity relates to their economic performance. We cannot show the economic performance for each individual vessel due to',
                     tags$a(href = "http://www.nwfsc.noaa.gov/research/divisions/fram/documents/Administration_Operations_Report_2014.pdf",'confidentiality rules', target="_blank"), 
                  'so we group the vessels into three tiered categories: top revenue earners, middle revenue earners, and bottom revenue earners.
                      We then calculate the average of the selected statistic (per vessel, vessel/day, or vessel/metric-ton) for vessels within each tiered category.', tags$br(), 
                  'Results are shown if there are at least three vessels in each group. Results are plotted as a dot plot if a single year is selected or a line plot if multiple years are selected.', tags$br(),

                  tags$strong("Note:"), 'Only a single class of the selected summary variable (fisheries, homeport, state, vessel length class) may be selected at a time. The', tags$em('Summed over all vessels'), 'statistic option is not currently supported.')
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