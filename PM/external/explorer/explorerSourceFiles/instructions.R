#Code in this file is for instructions page.

tags$div(style = "margin: 15px; 15px; 30px; width: 60%",
         HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
              <h3>Instructions</h3></div>",
              '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Instructions <br> in new browser tab</a>'
         ),
         #tags$h3('Instructions'), 
         tags$p('To use the Performance Metrics application, make data selections in each of the fields in the Control Panel (this panel will only appear when you are on the',tags$em('Explore the data'), 'page).
                Output will be automatically generated when each of the fields in the Control Panel has at least one selection.', tags$br(), tags$br(),
                'A button to download the plot(s) and data table can be found at the bottom of the Control Panel.',
                tags$br(),tags$br(),'To switch between viewing plots and data table, use the button above the plot or data output.'),
         tags$ul(style="margin-top:15px;" ,
                 
                 tags$li(tags$h4("Summary Plots and Data")),
                 tags$p('Visualize performance metrics for vessels that participate in the',tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/catch_shares.cfm", 'Catch Share program. ', target="_blank")),
                 
                 tags$strong('Plots'),tags$p('To aid visualizing changes over time, we show line plots for all selected metrics. You can select whether or not to show the variance around the average or median values. 
                                             A figure demonstrating how to interpret the plot output is located on the', tags$em('Explore the Data'), 'page. 
                                             Percent change is calculated based on the years selected and is the percent change between pre- and post-implementation of the Catch Share program. The Catch Share program began in 2011.'),
                 tags$strong("Data Table"),
                 tags$p('View data used to generate the plots.', tags$br(), 'After a table has been displayed, the data can be filtered using the', tags$em('Search'), 'box, or filtered within a column using the boxes at the bottom of the table.'),
                 tags$br()
              
          ),
    tags$p(strong('A note about net revenue:'),  'The', tags$a(href="http://www.nwfsc.noaa.gov/research/divisions/fram/economic/economic_data_forms.cfm", 'EDC survey forms', target="_blank"), 
   'capture costs that are directly related to vessel fishing operations. Other expenses such as vehicles or office costs that may be related to the fishing business are not included. 
   Therefore, the net revenue reported here is an overestimate of the true net revenue.'),
  tags$br()
)

