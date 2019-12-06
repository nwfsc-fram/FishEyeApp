# Text for definitions page

tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         # HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
         #      <h3>Definitions</h3></div>",
         #      '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Definitions <br> in new browser tab</a>'
         # ),
                 tags$div(
                   h3("Statistics"),
                   p('The statistic provided depends upon the metric selected. For instance, the mean or median per day or per metric ton is 
                     only available for Economic and Cost metrics.'),
                   tags$br(),
                   h4("Median or mean"),
                    p('The median and mean provide information about a representative vessel; however, they do it in different ways. The 
                      median means that half of the vessels have a larger result than the median, and half are smaller. The mean is the 
                      sum of the values divided by the number of responses. If the data do not have many extreme responses in a particular 
                      direction, the median and mean will be very similar. However, if the data are skewed by extreme responses, then the median 
                      is likely more representative of the typical vessel.'),
                    tags$ul(
                      tags$li(strong("Per vessel or processor:"),'Median or Mean for all vessels or processors that participated in the catch share program.'),
                      tags$li(strong("Per vessel or processor per day:"), "Median or Mean per day for all vessels or processors that participated in the catch share program."),
                      tags$li(strong("Per vessel or processor per metric ton caught/purchased:"), "Median or Mean per metric ton of fish caught or purchased for all vessels or processors 
                              that participated in the catch share program."),
                      tags$li(strong("Per vessel or processor per metric ton produced:"), "Median or Mean per metric ton of fish products for all vessels or processors 
                              that participated in the catch share program.")
                       ), tags$br(),             
                    h4('Fleet-wide total'),
                   p("The fleet-wide total measures how the entire fleet is doing, rather than a representative vessel. Summed over all vessels or processors that 
                     participated in the catch share program. For rates measurements (per day at sea or metric ton), we show the fleet-wide or industry-wide average. 
                     The fleet-wide average is the economic value summed over all vessels or processors divided by days at sea or metric tons summed over all vessels or processors.")
                 )
)