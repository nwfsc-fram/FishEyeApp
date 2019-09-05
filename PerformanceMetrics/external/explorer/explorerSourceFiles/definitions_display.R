# Text for definitions page

tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         # HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
         #      <h3>Definitions</h3></div>",
         #      '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Definitions <br> in new browser tab</a>'
         # ),
                 tags$div(
                   h3("Display"),
                   h4("Select multiple metrics"),
                    p('FISHEyE lets you choose whether you would like to create multiple figures, one for each selected vessel/processor category or one for 
                      each selected metric. The default is to allow the user to choose multiple categories and one metric, if you would like to look at multiple 
                      metrics for one category, you can turn this option on with the Select multiple metrics slider:'),
                   tags$img(src='Display_option.jpg', height=50),
                   tags$br(),
                   tags$br(),
                   h4("Show variance"),
                   p("This functionality allows user to include a variance band on the plots. When plotting mean the variance band represents the mean plus or minus 
                     the variance. When plotting median the variance band represents the 25th and 75th quartiles.")
                 )
)
                    