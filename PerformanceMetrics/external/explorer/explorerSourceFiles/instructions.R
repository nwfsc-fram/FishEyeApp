# Text for definitions page

tags$div(style = "margin: 15px 15px 30px; width: 60%",    
         # HTML("<div style='display:inline-block;width:100%;padding:0;line-height: 0.72em; margin-top:5px; margin-bottom:5px;'>
         #      <h3>Definitions</h3></div>",
         #      '<a class="btn btn-primary", href="Definitions.htm" target="_blank"  style="height:47px;margin: -54px 0px 0px 930px"> Open Definitions <br> in new browser tab</a>'
         # ),
                 tags$div(
                   h3("Instructions"),
                   p('FISHEyE is a data exploration tool that allows you to customize data visualization output.'),
                   p('There are six major customization features: (1) sector, (2) metric, (3) statistic, (4) participant category, (5) filters, and (6) display.'),
                    tags$ul(
                      tags$li(strong("Sector:"), 'Choose the sector of the fishery that you are interested in (Catcher vessels, Motherships, Catcher-processors, First receivers and shorebased processors).'),
                      tags$li(strong("Metric:"),'Choose the data that you would like to explore by choosing a metric category (vessel/processor characteristics, 
                              economic, labor, cost, impacts, other) and metric from the list.'),
                      tags$li(strong("Statistic:"),'Change how the data are summarized by selecting a statistic (mean, median, total). The Economic and Cost 
                              metrics also include the option to summarize as a rate (per day, per metric ton).'),
                      tags$li(strong("Participant category:"), 'Choose the category of vessels/processors that you would like to see.'),
                      tags$li(strong("Filters:"), "Customize the type of participants to include in the data summary (vessel/processor type, fisheries, location, size)."),
                      tags$li(strong("Display:"), "Customize how information is displayed by selecting the option to graph multiple metrics together and/or show variance bands in the plot.")
                       ), tags$br(),
                   p(tags$em('FISHEyE is pre-set to plot the total number of catcher-vessels for all fisheries and all types of vessels.')),
                   tags$br(),
                   tags$img(src='instructions_diagram.jpg', height=900),
                    h4('How to use FISHEyE'),
                   p("For a quick how-to we describe how to plot median total cost net revenue per vessel from all catch share fisheries for non-whiting vessels in Newport and Astoria."),
                   p("In order to use the data exploration tool make sure you are on the", tags$em("Explore the data"), "page."),
                    tags$ul(
                      tags$li("Select the Catcher-Vessel button on the top of control panel (the control panel is on the left-side of the screen)."),
                      tags$li("Select the Economic tab under the Metric dropdown."),
                      tags$li("Select the Total cost net revenue button."),
                      tags$li("Select Median from the dropdown under Statistic."),
                      tags$li("Select the Median per vessel button under the dropdown."),
                      tags$li("Continue to the Filter by: fisheries, location, size dropdown."),
                      tags$li("Select the Homeport tab."),
                      tags$li("Check the Newport and Astoria checkbox."),
                      tags$li("Continue to the Additional Filters dropdown."),
                      tags$li("Select the Non-whiting vessels checkbox under Vessel type."),
                      tags$li("Select All catch share fisheries under Fisheries.")
                    ),
                   tags$br(),
                   p("Other options"),
                   p("If you want to compare, for instance, Total cost net revenue and Variable cost net revenue for Newport you could choose", strong('Select multiple 
                     metrics'), "under the Display options. You can then return to the Metrics dropdown and check as many checkboxes as you would like to compare 
                     side-by-side. When you use the Select multiple metrics option, however, you will not be able to choose multiple groups of participants. 
                     A button to download the plot(s) and data table can be found at the bottom of the control panel. To switch between viewing plots and data table, 
                     use the button above the plot or data output.")
                 )
)
                      
                      
                   