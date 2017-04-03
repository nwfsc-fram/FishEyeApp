#======================================
#
# this page  
#  1. 
#  2. 
#  3. 
#======================================



.libPaths(c("/usr/lib64/R/shiny_library", .libPaths()))
library(appFrame)
# custom css functions
# calls .css selector for well-sub
wellPanelSub <- function(...){div(class = "well-sub", ...)}
# calls .css selector for radioButton header
wellPanelHeading <- function(...){div(class = "well-radioHeading", ...)} 

fluidPage(
title = "FISHEyE",
          #stylize the appearance of different aspects of the website
tags$head(tags$body(includeHTML("google-analytics.noaa.js"))),
         tags$head(
            tags$style(HTML(".shiny-output-error-validation {color: red; font-size: 120%;}")), #validation error
           # modify spacing of labels for specified widgits
             tags$style(HTML(".select {margin-top:-20px}"),tags$textarea(id="message", rows=3, cols=40, "Default value")), 
           #Style appearance of checkboxes
             tags$style(HTML(".ckbox{margin-top: 0px; margin-bottom: -15px;padding-top:10px}" )),
           #Style appears of Cost category check boxes - indents, bolding, spacing
             tags$style(HTML(".statbox {margin-top: -30px; margin-bottom: 0px}
                              .statboxC .checkbox:first-child label, 
                                     .statboxCP .checkbox:first-child label,
                                     .statboxM  .checkbox:first-child label,
                                     .statboxF  .checkbox:first-child label,
                                     .statboxF  .checkbox:nth-child(10) label,
                                     .statboxC  .checkbox:nth-child(9) label,
                                     .statboxM  .checkbox:nth-child(8) label,
                                     .statboxCP .checkbox:nth-child(7) label,
                                     .ckbox2 .checkbox:first-child label,
                                     .ckbox2 .checkbox:nth-child(2) label,
                                     .ckbox2 .checkbox:nth-child(9) label{font-weight:bold;}
                              .statboxC .checkbox:nth-child(-n+12) label,
                                    .statboxF .checkbox:nth-child(-n+14) label,
                                    .statboxM .checkbox:nth-child(-n+12) label,
                                    .statboxCP .checkbox:nth-child(-n+11) label,
                                    .ckbox2 .checkbox:nth-child(-n+12) label,
                                   .frckbox .checkbox:nth-child(3) label,
                                   .frckbox .checkbox:nth-child(4) label{margin-left:17px;}
                              .statboxM .checkbox:nth-child(1) label,
                                    .statboxC .checkbox:nth-child(1) label,
                                    .statboxF .checkbox:nth-child(1) label,
                                    .statboxC .checkbox:nth-child(9) label,
                                    .statboxF .checkbox:nth-child(10) label,
                                    .statboxCP .checkbox:nth-child(1) label,
                                    .statboxCP .checkbox:nth-child(7) label,
                                    .statboxM .checkbox:nth-child(8) label,
                                    .ckbox2 .checkbox:nth-child(1) label,
                                    .ckbox2 .checkbox:nth-child(2) label,
                                    .ckbox2 .checkbox:nth-child(9) label{margin-left:0px;}"
            )),
           #Stylize sector select drop-down menu
            tags$style(HTML(".sectselect >li{position:relative; z-index:10000; background-color:black !important;display:inline; vertical-align:middle;}")),
          #Stylize variable select check box
                      
            #stylize information icons
            tags$style(HTML('#iof {width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #isummed  {width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #ifg {width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #istat      {width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #ivs     {width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #iem     {width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #iprod   {width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}'))
       ),#end tags head
          # stylize show data and show plots buttons
   
          tags$style(type='text/css', "#data2 { background-color:RoyalBlue; color:white; height:37px;position:absolute;bottom:170%;left:425%;}
                                       #data { background-color:RoyalBlue; color:white; height:37px;position:absolute;bottom:170%;left:425%;}"),
          
          #java script code to run google analytics and display messages for information icons. 
          tags$head(includeScript("google-analytics.js"),
                       tags$script(src = "message-handler.js")),
              
       #Add links to style sheets
          tags$head(
            # Main css page, downloaded from bootswatch
            tags$link(rel="stylesheet", type="text/css", href="bootstrap.css"),
            # secondary css page with fisheye specific attributes
            tags$link(rel="stylesheet", type="text/css", href="fisheye.css"),
            tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.4.0/css/font-awesome.min.css"),
            tags$style(type="text/css", ".tab-content {overflow: visible;}")         
          ),
         #   source("www/shiny_framebuster/framebuster.R")$value,
          appFrameHeaderScrolling(),
          ## example R framebusting code
         fluidRow(div(style = "padding-bottom: 5px;margin-bottom:0"),
                   tags$h2(style = "margin-left: 15px", 
                           HTML("<div>
                                   <p style='font-size:120%'><strong><a style='color:white; background-color:#1a5d99;  font-family:Cambria; border-radius:25px; padding:5px' href='https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/'> FISHEyE</a> 
                                        - Costs Explorer</strong></p> 
                                </div>")), htmlOutput("SectPrint")#,
                           ),
          navbarPage(id="page", collapsible=TRUE, inverse=F,
                     title="",
                     tabPanel("Explore the data", value="results",    
                              sidebarLayout(
                                mainPanel(         
                                  tabsetPanel(tabPanel(title=HTML("Summary Plots <br> and Data"), value="Panel1", padding='10px',
                                                       fluidRow(column(12,'',
                                                                       fluidRow(column(12,
                                                                                       conditionalPanel(condition="input.VariableSelect==''",
                                                                                                        tabsetPanel(tabPanel('Overview',
                                                       fluidRow(
                                                         column(12, htmlOutput("DefaultPlotText"))#,                                
                                                                                                        )),
                                                      tabPanel('Get started',
                                                               fluidRow(column(12, htmlOutput('GetStartedText')))))))))),
                                
                                                      conditionalPanel(condition="input.VariableSelect!=''",  fluidRow(
                                                        column(2, uiOutput("DataButton")),  
                                                        column(12, dataTableOutput("TableMain"), plotOutput("PlotMain", height="auto",width="auto"))
                                                      ))))),
                                sidebarPanel(
                                  wellPanel( #left side panel  
                                    tags$head(
                                      tags$style(type="text/css", ".well{border: 0px transparent; padding-right:0px; margin-right:0px}"
                                      )),
                                    
                                    fluidRow(
                                      column(12, HTML("<p style = 'font-size: 160%'><strong>Control Panel </strong></p> <p style='font-size: 110%'><strong>Make selections in each of the panels below </strong></p>"))#,
                                     ),#end fluidRow
                                    fluidRow(
                                      column(12,uiOutput("resetButton"),
                                             uiOutput('Button')
                                             )),
                                    fluidRow(
                                      column(12,
                                             uiOutput("Sectorselect"),
                                             style = "background:white; padding:0px;padding-bottom:-15px;margin-bottom:0px;margin-left:1px; border: 3px solid #D3D3D3;border-radius:10px;width:95%;font-size:110%")
                                      ),
                                    fluidRow(
                                      column(6,
                                             uiOutput("Categoryselect"), style = "background:white; padding: 0px;margin:0px; border: 1px solid #D3D3D3;border-radius:1px;"
                                             
                                      ),
                                      column(6, 
                                             uiOutput("FishWhitingselect")), style = "background:white; padding: 0px;margin-bottom:10px; border: 3px solid #D3D3D3;border-radius:10px;"
                                    ), 
                                    #end fluid row
                                    fluidRow(
                                      column(12,
                                             " ")),
                                    fluidRow(
                                      column(6,
                                               wellPanelSub(
                                               conditionalPanel(condition="input.Sect_sel=='FR'",  
                                                              uiOutput("Productionselect")),
                                               conditionalPanel(condition="input.Sect_sel=='CV'|input.Sect_sel=='FR'",uiOutput("SelectText")),
                                               uiOutput("Variableselect")
                                             ),
                                             
                                              conditionalPanel(condition="input.Sect_sel!='CV'",
                                                              wellPanelSub(uiOutput("Yearselect2"))),

                                             style = "padding-left:-15px;margin-left:-15px;margin-right:0px;padding-right:2px;",
                                
                                               wellPanel(uiOutput("download_figure"),
                                                     uiOutput("download_Table"),
                                                     style = "margin-top:30px;padding-left:25px;width:62%"#,
                                           )
                                          ),
                                      column(6,
                                            wellPanelSub(
                                               uiOutput("Statselect"),#uiOutput("CostCategoryselect"), 
                                               uiOutput("Shortdescrselect")#)
                                             ),
                                            conditionalPanel(condition="input.Sect_sel=='CV'", wellPanelSub(
                                               uiOutput("Yearselect")
                                             ),
                                             style = "padding-right:5px;margin-right:0px; padding-left:2px;,width:100%"
                                      )),
                                      column(6,
                                             wellPanel(uiOutput('Plotselect')
                                      ),
                                      style = "padding-right:-20px;margin-right:-55px; padding-left:2px;,width:102%")#,
                                     ) #end Fluid row
                                  )
                                  ,       style = "padding: 0px;border: 1px solid #000000;") # end left side column
                              )), #End explore the data
                     navbarMenu("Instructions",
                                tabPanel("About", 
                                         source("external/explorer/explorerSourceFiles/about.R")$value
                                ),
                                tabPanel("Instructions", 
                                         source("external/explorer/explorerSourceFiles/instructions.R")$value
                                ),
                                tabPanel("Definitions", 
                                         source("external/explorer/explorerSourceFiles/definitions.R")$value
                                )
                     ), # end right side column     
                     tabPanel("Bulletin Board", 
                              fluidRow(
                                column(12, htmlOutput("BlogText")),
                                column(5,  htmlOutput("BlogUpdates")),
                                column(1),
                                column(5,  htmlOutput("BlogResponses")))),
                     tabPanel("Contact us",
                              fluidRow(
                                column(12, htmlOutput("Email")))
                     ),
                     tabPanel("FISHEyE Applications",
                              fluidRow(
                                column(12, htmlOutput("ApplicationsText"))
                              )),
                     
                     tabPanel(HTML('<a class="btn btn-warning", href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/"
                                   style="height:37px;border-radius:25px;margin: -24px -50px; float:top;position:absolute;right:-100px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
                                   padding-bottom:10px"> FISHEyE Homepage</a>' ),style='width:1000px')
                            ), #end app level fluid row#, target="_blank"
          appFrameFooterScrolling()
                     ) # end fluid Page
