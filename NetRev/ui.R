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
             tags$style(HTML(".navbar {margin-top:40px; position:relative; padding-left:15px; padding-right:0;}")),
             tags$style(HTML(".ckbox {margin-top: 0px; margin-bottom: -15px;padding-top:10px}
                             .ckbox3 {margin-top: 0px; margin-bottom: -15px;padding-top:10px}
                             .ckbox3 .checkbox input[type='checkbox'] { width: 0;}
                             .ckbox3  .checkbox:nth-child(n) label{color:#5a5a5a;}
                              .statbox {margin-top: -30px; margin-bottom: -15px}
                              .actbutton {margin-bottom:5px;}
                              .rbutton {margin-top:15px}")),
           
            
           
           tags$style(HTML(".sectselect >li{position:relative; z-index:10000; background-color:black !important;display:inline; vertical-align:middle;}")),
       #    tags$style(HTML(".sectselect{font-size:28px; line-height:28px;}")),
           
            # indenting and bolding fishery selections
           tags$style(HTML(".rbutton .radio:first-child label,
                             .rbutton2 .radio:first-child label,
                             .frbutton .radio:first-child label{font-style:italic;}
                              
                             .ckbox2 .checkbox:nth-child(-n+2) label,
                             .ckbox2 .checkbox:nth-child(11) label,
                             .rbutton .radio:nth-child(2) label,
                             .rbutton .radio:nth-child(3) label,
                             .rbutton .radio:nth-child(11) label{font-weight:bold;}
                             
                             .rbutton .radio:nth-child(-n+15) label,
                             .frbutton .radio:nth-child(4) label,
                             .frbutton .radio:nth-child(5) label,
                             .ckbox2 .checkbox:nth-child(-n+14) label,
                             .frckbox .checkbox:nth-child(3) label,
                             .frckbox .checkbox:nth-child(4) label{margin-left:17px;}
            
                             .ckbox2 .checkbox:nth-child(4) label,
                             .ckbox2 .checkbox:nth-child(5) label,
                             .ckbox2 .checkbox:nth-child(7) label,
                             .ckbox2 .checkbox:nth-child(8) label,
                             .ckbox2 .checkbox:nth-child(9) label,
                             .rbutton .radio:nth-child(5) label,
                             .rbutton .radio:nth-child(6) label,
                             .rbutton .radio:nth-child(8) label,
                             .rbutton .radio:nth-child(9) label,
                             .rbutton .radio:nth-child(10) label{margin-left:34px;}
                            
                             .rbutton .radio:nth-child(-n+3) label,
                             .rbutton .radio:nth-child(11) label,
                             .ckbox2 .checkbox:nth-child(-n+2) label,
                             .ckbox2 .checkbox:nth-child(11) label{margin-left:0px;}")),
            
            #stylize information icons
            tags$style(HTML('#iof{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #isummed{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #ifg{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #istat{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #ipo{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #ivs{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #iem{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #iprod{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}'))
          ),#end tags head
          
          # stylize show data and show plots buttons
           tags$style(type='text/css', "#data2 { background-color:RoyalBlue; color:white; height:37px;position:absolute;bottom:110%;left:325%;}
                                       #data { background-color:RoyalBlue; color:white; height:37px;position:absolute;bottom:110%;left:325%;}"),
          
          #java script code to run google analytics and display messages for information icons. 
          tags$head(includeScript("google-analytics.js"),
          tags$script(src = "message-handler.js")),
              
     
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
                                        - Net Revenue Explorer</strong></p> 
                                </div>")), htmlOutput("SectPrint")#,
#                   uiOutput("SectorSelect") 
                           ),
          navbarPage(id="page", collapsible=TRUE, inverse=F,
                     title="",#HTML("<div> <p style='color:#2fa4e7'>_</p></div>"), #inverse=T, 
                     #  collapsible=T,
                     
                     
                     tabPanel("Explore the data", value="results",    
                              # fluidRow(
                              sidebarLayout(
                                mainPanel(         
                                  tabsetPanel(id = "tabs", 
                                              tabPanel(title=HTML("Summary Plots <br> and Data"), value="Panel1", 
                                                       fluidRow(
                                                         column(12, htmlOutput("DefaultPlotText")),                                
                                                         column(2, uiOutput("DataButton")),  
                                                         column(2, uiOutput("VCNRButton")), 
                                                         column(12, dataTableOutput("TableMain"), plotOutput("PlotMain", height="auto",width="auto"))
                                                       )),#end fluid row
                                              tabPanel(title=HTML("<div> Fleet-wide <br> Variability Analysis</div>"),value="Panel2",
                                                       fluidRow(
                                                         column(12, htmlOutput("DefaultThirdsText")),                             
                                                         column(2, uiOutput("DataButton2")),
                                                         column(12, dataTableOutput("TableThirds"),plotOutput("PlotThirds", height="auto", width="auto"))))#,
                                  ) #end id-tabs
                                ), #end main panel
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
                                             uiOutput("SectorSelect"),
                                             style = "background:white; padding:0px;padding-bottom:-15px;margin-bottom:0px;margin-left:1px; border: 3px solid #D3D3D3;border-radius:10px;width:95%;font-size:110%")
                                      ), 
                                    fluidRow(
                                      column(6,
                                               wellPanelSub(
                                               conditionalPanel(condition="input.Sect_sel=='FR'",  
                                                              uiOutput("Productionselect")),
                                               wellPanelHeading(
                                                 uiOutput("Categoryselect")
                                               ),
                                               conditionalPanel(condition="input.Sect_sel=='CV'|input.Sect_sel=='FR'",uiOutput("SelectText")),
                                               uiOutput("Variableselect")
                                             ),
                                             #  conditionalPanel(condition = "input.CategorySelect != 'Fisheries'",
                                             #    wellPanelSub(
                                             #      uiOutput("FisherySubsetSelect")
                                             #   )
                                             #   ),
                                             conditionalPanel(condition="input.Sect_sel=='CV'",
                                                              wellPanelSub(uiOutput("FishAkselect"),uiOutput("FishWhitingselect"))),
                                             
                                             conditionalPanel(condition="input.Sect_sel!='CV'",
                                                              wellPanelSub(uiOutput("Yearselect2"))),
                                                                                             #,
                                             #  conditionalPanel(condition="input.Sect_sel!='CV'",uiOutput("DodgeSelect"),uiOutput("PlotSelect"))
                                             #  ,
                                             style = "padding-left:-15px;margin-left:-15px;margin-right:0px;padding-right:2px;"
                                             ),
                                      column(6,
                                            wellPanelSub(
                                               uiOutput("Statselect"),
                                             #conditionalPanel(condition ="input.tabs == 'Panel2'|| input.tabs== 'Panel1' & input.DodgeSelect == 'Economic measures side-by-side'",
                                                               uiOutput("Shortdescrselect")#)
                                             ),
                                            conditionalPanel(condition="input.Sect_sel=='CV'", 
                                                              wellPanelSub(uiOutput("Yearselect")
                                             ),
                                             style = "padding-right:5px;margin-right:0px; padding-left:2px;,width:100%"
                                      )),
                                      column(6,
                                             wellPanel(
                                            #   conditionalPanel(condition="input.Sect_sel=='CV'", uiOutput('DodgeSelect2'), uiOutput('PlotSelect2'))
                                               uiOutput('Dodgeselect'), uiOutput('Plotselect')
                                      ),
                                      style = "padding-right:-20px;margin-right:-55px; padding-left:2px;,width:102%"),
                                      column(4,
                                             wellPanel(uiOutput("download_figure"),
                                                       uiOutput("download_Table"),
                                                       style = "padding:0px;,width:102%"#,
                                              ),style = "padding-left:5px;margin-top:-10px;padding-top:-20px;"
                                      )
                                    ) #end Fluid row
                                  )
                                  ,       style = "padding: 0px;border: 1px solid #000000;") # end left side column
                              )), #End explore the data
                     navbarMenu("Instructions",
                                #HTML("<div> <p style= 'color:black; margin-bottom:-65px; margin-top:-15px; padding:19px; border-left: 1px solid black;border-right: 1px solid black; width:250px'>About, Instructions, Definitions</p></div>"),
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
                              #The following code chunk that has been commented out is for referene purposes 
                              # The code chunk, with the mail library create an email form for users to use
                              #      textInput("from", "From:", value="From"),
                              #      textInput("to", "To:", value="nwfsc.fisheye@noaa.gov"),
                              #            textInput("subject", "Subject:", value="Subject"),
                              # tags$style(type="text/css", "textarea {width:100%}"),
                              # tags$textarea(id = 'message', placeholder = 'Write your message here. Please press "Send mail" only once.', rows = 8, ""),
                              #   verbatimTextOutput("output_text")
                              
                              #aceEditor("message", "Body:", value="Function currently not working.  Do not use. 
                              ##           In future message will state: Write message here"),
                              #    textInput("message", "Body:", "type message here"),
                              #      actionButton("send",label = "Send mail")
                     ),
                     # HTML('<a href="http://devdataexplorer.nwfsc.noaa.gov/fisheye/"style="display: padding-bottom:100;margin: -30px -50px 0px 300px;color:blue"> 
                     #Return to FISHEyE homepage </a>'))# style="height:40px;margin: -40px -20px 0px 50px; float:top; border:0"/>
                     tabPanel("FISHEyE Applications",
                              fluidRow(
                                column(12, htmlOutput("ApplicationsText"))
                              )),
                     
                     tabPanel(HTML('<a class="btn btn-warning", href="https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/"
                                   style="height:37px;border-radius:25px;margin: -24px -50px; float:top;position:absolute;right:-100px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
                                   padding-bottom:10px"> FISHEyE Homepage</a>' ),style='width:1000px')
                            ), #end app level fluid row#, target="_blank"
          appFrameFooterScrolling()
                     ) # end fluid Page
