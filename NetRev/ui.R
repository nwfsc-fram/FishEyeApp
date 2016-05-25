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
            tags$style(HTML(".ckbox {margin-top: 0px; margin-bottom: -15px}
                              .statbox {margin-top: -30px; margin-bottom: -15px}
                              .actbutton {margin-bottom:5px;}
                              .rbutton {margin-top:15px}")),
            
            # indenting and bolding fishery selections
            tags$style(HTML(".ckbox2 .checkbox:first-child label{font-weight:bold;}
                             .ckbox2 .checkbox:nth-child(2) label{font-weight:bold;}
                             .ckbox2 .checkbox:nth-child(3) label{margin-left:17px;}
                             .ckbox2 .checkbox:nth-child(4) label{margin-left:17px;}
                             .ckbox2 .checkbox:nth-child(5) label{margin-left:17px;}
                             .ckbox2 .checkbox:nth-child(6) label{margin-left:17px;}
                             .ckbox2 .checkbox:nth-child(7) label{margin-left:17px;}
                             .ckbox2 .checkbox:nth-child(9) label{font-weight:bold;}
                             .ckbox2 .checkbox:nth-child(8) label{margin-left:17px;}
                             .ckbox2 .checkbox:nth-child(10) label{margin-left:17px;}
                             .ckbox2 .checkbox:nth-child(11) label{margin-left:17px;}
                             .ckbox2 .checkbox:nth-child(12) label{margin-left:17px;}")),
            tags$style(HTML(".rbutton .radio:first-child label{font-style:italic;}
                             .rbutton .radio:nth-child(2) label{font-weight:bold;}
                             .rbutton .radio:nth-child(3) label{font-weight:bold;}
                             .rbutton .radio:nth-child(4) label{margin-left:17px;}
                             .rbutton .radio:nth-child(5) label{margin-left:17px;}
                             .rbutton .radio:nth-child(6) label{margin-left:17px;}
                             .rbutton .radio:nth-child(7) label{margin-left:17px;}
                             .rbutton .radio:nth-child(8) label{margin-left:17px;}
                             .rbutton .radio:nth-child(10) label{font-weight:bold;}
                             .rbutton .radio:nth-child(9) label{margin-left:17px;}
                             .rbutton .radio:nth-child(11) label{margin-left:17px;}
                             .rbutton .radio:nth-child(12) label{margin-left:17px;}
                             .rbutton .radio:nth-child(13) label{margin-left:17px;}"
                             )),
            
            
            #stylize information icons
            tags$style(HTML('#iof{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #isummed{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #ifg{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #istat{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #ipo{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #ivs{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}
                             #iem{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}'))),
          # stylize show data and show plots buttons
   
          tags$style(type='text/css', "#data2 { background-color:RoyalBlue; color:white; height:37px;position:absolute;bottom:170%;left:425%;}
                                       #data { background-color:RoyalBlue; color:white; height:37px;position:absolute;bottom:170%;left:425%;}"),
          
          #java script code to run google analytics and display messages for information icons. 
          tags$head(includeScript("google-analytics.js"),
                    
                    tags$script(src = "message-handler.js")),
              
     
          tags$head(
            # Main css page, downloaded from bootswatch
            tags$link(rel="stylesheet", type="text/css", href="bootstrap.css"),
            # secondary css page with fisheye specific attributes
            tags$link(rel="stylesheet", type="text/css", href="fisheye.css"),
            tags$link(rel="stylesheet", href="http://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.4.0/css/font-awesome.min.css"),
            tags$style(type="text/css", ".tab-content {overflow: visible;}")         
          ),
         #   source("www/shiny_framebuster/framebuster.R")$value,
          appFrameHeaderScrolling(),
          ## example R framebusting code
          fluidRow(div(style = "padding-bottom: 5px;margin-bottom:0"),
                   tags$h2(style = "margin-left: 15px", 
                           HTML("<div>
                                
                                <p style='font-size:120%'><strong><a style='color:white; background-color:#1a5d99;  font-family:Cambria; border-radius:25px; padding:5px' href='https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/'> FISHEyE</a> - Net Revenue Explorer</strong></p> 
                                <p><i>West Coast Trawl Catch Share Program: Catcher Vessels</i></p>
                                </div>"))  
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
                                  )
                                ), #end main panel
                                sidebarPanel( 
                                  wellPanel( #left side panel  
                                    tags$head(
                                      tags$style(type="text/css", ".well{border: 0px transparent;}"
                                      )),
                                    
                                    fluidRow(
                                      column(8, HTML("<p style = 'font-size: 160%'><strong>Control Panel </strong></p> <p style='font-size: 110%'><strong>Make selections in each of the panels below </strong></p>"))#,
                                     ),#end fluidRow
                                    fluidRow(
                                      column(6,
                                             uiOutput("resetButton"),
                                             uiOutput('Button'),
                                             wellPanelSub(
                                               wellPanelHeading(
                                                 uiOutput("CategorySelect")
                                               ),
                                               uiOutput("SelectText"),
                                               uiOutput("VariableSelect")
                                             ),
                                             #  conditionalPanel(condition = "input.CategorySelect != 'Fisheries'",
                                             #    wellPanelSub(
                                             #      uiOutput("FisherySubsetSelect")
                                             #   )
                                             #   ),
                                             wellPanelSub(
                                               uiOutput("FishAkSelect"),
                                               uiOutput("FishWhitingSelect")
                                               )
                                             ),
                                      column(6,
                                             wellPanelSub(
                                               uiOutput("StatSelect")
                                             ),
                                             wellPanelSub(
                                               uiOutput("YearSelect")
                                             ),
                                             conditionalPanel(condition ="input.tabs == 'Panel2'|| input.tabs== 'Panel1' & input.DodgeSelect == 'Economic measures side-by-side'",
                                                              wellPanelSub(
                                                                uiOutput("ShortdescrSelect")))
                                      ),
                                      column(6,
                                             wellPanel(
                                               uiOutput("DodgeSelect"),
                                               uiOutput("PlotSelect"))
                                      ),
                                      column(4,
                                             wellPanel(uiOutput("download_figure"),
                                                       uiOutput("download_Table")#,
                                              )
                                      )
                                    ) #end Fluid row
                                  )
                                  ,       style = "padding: 0px;border: 1px solid #000000;") # end left side column
                                
                                
                                
                              )),
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
                     
                     tabPanel(HTML('<a class="btn btn-warning", href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/"
                                   style="height:37px;border-radius:25px;margin: -24px -50px; float:top;position:absolute;right:-100px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;
                                   padding-bottom:10px"> FISHEyE Homepage</a>' ),style='width:1000px')
                     
                     ), #end app level fluid row#, target="_blank"
          appFrameFooterScrolling()
                     ) # end fluid Page
