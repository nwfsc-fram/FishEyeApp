.libPaths(c("/usr/lib64/R/shiny_library", .libPaths()))

library(appFrame)

# custom css functions
# calls .css selector for well-sub
wellPanelSub <- function(...){div(class = "well-sub", ...)}
# calls .css selector for radioButton header
wellPanelHeading <- function(...){div(class = "well-radioHeading", ...)} 


fluidPage(title = "FISHEyE",
            # create a CSS to modify style of validation test error (for Variability analysis)
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-validation {
                            color: red;
                            font-size: 120%;
                            padding: 50px;
                            }
                            ")),
            tags$style(HTML(".select {margin-top:-20px}"),
                       tags$textarea(id="message", rows=3, cols=40, "Default value")),
          #  tags$style(HTML(".navbar .nav > li { position:relative; z-index: 10000;}")),
            tags$style(HTML(".navbar {position:static}")),
            tags$style(HTML(".ckbox {margin-top: 0px; margin-bottom: -15px}")),
            tags$style(HTML(".statbox {margin-top: -30px; margin-bottom: -15px}")),
            tags$style(HTML(".ckbox2 .checkbox:first-child label{font-weight:bold;}")),
            tags$style(HTML(".ckbox2 .checkbox:nth-child(2) label{font-weight:bold;}")),
            tags$style(HTML(".ckbox2 .checkbox:nth-child(8) label{font-weight:bold;}")),
            tags$style(HTML(".ckbox2 .checkbox:nth-child(3) label{margin-left:17px;}")),
            tags$style(HTML(".ckbox2 .checkbox:nth-child(4) label{margin-left:17px;}")),tags$style(HTML(".ckbox2 .checkbox:nth-child(5) label{margin-left:17px;}")),
            tags$style(HTML(".ckbox2 .checkbox:nth-child(6) label{margin-left:17px;}")),tags$style(HTML(".ckbox2 .checkbox:nth-child(7) label{margin-left:17px;}")),
            tags$style(HTML(".ckbox2 .checkbox:nth-child(9) label{margin-left:17px;}")),tags$style(HTML(".ckbox2 .checkbox:nth-child(10) label{margin-left:17px;}")),
            tags$style(HTML(".ckbox2 .checkbox:nth-child(11) label{margin-left:17px;}")),tags$style(HTML(".ckbox2 .checkbox:nth-child(12) label{margin-left:17px;}")),

            tags$style(HTML(".sectselect >li{position:relative; z-index:10000; background-color:black !important; display:inline-block; vertical-align:middle}")),
#            tags$style(HTML(".sectselect label{display:inline-block; vertical-align:middle; float:left; width:95%}")),
#            tags$style(HTML(".sectselect select{display:inline-block; vertical-align:middle; float:right; width:35%}")),
            tags$style(HTML(".sectselect{font-size:28px; line-height:28px;}")),
          
            tags$style(HTML(".actbutton {margin-bottom:5px}")),
            tags$style(HTML(".rbutton {margin-top:15px}")),
          
            tags$style(HTML(".rbutton .radio:nth-child(2) label{font-weight:bold")),
            tags$style(HTML(".rbutton .radio:nth-child(3) label{font-weight:bold")),
            tags$style(HTML(".rbutton .radio:nth-child(4) label{margin-left:17px;")), tags$style(HTML(".rbutton .radio:nth-child(5) label{margin-left:17px;")), tags$style(HTML(".rbutton .radio:nth-child(6) label{margin-left:17px;")),
           tags$style(HTML(".rbutton .radio:nth-child(7) label{margin-left:17px;")), tags$style(HTML(".rbutton .radio:nth-child(7) label{margin-left:17px;")),tags$style(HTML(".rbutton .radio:nth-child(8) label{margin-left:17px;")),
           tags$style(HTML(".rbutton .radio:nth-child(9) label{font-weight:bold")),
           tags$style(HTML(".rbutton .radio:nth-child(10) label{margin-left:17px;")), tags$style(HTML(".rbutton .radio:nth-child(11) label{margin-left:17px;")),tags$style(HTML(".rbutton .radio:nth-child(12) label{margin-left:17px;")),
           tags$style(HTML(".rbutton .radio:nth-child(13) label{margin-left:17px;")),
           tags$style(HTML(".rbutton .radio:nth-child(1) label{font-style:italic")),
           tags$style(HTML(".rbutton2 .radio:nth-child(1) label{font-style:italic")),
           tags$style(HTML('#iof{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}')),
           tags$style(HTML('#isummed{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}')),
           tags$style(HTML('#ifg{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}')),
           tags$style(HTML('#istat{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}')),
           tags$style(HTML('#ipo{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}')),
           tags$style(HTML('#ivs{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}')),
           tags$style(HTML('#iem{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}')),
           tags$style(HTML('#iVesSum{width:20px; height:19px; margin:0px;border:none; padding:0px;border-radius:25px;background-color:transparent;font-size:12px; color:RoyalBlue}'))),
# java script 
          tags$style(type='text/css', "#data2 { background-color:RoyalBlue; color:white; height:37px;position:absolute;bottom:170%;left:425%;}"),
          tags$style(type='text/css', "#data { background-color:RoyalBlue; color:white; height:37px;position:absolute;bottom:170%;left:425%;}"),
          
           tags$head(includeScript("google-analytics.js")),
           tags$head(tags$script(src = "message-handler.js")),
          
           tags$head(
            # Main css page, downloaded from bootswatch
            tags$link(rel="stylesheet", type="text/css", href="bootstrap.css"),
            # secondary css page with fisheye specific attributes
            tags$link(rel="stylesheet", type="text/css", href="fisheye.css"),
            tags$link(rel="stylesheet", href="http://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.4.0/css/font-awesome.min.css"),
            tags$style(type="text/css", ".tab-content {overflow: visible;}")         
            ),
          
         # source("www/shiny_framebuster/framebuster.R")$value,
          appFrameHeaderScrolling(),
          ## example R framebusting code
          fluidRow(div(style = "padding-bottom: 5px;margin-bottom:0"),
                   tags$h2(style = "margin-left: 15px", 
                     HTML("<div>
<p style='font-size:120%'><strong><a style='color:white; background-color:#1a5d99;  font-family:Cambria; border-radius:25px; padding:5px' href='https://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/'> FISHEyE</a>
                             - Performance Metrics</strong></p> 
                            
                          </div>")), uiOutput("SectorSelect")  
          ),# <p><i>West Coast Trawl Catch Share Program:</i></p>     
          navbarPage(id="page", collapsible=TRUE, inverse=F,
            title="",


          tabPanel("Explore the data", value="results",    
            sidebarLayout(
              mainPanel(         
                tabsetPanel(id = "tabs", 
                            tabPanel(title=HTML("Summary Plots <br> and Data"), value="Panel1",
                                     fluidRow(
                                       column(12, htmlOutput("DefaultPlotText")),                                
                                       column(2, uiOutput("DataButton")),  
                                     #  column(2, uiOutput("VCNRButton")), 
                                       column(12, dataTableOutput("TableMain"), plotOutput("PlotMain", height="auto",width="auto"))
                                     )),#end fluid row
                            tabPanel(title=HTML("<div> Fleet-wide <br> Variability Analysis</div>"),value="Panel2",
                                     fluidRow(
                                       column(12, htmlOutput("DefaultThirdsText")),                             
                                       column(2, uiOutput("DataButton2")),
                                       column(12, dataTableOutput("TableThirds"),plotOutput("PlotThirds", height="auto", width="auto")))))),
                            
                  sidebarPanel( 
                   wellPanel( 
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
                                uiOutput("VesSumSelect"),uiOutput("SelectText"),
                                 uiOutput("VariableSelect")
                               
                              )
                            
                       ),
                  
                      column(6,
                              wellPanelSub(
                                uiOutput("IndicatorSelect"),
                              conditionalPanel(condition="input.Ind_sel=='Economic'",
                                uiOutput("ShortdescrSelect"),
                                uiOutput("StatSelect"))
                              ),
                              wellPanelSub(
                                 uiOutput("YearSelect")#,
                               ),
                             
                      wellPanelSub(
                                uiOutput("FishAkSelect"),
                                uiOutput("FishWhitingSelect")
                              ) ), 
                        column(6,
                               wellPanel(
#                                 uiOutput("DodgeSelect"),
                                 uiOutput("PlotSelect"))
                               ),
                        column(4,
                                wellPanel(uiOutput("download_figure"),
                                #tags$br(),
                                 uiOutput("download_Table")#,
                              ))
                       ) #end Fluid row
            ),      style = "padding: 0px;border: 1px solid #000000;") # end left side column



)),
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
