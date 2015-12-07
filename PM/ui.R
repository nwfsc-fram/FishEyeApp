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
           tags$style(HTML(".rbutton2 .radio:nth-child(1) label{font-style:italic"))
           ),
           tags$head(includeScript("google-analytics.js")),
           tags$head(tags$script(src = "message-handler.js")),
          
           tags$head(
            # Main css page, downloaded from bootswatch
            tags$link(rel="stylesheet", type="text/css", href="bootstrap.css"),
            # secondary css page with fisheye specific attributes
            tags$link(rel="stylesheet", type="text/css", href="fisheye.css"),
            tags$style(type="text/css", ".tab-content {overflow: visible;}")         
            ),
          
         # source("www/shiny_framebuster/framebuster.R")$value,
          appFrameHeaderScrolling(),
          ## example R framebusting code
          fluidRow(div(style = "padding-bottom: 5px;margin-bottom:0"),
                   tags$h2(style = "margin-left: 15px", 
                     HTML("<div>
                             <p style='font-size:120%'><strong>Performance Metrics</strong></p> 
                             <p><i>West Coast Trawl Catch Share Program:</i></p>     
                          </div>")), uiOutput("SectorSelect")  
          ),
          navbarPage(id="page", collapsible=TRUE, inverse=F,
            title="",#HTML("<div> <p style='color:#2fa4e7'>_</p></div>"), #inverse=T, 
          #  collapsible=T,

 
 
          
          tabPanel("Explore the data", value="results",    
         # fluidRow(
            sidebarLayout(
                 mainPanel(         
                  tabsetPanel(id = "tabs", 
                     tabPanel(title=HTML("Visualize Data <br> with Plots"), value="Panel1",
                              fluidRow(
                                column(12, htmlOutput("DefaultPlotText")
                                ),                                
                               column(2, uiOutput("VCNRButton")), #  column(4, bsAlert("alert")),
                                column(12, plotOutput("PlotMain", height="auto",width="auto")                                  
                                )
                               
                              )#, #end fluidRow
                             # fluidRow(
                               # column(12,
                                      # wellPanel(
                                        # fluidRow(HTML("<strong>Plot Options: </strong>"),
                                        #          style = "padding-bottom: 15px;
                                        #          padding-left: 15px"),
                                        #fluidRow(                                  
                                           #column(12,  
                                              #  column(5,
                                              #     wellPanelSub(
                                              #        fluidRow(
                                              #          column(3, uiOutput("PlotSelect")
                                              #          ),
                                              #          column(8, uiOutput("DodgeSelect")
                                              #          )))) #end column
                                       #  )
                                     #  )
                              # )
                           #  ) )
                      ),#end fluid row
                    
                     tabPanel(HTML("Data <br> Table"), value="Panel1",                    
                              fluidRow(
                                column(12, htmlOutput("DefaultTableText")
                              )
                              ),
                              fluidRow(
                                
                                 column(12, dataTableOutput("TableMain")
                                )
                     )),
                     tabPanel(" "),
                     tabPanel(title=HTML("Fleetwide <br> Variability Analysis"),value="Panel2",
                              fluidRow(
                                column(12, htmlOutput("DefaultThirdsText")
                                )
                              ),                             
                              fluidRow(
                                  column(12, plotOutput("PlotThirds", height="auto", width="auto")
                                     
                                ) ) ),
                     tabPanel(HTML("Data <br> Table"), value="Panel2",                    
                              fluidRow(
                                column(12, htmlOutput("DefaultTableTextThirds")
                                )
                              ),
                              fluidRow(
                                
                                column(12, dataTableOutput("TableThirds")
                                )
                              )#)
                     ))
                     ), #end main panel
                
                  sidebarPanel( 
     #         column(12,
                   wellPanel( #left side panel                    
                      fluidRow(
                        column(8, HTML("<p style = 'font-size: 120%'><strong>Make selections in each of the panels below </strong></p>"))#,
                       # column(2, uiOutput("resetButton")
                       # )
                     ),#end fluidRow
                     fluidRow(
                       column(6,
                              uiOutput("resetButton"),
                              wellPanelSub(
                                wellPanelHeading(
                                  uiOutput("CategorySelect")
                                ),
                                uiOutput("SelectText"),
                                
#                                 fluidRow((div(style="padding-left:120%;")),
                                     uiOutput("VariableSelect")
                              ),
                            #  conditionalPanel(condition = "input.CategorySelect != 'Fisheries'",
                            #    wellPanelSub(
                            #      uiOutput("FisherySubsetSelect")
                             #   )
                           #   ),
                              wellPanelSub(
                                uiOutput("FishAkSelect")
                              )
                       ),
                       column(6,
                              wellPanelSub(
                                uiOutput("VesSumSelect")
                              )),
                      column(6,
                              wellPanelSub(
                                uiOutput("IndicatorSelect"),
                              conditionalPanel(condition="input.Ind_sel=='Economic'",
                                uiOutput("ShortdescrSelect"),
                                uiOutput("StatSelect"))
                              ),
                              wellPanelSub(
                                 uiOutput("YearSelect")#,
                                # uiOutput("SelectTextYear")
                              )#,
                             # conditionalPanel(condition ="input.tabs == 'Panel2'|| input.tabs== 'Panel1' & input.DodgeSelect == 'Economic measures side-by-side'",
                              
                             # ))
                              ),
                        column(6,
                               wellPanel(
                                 uiOutput("DodgeSelect"),
                                 uiOutput("PlotSelect"))
                               ),
                        column(4,
                                wellPanel(uiOutput("download_figure"),
                                #tags$br(),
                                 uiOutput("download_Table")#,
                                #tags$br(),
                               # uiOutput("resetButton")
                                
                              )
                        )
                       ) #end Fluid row
                  # ) #end well panel 
            )
               ,       style = "padding: 0px;") # end left side column



)),
        navbarMenu("About, Instructions, Definitions",
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
          tabPanel(HTML('<a class="btn btn-warning", href="http://dataexplorer.northwestscience.fisheries.noaa.gov/fisheye/"style="height:37px;margin: -24px -50px; float:top;position:absolute;right:-200px;font-familiy: Arial, Helvetica, sans-serif;font-size: 12pt; padding-top:7px;padding-bottom:10px"> FISHEyE Homepage</a>'))
          # HTML('<a href="http://devdataexplorer.nwfsc.noaa.gov/fisheye/"style="display: padding-bottom:100;margin: -30px -50px 0px 300px;color:blue"> 
          #Return to FISHEyE homepage </a>'))# style="height:40px;margin: -40px -20px 0px 50px; float:top; border:0"/>

                        
          ), #end app level fluid row#, target="_blank"
          appFrameFooterScrolling()
) # end fluid Page
