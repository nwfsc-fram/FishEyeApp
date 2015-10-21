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
            tags$style(HTML(".ckbox {margin-top: 5px; margin-bottom: -15px}"))
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
          fluidRow(div(style = "padding-bottom: 15px;"),
                   tags$h2(style = "margin-left: 15px", 
                     HTML("<div>
                             <p style='font-size:120%'><strong>Net Revenue Explorer</strong></p> 
                             <p><i>West Coast Trawl Catch Share Program: Catcher Vessels</i></p>
                          </div>"))  
          ),
          navbarPage(id="page", collapsible=TRUE, inverse=FALSE,
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
                              #   column(4, bsAlert("alert")),
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
                     tabPanel(title=HTML("Variability <br> Analysis"),value="Panel2",
                              fluidRow(
                                column(12, htmlOutput("DefaultThirdsText")
                                )
                              ),                             
                              fluidRow(
                                  column(12, plotOutput("PlotThirds", height="auto", width="auto")
                                     
                                ) ) 
              
                     ))#)
                     ),
                
                  sidebarPanel( 
     #         column(12,
                   wellPanel( #left side panel                    
                      fluidRow(
                        column(8, HTML("<p style = 'font-size: 120%'><strong>Make selections in each of the panels below </strong></p>")
                        )
                     ),#end fluidRow
                     fluidRow(
                       column(6,
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
                                uiOutput("StatSelect")
                              ),
                              wellPanelSub(
                                 uiOutput("YearSelect")#,
                                # uiOutput("SelectTextYear")
                              ),
                              conditionalPanel(condition ="input.tabs == 'Panel2'|| input.tabs== 'Panel1' & input.DodgeSelect == 'Compare economic measures side-by-side'",
                              wellPanelSub(
                                 uiOutput("ShortdescrSelect")
                              ))
                              ),
                        column(6,
                               wellPanel(
                                 uiOutput("DodgeSelect"),
                                 uiOutput("PlotSelect"))
                               ),
                        column(4,
                                wellPanel(uiOutput("download_figure"),
                                tags$br(),
                                 uiOutput("download_Table")
                              )
                        )
                       ) #end Fluid row
                  # ) #end well panel 
            )
               ,       style = "padding: 0px;") # end left side column



)),
        navbarMenu("About, Instructions, Definitions",
          #HTML("<div> <p style= 'color:black; margin-bottom:-65px; margin-top:-15px; padding:19px; border-left: 1px solid black;border-right: 1px solid black; width:250px'>About, Instructions, Definitions</p></div>"),
                     tabPanel("About", value="Panel1", 
                              source("external/explorer/explorerSourceFiles/about.R")$value
                     ),
                     tabPanel("Instructions", value="Panel1", 
                              source("external/explorer/explorerSourceFiles/instructions.R")$value
                     ),
                     tabPanel("Definitions", value="Panel1", 
                              source("external/explorer/explorerSourceFiles/definitions.R")$value
                     )
            ), # end right side column     
          tabPanel("Contact us",
                   
                              textInput("from", "From:", value="From"),
                              textInput("to", "To:", value="nwfsc.fisheye@noaa.gov"),
                               textInput("subject", "Subject:", value="Subject"),
                   tags$style(type="text/css", "textarea {width:100%}"),
                   tags$textarea(id = 'message', placeholder = 'Write your message here. Please press "Send mail" only once.', rows = 8, ""),
                #   verbatimTextOutput("output_text")
                   
                               #aceEditor("message", "Body:", value="Function currently not working.  Do not use. 
                              ##           In future message will state: Write message here"),
                          #    textInput("message", "Body:", "type message here"),
                               actionButton("send",label = "Send mail")
                   ),
          tabPanel("Blog",
                    fluidRow(
                      column(12, htmlOutput("BlogText")))),
          tabPanel(HTML('<a href="http://devdataexplorer.nwfsc.noaa.gov/fisheye/"style="display: padding-bottom:100;margin: -30px -50px 0px 380px;color:blue"> 
                        Return to FISHEyE homepage </a>'))# style="height:40px;margin: -40px -20px 0px 50px; float:top; border:0"/>
#
                        
          ), #end app level fluid row#, target="_blank"
          appFrameFooterScrolling()
) # end fluid Page

