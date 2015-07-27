.libPaths(c("/usr/lib64/R/shiny_library", .libPaths()))
library(appFrame)

# custom css functions
# calls .css selector for well-sub
wellPanelSub <- function(...){div(class = "well-sub", ...)}
# calls .css selector for radioButton header
wellPanelHeading <- function(...){div(class = "well-radioHeading", ...)} 


fluidPage(title = "FISHEyE",
          
          tags$head(
            # Main css page, downloaded from bootswatch
            tags$link(rel="stylesheet", type="text/css", href="bootstrap.css"),
            # secondary css page with fisheye specific attributes
            tags$link(rel="stylesheet", type="text/css", href="fisheye.css")
            ),
          
         # source("www/shiny_framebuster/framebuster.R")$value,
          appFrameHeaderScrolling(),
          ## example R framebusting code
          fluidRow(div(style = "padding-botttom: 15px;"),
                   tags$h2(style = "margin-left: 15px", 
                     HTML("<div>
                             <p style='font-size:120%'><strong>Net Revenue Explorer</strong></p> 
                             <p><i>West Coast Trawl Catch Share Program: Catcher Vessels</i></p>
                          </div>"))  
          ),
          fluidRow(
            column(4,
                   wellPanel( #left side panel                    
                      fluidRow(
                        column(12, HTML("<p><strong>Select data in each of the panels below: </strong></p>")
                        )
                     ),
                     fluidRow(
                       column(6,
                              wellPanelSub(
                                wellPanelHeading(
                                  uiOutput("CategorySelect")
                                ),
                                HTML("<div style='display:inline-block;width:100%; margin-top:10px'>
                                       <i>Select one or more of the following:</i>
                                      </div>"),
#                                 fluidRow((div(style="padding-left:120%;")),
                                uiOutput("VariableSelect")
#                                 )  
                              ),
                              conditionalPanel(condition = "input.CategorySelect != 'Fisheries'",
                                wellPanelSub(
                                  uiOutput("FisherySubsetSelect")
                                )
                              ),
                              wellPanelSub(
                                uiOutput("FishAkSelect")
                              )
                       ),
                       column(6,
                              wellPanelSub(
                                uiOutput("StatSelect")
                              ),
                              wellPanelSub(
                                 uiOutput("YearSelect")
                              ),
                              wellPanelSub(
                                 uiOutput("ShortdescrSelect")
                              ),
                              wellPanel( 
                                downloadButton("",# dlTable
                                               label = "Download table",  
                                               class = "btn btn-info"
                                             )
                                
                              ,
                              #wellPanel( 
                                downloadButton("", 
                                               label = "Download figure",
                                               class = "btn btn-info")
                                
                              )
                        )
                       )
                   ) #end well panel 
            ), # end left side column
            column(8,
                   tabsetPanel(
                     tabPanel(HTML("Summary <br>
                              Plot"),
                              fluidRow(
                                column(12, htmlOutput("DefaultPlotText")
                                ),                                
                                column(12, plotOutput("PlotMain", height = "auto", width="auto")                                  
                                )
                              ),
                              fluidRow(
                                column(12,
                                       wellPanel(
                                         fluidRow(HTML("<strong>Plot Options: </strong>"),
                                                  style = "padding-bottom: 15px;
                                                  padding-left: 15px"),
                                         fluidRow(                                  
                                           column(12,  
                                                column(4,
                                                    wellPanelSub(
                                                       fluidRow(
                                                         column(6, uiOutput("PlotSelect")
                                                         ),
                                                         column(6, uiOutput("DodgeSelect")
                                                         )
                                                         )
                                                         )
                                                  ),
                                                  column(2,
                                                         fluidRow(
                                                           downloadButton("dlPlotMain", 
                                                                          label = "Download plot",
                                                                          class = "btn btn-info")
                                                         )
                                                  )
                                           )
                                         )
                                       )
                                )
                              )
                     ),
                     tabPanel(HTML("Data <br>
                              Table"),                      
                              fluidRow(
                                column(12, htmlOutput("DefaultTableText")
                           #   )
                              ),
                           #   fluidRow(
                                 column(12, dataTableOutput("TableMain")
                                )
                              ),
                              fluidRow(
                                column(12,
                                #       wellPanel(
                                     #    fluidRow(HTML("<strong>Plot Options: </strong>"),
                                      #            style = "padding-bottom: 15px;
                                      #            padding-left: 15px" ),
                                         fluidRow(
                                           column(2,
                                              #    fluidRow(
                                                    downloadButton("dlTable", 
                                                                   label = "Download table",
                                                                   class = "btn btn-info")
                                                #  )
                                          # )
                                         )
                                       )                                     
                                )
                              )
                              #                 )
                     ),
                     tabPanel(HTML("Variability <br>
                                   Analysis"),
                              fluidRow(
                                column(12, htmlOutput("DefaultThirdsText")
                                )
                              ),                             
                              fluidRow(
                                column(12, plotOutput("PlotThirds", height = "auto", width="auto")
                                )  
                              ),
                              fluidRow(
                                column(12,
                                       wellPanel(
                                         fluidRow(HTML("<strong>Plot Options: </strong>"),
                                                  style = "padding-bottom: 15px;
                                                  padding-left: 15px" ),
                                         fluidRow(
                                           column(2,
                                                  fluidRow(
                                                    downloadButton("dlPlotThirds", 
                                                                   label = "Download plot",
                                                                   class = "btn btn-info")
                                                  )
                                           )
                                         )
                                       )
                                )
                              )
                     ),
                     tabPanel("Definitions", 
                              source("external/explorer/explorerSourceFiles/definitions.R")$value
                     ),
                     tabPanel("Instructions",
                              source("external/explorer/explorerSourceFiles/instructions.R")$value
                     ),
                     tabPanel("About",
                              source("external/explorer/explorerSourceFiles/about.R")$value
                     )
                   ) # end of tabsetPanel
            ) # end right side column     
          ), #end app level fluid row
          appFrameFooterScrolling()
) # end fluid Page

