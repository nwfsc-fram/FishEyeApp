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
                            }
                            "))
            ),
          
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
                              )),
                        column(4,
                                wellPanel(uiOutput("download_figure"),
                                tags$br(),
                                 uiOutput("download_Table")
                              )
                        )
                       ) #end Fluid row
                   ) #end well panel 
            ), # end left side column
            column(8,
                   tabsetPanel(id = "tabs",
                     tabPanel(title=HTML("Visualize <br> Data"), value="Panel1",
                              fluidRow(
                                column(12, htmlOutput("DefaultPlotText")
                                ),                                
                                column(12, plotOutput("PlotMain", height="auto",width="auto")                                  
                                )
                              ), #end fluidRow
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
                                           )
                                         )
                                       )
                                )
                              )
                     ) ),#end fluid row
                    
                     tabPanel(HTML("Data <br>
                              Table"), value="Panel1",                    
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
              
                     ),
                     tabPanel("Definitions", value="Panel1", 
                              source("external/explorer/explorerSourceFiles/definitions.R")$value
                     ),
                     tabPanel("Instructions", value="Panel1", 
                              source("external/explorer/explorerSourceFiles/instructions.R")$value
                     ),
                     tabPanel("About", value="Panel1", 
                              source("external/explorer/explorerSourceFiles/about.R")$value
                     )
                   ) # end of tabsetPanel
            ) # end right side column     
          ), #end app level fluid row
          appFrameFooterScrolling()
) # end fluid Page

