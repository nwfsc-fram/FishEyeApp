# specify lib.loc for nwcshiny server or local machine
appFrame_lib_loc <- function(wd){
  if (grepl("shiny-server", wd)){
    library(appFrame, lib.loc = "/usr/lib64/R/shiny_library/")
  } else {
    library(appFrame)
  }      
}


appFrame_lib_loc(wd = getwd())
require(shiny)
require(ggplot2)
require(reshape2)
require(grid)
require(scales)
# require(ggthemes)

# waitingBlock <- fluidRow((div(style="display: inline-block; width: 1200; height: 700px")))

# custom css functions
wellPanelSub <- function(...){div(class = "well-sub", ...)} # calls .css selector for well-sub

# navbar UI
# shinyUI(
#   navbarPage(title = "FISHeries Economics Explorer (FISHEyE)",
#              header = appFrameHeaderScrolling(),                   
#              tabPanel(title = "Net Revenue", 
#                       source("external/explorer/ex.fluidPage.r", 
#                              local = TRUE)$value),
#              theme = "bootstrap_nwfsc.css",
#              footer = appFrameFooterScrolling()           
#   ) 
# )


fluidPage(title = "FISHEyE",
          theme = "bootstrap_nwfsc.css",
          source("/www/shiny_framebuster/framebuster.R")$value,
          appFrameHeaderScrolling(),
          ## example R framebusting code
          fluidRow(div(style="padding-botttom: 15px;"),
                   tags$h2(style = "margin-left: 15px", tags$strong("Net Revenue Explorer"))  
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
                                uiOutput("CategorySelect"),
                                uiOutput("VariableSelect")
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
                                column(12, plotOutput("PlotMain", height = "auto")                                  
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
                     tabPanel(HTML("Summary <br>
                              Table"),                      
                              fluidRow(
                                column(12, htmlOutput("DefaultTableText")
                                )
                              ),
                              fluidRow(
                                column(12, dataTableOutput("TableMain")
                                )
                              ),
                              fluidRow(
                                column(12,
                                       wellPanel(
                                         fluidRow(HTML("<strong>Plot Options: </strong>"),
                                                  style = "padding-bottom: 15px;
                                                  padding-left: 15px"
                                         ),
                                         fluidRow(
                                           column(2,
                                                  fluidRow(
                                                    downloadButton("dlTable", 
                                                                   label = "Download table",
                                                                   class = "btn btn-info")
                                                  )
                                           )
                                         )
                                       )                                     
                                )
                              )
                              #                 )
                     ),
                     tabPanel(HTML("Thirds <br>
                                   Analysis"),
                              fluidRow(
                                column(12, htmlOutput("DefaultThirdsText")
                                )
                              ),                             
                              fluidRow(
                                column(12, plotOutput("PlotThirds", height = "auto")
                                )  
                              ),
                              fluidRow(
                                column(12,
                                       wellPanel(
                                         fluidRow(HTML("<strong>Plot Options: </strong>"),
                                                  style = "padding-bottom: 15px;
                                                  padding-left: 15px"
                                         ),
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
                     #                      tabPanel("Help", 
                     #                               source("external/explorer/explorerSourceFiles/help.R")$value
                     #                      ),
                     tabPanel("Instructions",
                              source("external/explorer/explorerSourceFiles/instructions.r")$value
                     ),
                     tabPanel("About",
                              source("external/explorer/explorerSourceFiles/about.r")$value
                     )
                   ) # end of tabsetPanel
            ) # end right side column     
          ), #end app level fluid row
          appFrameFooterScrolling()
          # fluidRow(source("external/uiComponents/uiFooter.R", local = TRUE)$value)
) # end fluid Page

