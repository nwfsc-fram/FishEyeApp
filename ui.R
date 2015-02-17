# specify lib.loc for nwcshiny server or local machine
appFrame_lib_loc <- function(wd){
  if (grepl("shiny-server", wd)){
    library(appFrame, lib.loc = "/usr/lib64/R/shiny_library/")
  } else {
    library(appFrame)
  }      
}


appFrame_lib_loc(getwd())
require(shiny)
require(ggplot2)
require(reshape2)
require(grid)
require(scales)
# require(ggthemes)


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
          source("www/shiny_framebuster/framebuster.R")$value,
          appFrameHeaderScrolling(),
          ## example R framebusting code
          fluidRow(div(style="paddign-botttom: 15px;"),
              tags$h2(style = "margin-left: 15px", tags$strong("Net Revenue Explorer"))  
          ),
          fluidRow(
            column(2,
                   wellPanel( #left side panel                    
                     fluidRow(
                       column(12, HTML("<p><strong>Select Data: </strong></p>")
                       )
                     ),
                     wellPanelSub(                       
                       fluidRow(
                         column(12, uiOutput("YearSelect")
                         )
                       )
                     ),                     
                     wellPanelSub(
                       fluidRow(
                         column(12, uiOutput("CategorySelect")
                         )
                       ),
                       fluidRow(
                         column(12, uiOutput("VariableSelect")
                         )
                       )
                     ),
                     wellPanelSub(
                       fluidRow(
                         column(12, uiOutput("ShortdescrSelect") 
                         )
                       )
                     ),
                     wellPanelSub(
                       fluidRow( 
                         column(12, uiOutput("FishAkSelect")
                         )
                       )
                      ),
                      wellPanelSub(
                       fluidRow(
                         column(12, uiOutput("StatSelect")
                         )
                       )
                      
                     )
                     #              fluidRow(
                     #                column(12, uiOutput("DataButton")
                     #                )
                     #              )
                   ) #end well panel 
            ), # end left side column
            column(9,
                   tabsetPanel(
                     tabPanel("Plot",
                              fluidRow(
                                column(12, htmlOutput("DefaultPlotText")
                                ),
                                column(12, plotOutput("PlotMain", height = "auto")
                                )
                              ),
#                              conditionalPanel(condition = "PermitPlot == true",
                              wellPanel(
                                fluidRow(HTML("<strong>Plot Options: </strong>"),
                                  column(12,
                                        column(2,
                                               
                                                  column(11,
                                                         
                                                       fluidRow(
                                                         uiOutput("PlotSelect")
                                                       )
                                                         
                                                
                                                ),
                                                column(11,
                                                       fluidRow(
                                                         uiOutput("DodgeSelect")
                                                       )
                                                )
                                          
                                         ),
                                         column(2,
                                                fluidRow(
                                                  downloadButton("dlPlotMain", 
                                                    label = "Download plot ")
                                                ),
                                                fluidRow(
                                                  column(12, div(style="height:10px")
                                                  )
                                                )
                                         )
                                  )
                                )
#                               )
                            )
                     ),
                     tabPanel("Table",                      
                              fluidRow(
                                column(12, htmlOutput("DefaultTableText")
                                ),
                                column(12, div(style="height:10px")
                                )
                              ),
                              fluidRow(
                                column(12, dataTableOutput("TableMain")
                                )
                              ),
                              #                conditionalPanel(condition = "input.DataButton > 0",
                              wellPanel(
                                fluidRow(
                                  column(12,
                                         fluidRow(
                                           downloadButton("dlTable", label = "Download table")
                                         )
                                  )
                                )
                              )
                              #                 )
                     ),
                     tabPanel("Definitions", 
                              source("external/explorer/explorerSourceFiles/definitions.R")$value
                     ),
#                      tabPanel("Help", 
#                               source("external/explorer/explorerSourceFiles/help.R")$value
#                      ),
                     tabPanel("About",
                              source("external/explorer/explorerSourceFiles/about.r")$value
                     )
                   ) # end of tabsetPanel
            ) # end right side column     
          ), #end app level fluid row
          appFrameFooterScrolling()
          # fluidRow(source("external/uiComponents/uiFooter.R", local = TRUE)$value)
) # end fluid Page

