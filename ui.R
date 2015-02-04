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
fluidPage(
  appFrameHeaderScrolling(),  
  fluidRow(
    column(2,
           wellPanel( #left side panel                    
             fluidRow(
               column(3, HTML("<p>Dataset:</p>
                 <p><strong>Catcher Vessels</strong></p>")
               )
             ),
             wellPanelSub(                       
               fluidRow(
                 column(8, uiOutput("YearSelect")
                 )
               )
             ),
             wellPanelSub(
               fluidRow(
                 column(8, uiOutput("ShortdescrSelect")
                 )
               ) 
             ),                     
             wellPanelSub(
               fluidRow(
                 column(8, uiOutput("CategorySelect")
                 )
               ),
#                conditionalPanel(condition = "if(input.CategorySelect)",
                                fluidRow(
                                  column(12, uiOutput("VariableSelect")
                                  )
#                                 )
               )
             ),
             wellPanelSub(
               fluidRow( 
                 column(8, uiOutput("FishAkSelect"))
               ),
               fluidRow(
                 column(8, uiOutput("StatSelect")
                 )
               )
             ),
             wellPanelSub(
               fluidRow(
                 column(6, uiOutput("PlotSelect")
                 ),
                 column(6, uiOutput("DodgeSelect")
                 )
               )
             ),
             fluidRow(
               column(8, uiOutput("DataButton")
               )
             )
           ), #end well panel
           conditionalPanel(condition = "if(input.dataButton > 0)",
              wellPanel(                                                                       
                fluidRow(
                  column(6, downloadButton("dlPlot", label = "Download plot")
                  ),
                  column(6, downloadButton("dlTable", label = "Download table")
                  )
                ) 
              )# end well panel
           ) 
    ), # end left side column
    column(9,
           tabsetPanel(
             tabPanel("Output",
                      fluidRow(
                        column(12, plotOutput("PlotMain",height="800px")
                        )
                      ),                   
                      fluidRow(HTML("<hr>")),
                      fluidRow(
                        column(12, dataTableOutput("TableMain") # testing
                        )
                      )
             ),
             tabPanel("Definitions", source("external/explorer/definitions.R")$value)
           ) # end of tabsetPanel
    ) # end right side column
  ), #end app level fluid row
  appFrameFooterScrolling()
  # fluidRow(source("external/uiComponents/uiFooter.R", local = TRUE)$value)
) # end fluid Page

