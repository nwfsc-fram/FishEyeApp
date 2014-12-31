
# custom css functions
wellPanelSub <- function(...){div(class = "well-sub", ...)} # calls .css selector for well-sub

# OG UI
# shinyUI(
#   navbarPage(title = "FISHeries Economics Explorer (FISHEyE)",
#              header = source("external/uiComponents/uiHead.R", local = TRUE)$value,                   
#              tabPanel(title = "Net Revenue", source("external/explorer/ex.fluidPage.r", local = TRUE)$value),
#              tabPanel(title = "Costs", source("external/costs/costs.fluidPage.R", local = TRUE)$value),
#              theme = "bootstrap_nwfsc.css",
#              footer = source("external/uiComponentsShin/uiFooter.R", local = TRUE)$value
#              
#   ) 
# )

#this file handles content page organization for the explorer and is sourced to ui.r


fluidPage(
  tags$div(source("external/uiComponents/uiHead.R", local = TRUE)$value),
  includeCSS("www/bootstrap_nwfsc.css"),  
  fluidRow(
    column(3,
           wellPanel( #left side panel                    
             fluidRow(
               column(8, HTML("<p>Dataset:</p>
                              <p><strong>Catcher Vessels</strong></p>")
               )
               ),
             wellPanelSub(                       
               fluidRow(
                 column(8, uiOutput("years")
                 )
               )
             ),
             wellPanelSub(
               fluidRow(
                 column(8, uiOutput("dat.name")
                 )
               ) 
             ),                     
             wellPanelSub(
               fluidRow(
                 column(8, uiOutput("topicSelect")
                 )
               ),
               conditionalPanel(condition = "if(input.topicSelect)",
                                fluidRow(
                                  column(12, uiOutput("topics")
                                  )
                                )
               )
             ),
             wellPanelSub(
               fluidRow(
                 column(8, uiOutput("stat")
                 )
               )
             ),
             wellPanelSub(
               fluidRow(
                 column(6, uiOutput("plotType")
                 ),
                 column(6, uiOutput("dodge")
                 )
               )
             ),
             fluidRow(
               column(8, uiOutput("dataButton")
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
                      column(12, plotOutput("plotTest",height="800px")
                      )
                    ),                   
                    fluidRow(HTML("<hr>")),
                    fluidRow(
                      column(12, dataTableOutput("tableTest") # testing
                      )
                    )
           ),
           tabPanel("Definitions", source("external/explorer/definitions.R")$value)
         ) # end of tabsetPanel
  ) # end right side column
), #end app level fluid row
fluidRow(source("external/uiComponents/uiFooter.R", local = TRUE)$value)
) # end fluid Page
