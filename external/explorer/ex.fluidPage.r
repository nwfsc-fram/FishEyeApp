#this file handles content page organization for the explorer and is sourced to ui.r

fluidPage(theme="bootstrap.css",
          title = "Data Explorer",
          
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
                     #                      fluidRow(
                     #                        column(8, uiOutput("removeAK")
                     #                        )
                     #                      ),
                     wellPanelSub(
                       fluidRow(
                         column(8, uiOutput("topicSelect")
                         )
                       ),
                       #                      conditionalPanel(condition = "input.topicSelect=='Homeport' | input.topicSelect=='Delivery port'",
                       #                                       fluidRow(
                       #                                         column(6, uiOutput("placeUnit"), offset = 2
                       #                                         )
                       #                                       )
                       #                      ),
                       conditionalPanel(condition = "if(input.topicSelect)",
                                        fluidRow(
                                          column(12, uiOutput("topics")
                                          )
                                        )
                       )
#                        conditionalPanel(condition = "input.topicSelect=='Vessel length class'",
#                                         fluidRow(
#                                           column(12, uiOutput("length")
#                                           )
#                                         )
#                        ),
#                        conditionalPanel(condition = "input.topicSelect=='Homeport' || input.topicSelect=='State'",
#                                         fluidRow(
#                                           column(12, uiOutput("place")
#                                           )
#                                         )
#                        ),
                       #                      conditionalPanel(condition = "input.topicSelect=='Delivery port'",
                       #                                       fluidRow(
                       #                                         column(12, uiOutput("delivPort")
                       #                                         )
                       #                                       )
                       #                      ),
#                        conditionalPanel(condition = "input.topicSelect=='Cost type'",
#                                         fluidRow(
#                                           column(12, uiOutput("costtyp")
#                                           )
#                                         )
#                        )
                     ),
                     wellPanelSub(
                       fluidRow(
                         column(8, uiOutput("stat")
                         )
                       )
                     ),
                     #                      fluidRow(
                     #                        column(8, uiOutput("groupBy")
                     #                        )
                     #                      ),
                     #                      br(),
                     fluidRow(
                       column(8, uiOutput("dataButton")
                       )
                     )
                   ), #end well panel
                   conditionalPanel(condition = "if(input.dataButton > 0)",
                                    wellPanel(
                                      wellPanelSub(
                                        fluidRow(
                                          column(6, uiOutput("plotType")
                                          ),
#                                           conditionalPanel(condition = "input.plotType=='Bar'",
                                                           column(6, uiOutput("dodge")
                                                           )
#                                           )
                                        )
                                      ),                                                                       
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
                     tabPanel("Definitions", source("external/explorer/definitions.R"))
                   ) # end of tabsetPanel
            ) # end right side column
          ) #end the top level fluid row
) # end fluid Page
