#this file handles content page organization for the explorer and is sourced to ui.r

fluidPage(theme="bootstrap.css",
          title = "Data Explorer",
          
          fluidRow(
            column(4,
                   wellPanel( #left side panel
                     #              wellPanel( #data select panel
                     fluidRow(
                       column(6, HTML("<p>Dataset:</p>
                                <p><strong>Catcher Vessels</strong></p>")
                       )
                     ),
                     #              ),
                     #                fluidRow(
                     #                  column(6, uiOutput("subsetChoice")
                     #                  )
                     #                ),
                     
                     #                conditionalPanel(condition= "input.subsetChoice==true" #depcricated
                     
                     fluidRow(
                       column(6, uiOutput("years")
                       ),
                       column(6, uiOutput("dat.name")
                       )
                     ),
                     fluidRow(
                       column(6, uiOutput("topicSelect")
                       ),
                       column(6, uiOutput("placeUnit")
                       )
                     ),
#                      fluidRow(
#                        column(6, uiOutput("topicVars")
#                        )
#                      ),
                    conditionalPanel(condition = "input.topicSelect != 'Fisheries'",
                    fluidRow(
                      column(12, uiOutput("removeAK")
                      )
                    )
                    ),
                     conditionalPanel(condition = "input.topicSelect=='Fisheries'",
                     fluidRow(
                       column(12, uiOutput("fishery")
                       )
                     )
                     ),
                     conditionalPanel(condition = "input.topicSelect=='Vessel length class'",
                                      fluidRow(
                                        column(12, uiOutput("length")
                                        )
                                      )
                                      ),
                     conditionalPanel(condition = "input.topicSelect=='Home-port area'",
                     fluidRow(
                       column(6, uiOutput("place")
                       )
                     )
                     ),
#                        column(6, uiOutput("placeUnit") #moved up a few rows
#                        )
#                      ),
                     conditionalPanel(condition = "input.topicSelect=='Delivery-port area'",
                     fluidRow(
                       column(6, uiOutput("delivPort")
                       )
                     )
                     ),
#                        column(6, uiOutput("delivUnit") #moved up a few row and integrated with placeUnit
#                        )
#                      ),
                     conditionalPanel(condition = "input.topicSelect=='Cost type'",
                     fluidRow(
                       column(6, uiOutput("costtyp")
                       )
                     )
                     ),                     
                     fluidRow(
                       column(6, uiOutput("dataButton")
                       )
                     )
                     
                     
#                      wellPanel( #begin plot options well
#                        fluidRow(
#                          column(4, uiOutput("by.var")
#                          ),
#                          column(4, uiOutput("group.var")
#                          ),
#                          column(4, uiOutput("facet.var")
#                          )
#                        ),
#                        fluidRow(
#                          column(4, uiOutput("stat")
#                          ),                
#                        ),
#                        
#                        fluidRow(
#                          column(6, uiOutput("plotButton")
#                          )
#                        )
#                      )
                   )
            ), #start second half of page ie. plot/table output
            column(8, 
                   fluidRow(
                     column(12, plotOutput("plotTest",height="800px")
                     )
                   ),
                   fluidRow(
                     column(2, uiOutput("plotType")
                     ),
                     column(2, uiOutput("dodge")
                     ),
                     column(2,uiOutput("groupMean")
                     ),
                     column(2, uiOutput("palette")
                     )
                   ),
                   fluidRow(HTML("<hr>")),
                   fluidRow(
                     column(12, dataTableOutput("tableTest") # testing
                     )
                   )
            )
          )
)
