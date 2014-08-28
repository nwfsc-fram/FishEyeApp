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
                     fluidRow(
                       column(8, uiOutput("years")
                       )
                     ),
                     fluidRow(
                       column(8, uiOutput("dat.name")
                       )                       
                     ),                     
                     fluidRow(
                       column(8, uiOutput("removeAK")
                       )
                     ),
                                      fluidRow(
                                        column(8, uiOutput("topicSelect")
                                        )
                                      ),
                     conditionalPanel(condition = "input.topicSelect=='Homeport' | input.topicSelect=='Delivery port'",
                                      fluidRow(
                                        column(8, uiOutput("placeUnit")
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
                     conditionalPanel(condition = "input.topicSelect=='Homeport'",
                                      fluidRow(
                                        column(12, uiOutput("place")
                                        )
                                      )
                     ),
                     conditionalPanel(condition = "input.topicSelect=='Delivery port'",
                                      fluidRow(
                                        column(12, uiOutput("delivPort")
                                        )
                                      )
                     ),
                     conditionalPanel(condition = "input.topicSelect=='Cost type'",
                                      fluidRow(
                                        column(12, uiOutput("costtyp")
                                        )
                                      )
                     ),
                     br(),
                     fluidRow(
                       column(8, uiOutput("dataButton")
                       )
                     )
                   ), #end well panel
                   wellPanel(
                     fluidRow(
                       column(6, uiOutput("plotType")
                       ),
                       column(6, uiOutput("dodge")
                       )
                     ),
                     fluidRow(
                       column(6,uiOutput("palette")
                       )
                     ),
                     fluidRow(
                       column(6, uiOutput("groupMean")
                       )
                     )                     
                   ) #end well panel
                ), #end left side column
            column(9, 
                   fluidRow(
                     column(12, plotOutput("plotTest",height="800px")
                     )
                   ),                   
                   fluidRow(HTML("<hr>")),
                   fluidRow(
                     column(12, dataTableOutput("tableTest") # testing
                     )
                   )
            ) # end right side column
          ) #end the top level fluid row
) # end fluid Page
