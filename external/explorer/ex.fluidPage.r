#this file handles content page organization for the explorer and is sourced to ui.r

fluidPage(theme="bootstrap.css",
  title = "Data Explorer",
  
  fluidRow(
    column(4,
           wellPanel( #left side panel
             wellPanel( #data select panel
               fluidRow(
                 column(6, HTML("<p>Dataset:</p>
                                <p><strong>Catcher Vessels</strong></p>")
                 ),
                 column(6, uiOutput("dat.name")
                 )
               )
             ),
             wellPanel( # begin data subsetting well
               fluidRow(
                 column(6, uiOutput("subsetChoice")
                 )
               ),
               
               conditionalPanel(condition= "input.subsetChoice==true",
                                fluidRow(
                                  column(6, uiOutput("years")
                                  )
                                ),
                                fluidRow(
                                  column(12, uiOutput("fishery")
                                  )
                                ),
                                fluidRow(
                                  column(12, uiOutput("length")
                                  )
                                ),
                                fluidRow(
                                  column(6, uiOutput("place")
                                  ),
                                  column(6, uiOutput("placeUnit")
                                  )
                                ),
                                fluidRow(
                                  column(6, uiOutput("delivPort")
                                  ),
                                  column(6, uiOutput("delivUnit")
                                  )
                                ),
                                fluidRow(
                                  column(6, uiOutput("costtyp")
                                  )
                                ),
                                fluidRow(
                                  column(6, uiOutput("dataButton")
                                  )
                                )
               )
             ),           
             wellPanel( #begin plot options well
               fluidRow(
                 column(4, uiOutput("by.var")
                 ),
                 column(4, uiOutput("group.var")
                 ),
                 column(4, uiOutput("facet.var")
                 )
               ),
               fluidRow(
                 column(4, uiOutput("stat")
                 ),
                 column(4, uiOutput("plotType")
                 ),
                 column(2, uiOutput("dodge")
                 )                 
               ),
               fluidRow(
                 column(4,uiOutput("groupMean")
                 )
               ),
               fluidRow(
                 column(6, uiOutput("plotButton")
                 )
               )
             )
           )
    ), #start second half of page ie. plot/table output
    column(8, 
           fluidRow(
             column(12, plotOutput("plotTest",height="800px")
             )
           ),           
           fluidRow(
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
