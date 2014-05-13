fluidPage(
  title = "Data Explorer",
  
  fluidRow(
    column(4,
           wellPanel(
             wellPanel( # begin data subsetting well
               fluidRow(
                 column(6, uiOutput("dat.name")
                 ),
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
                 column(6, uiOutput("dataGo")
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
                 br(),
                 column(4,uiOutput("groupMean"),uiOutput("groupRange")
                 )
               )
             )
           )
    ), #start second half of page ie. plot/table output
    column(8, 
           fluidRow(
             column(12, plotOutput("plotTest")
             )
           ),
           fluidRow(
             column(12, tableOutput("tableTest") # testing
             )
           )
    )
  )
)