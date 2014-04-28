fluidPage(
  title = "Data Explorer",
  
  fluidRow(
    column(4,
           wellPanel(
             fluidRow(
               column(6, uiOutput("dat.name")
               ),
               column(6, uiOutput("fishery")
               )
             ),
             fluidRow(
               column(6, uiOutput("dataGo")
               )
             )
           )
    ),
    column(8
    )
  )
)