
# custom css functions
wellPanelSub <- function(...){div(class = "well-sub", ...)} # calls .css selector for well-sub


# UI
# fluidPage(
#     source("external/uiHead.R", local = TRUE)$value,
#     source("external/explorer/ex.fluidPage.r", local = TRUE)$value
# )


# OG UI
shinyUI(
  navbarPage(title = "FISHeries Economics Explorer (FISHEyE)",
             header = source("external/uiComponents/uiHead.R", local = TRUE)$value,                   
             tabPanel(title = "Net Revenue", source("external/explorer/ex.fluidPage.r", local = TRUE)$value),
             tabPanel(title = "Costs", source("external/costs/costs.fluidPage.R", local = TRUE)$value),
             theme = "bootstrap_nwfsc.css",
             footer = source("external/uiComponents/uiFooter.R", local = TRUE)$value
             
  ) 
)