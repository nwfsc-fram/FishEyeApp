
# custom css functions
source("external/uiHead.R")

#UI
shinyUI(
  navbarPage(title = "FISHeries Economics Explorer (FISHEyE)",
             tabPanel(title = "Home", source("external/about.r", local = TRUE)$value),
             navbarMenu(title = "Economic Indicators",                   
                        tabPanel(title = "Net Revenue", source("external/explorer/ex.fluidPage.r", local = TRUE)$value),
                        tabPanel(title = "Costs", source("external/costs/costs.fluidPage.R")$value)
             ),
             navbarMenu("Scenario Planning",
                        tabPanel("Net Rev Simulator", source("external/simulator/sim.fluidPage.r", local = TRUE)$value)             
             ),
             theme = "bootstrapFisheye.css", #note that you need to change theme both here and in the fluidpage
             footer = source("external/shinyFooter.R", local = TRUE)$value
  ) 
)