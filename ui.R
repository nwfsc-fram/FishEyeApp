#UI

shinyUI(
  navbarPage(theme="bootstrap.css", #note that you need to change theme both here and in the fluidpage 
             title = "FISHeries Economics Explorer (FISHEyE)",             
             tabPanel("EDC Data Explorer", source("external/explorer/ex.fluidPage.r")$value, local=T),
             tabPanel("Net Rev Simulator", source("external/simulator/sim.fluidPage.r")$value, local=T),
             tabPanel("About", source("external/about.r")$value, local=T)
  ) 
)