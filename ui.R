#UI

shinyUI(
  navbarPage(theme= "bootstrap.css",
             title = "West Coast Fisheries Economic Simulator",             
             tabPanel("EDC Data Explorer", source("external/explorer/ex.fluidPage.r")$value, local=T),
             tabPanel("EDC netRev Simulator", source("external/simulator/sim.fluidPage.r")$value, local=T),
             tabPanel("About", source("external/about.r")$value, local=T),
             inverse = TRUE
  ) 
)