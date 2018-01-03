#server
#.libPaths(c("/usr/lib64/R/shiny_library", .libPaths()))
library(appFrame)
library(shiny)
library(ggplot2)
library(grid)
library(dplyr)
library(scales)

#options(error=browser) # debugging
source("external/serverHead.R")
options(shiny.sanitize.errors = FALSE)
#options(shiny.trace=FALSE)

###!! -- Need to change this on Definitions.R too -- !!###
currentyear <- 2016

shinyServer(
  function(input, output, session) {    
    source("external/explorer/explorer.R", local=T)    
  }
)
