#server
.libPaths(c("/usr/lib64/R/shiny_library", .libPaths()))
library(appFrame)
library(shiny)
library(ggplot2)
#library(reshape2)
library(grid)
#library(scales)

#options(error=browser) # debugging
source("external/serverHead.R")

shinyServer(
  function(input, output, session) {    
    source("external/explorer/explorer.R", local=T)    
  }
)
