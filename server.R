#server
.libPaths(c("/usr/lib64/R/shiny_library", .libPaths()))
# appFrame_lib_loc(wd = getwd())
library(appFrame)
library(shiny)
library(ggplot2)
library(reshape2)
#library(gridExtra)
#library(grid)
library(scales)
#library(DT) #This package is good for filtering data but not compatible with current version of shiny on the server
library(shinyBS) # this package is used to modify the size of actionbuttons
#library(mail) #for sending email


#options(error=browser) # debugging
source("external/serverHead.R")


shinyServer(
  function(input, output, session) {    
    source("external/explorer/explorer.R", local=T)    
  }
)
