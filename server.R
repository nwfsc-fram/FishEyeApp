#server
.libPaths(c("/usr/lib64/R/shiny_library", .libPaths()))
# appFrame_lib_loc(wd = getwd())
library(appFrame)
library(shiny)
library(ggplot2)
library(reshape2)
library(grid)
library(scales)
library(DT,lib.loc="/usr/lib64/R/shiny_library") #THis may not be needed, depends on how modifying the table goes
library(shinyBS,lib.loc="/usr/lib64/R/shiny_library") # this package is used to modify the size of actionbuttons


#options(error=browser) # debugging
source("external/serverHead.R")


shinyServer(
  function(input, output, session) {    
    source("external/explorer/explorer.R", local = T)    
  }
)