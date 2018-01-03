#======================================
#
# this page  
#  1. 
#  2. 
#  3. 
#======================================

#server
.libPaths(c("/usr/lib64/R/shiny_library", .libPaths()))
# appFrame_lib_loc(wd = getwd())
library(appFrame)
library(shiny)
library(ggplot2)
library(reshape2)
library(grid)
#library(scales)
library(dplyr)
#library(mail) #for sending email

options(shiny.sanitize.errors = FALSE)
options(shiny.trace=FALSE)
#options(error=browser) # debugging

###!! -- Need to change this on Definitions.R too -- !!###
currentyear <- 2016

source("external/serverHead.R")

shinyServer(
  function(input, output, session) {    
    source("external/explorer/explorer.R", local=T)  
     })

