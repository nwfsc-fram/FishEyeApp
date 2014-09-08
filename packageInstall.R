# Run the following code the first time you load up the app

list.of.packages <- c("ggplot2", "shiny", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

################################################
# To run app, ctrl + enter the following lines
################################################

#Todd's Laptop:
setwd("C:/todd.lee/Desktop/FISHEYE/vesselSim_app/")
shiny::runApp()

