library(shiny)
library(ggplot2)
library(reshape2)

years.list <- c("2009", "2010", "2011", "2012")
# load("data/tables.RData") #depricated
load("data/tabs.out.RData")

pal.netrev <- c("#FB9A99", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C")
