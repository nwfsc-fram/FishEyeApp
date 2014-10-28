library(shiny)
library(ggplot2)
library(reshape2)

years.list <- c("2009", "2010", "2011", "2012")
# load("data/tables.RData") #depricated

# load("data/tabs.out.RData") ## this is now the following datatables
load("data/netrevTabsMean.RData")
load("data/netrevTabsSum.RData")


# custom palettes
# for net rev figures
pal.netrev <- c("REV" = "#FB9A99", "FIXEDCOST" = "#A6CEE3","TOTALNETREV" = "#1F78B4", "VARCOST" = "#B2DF8A", "VARNETREV" = "#33A02C")

# for EDC fishery groups
  # run the following to find color codes for gg_theme functions
  # library(ggtheme) # library(scales)
  # show_col(stata_pal("s2color")(8))
pal.fisheries <- c("At-sea Pacific whiting" = "#1a476f", 
                  "Shoreside Pacific whiting" = "#90353b", 
                  "DTS trawl with trawl endorsement" = "55752f", 
                  "Non-whiting, non-DTS trawl with trawl endorsement" ="#e7e00",
                  "Groundfish fixed gear with trawl endorsement" = "#6e8e84",
#                   "Groundfish fixed gear with fixed gear endorsement" = "c10534",
                  "Crab" = "c10534"
                  "Other fisheries" = "#938dd2",
#                   "Alaska" = "#cac27e"
                  "Shrimp" = "#cac27e")