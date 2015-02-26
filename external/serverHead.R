years.list <- c("2009", "2010", "2011", "2012")
# load("data/tables.RData") #depricated

# load("data/tabs.out.RData") ## this is now the following datatables
load("data/netrevTable.RData")
load("data/factorOrder.RData")
load("data/netrevThirds.RData")

# custom palettes
# for net rev figures
pal.netrev <- c("Revenue" = "#66a61e", "Fixed costs" = "#e41a1c",
  "Total cost net revenue" = "#7570b3", 
  "Variable costs" = "#d95f02", "Variable cost net revenue" = "#1b9e77")

pal.thirds <- c("#1b9e77", "#d95f02", "#7570b3")


# DEPRICATED -- showing categories as facets, not groups
# for EDC fishery groups
    # library(ggtheme) # library(scales)
  # show_col(stata_pal("s2color")(8))
# pal.fisheries <- c("At-sea Pacific whiting" = "#1a476f", 
#                   "Shoreside Pacific whiting" = "#90353b", 
#                   "DTS trawl with trawl endorsement" = "#55752f", 
#                   "Non-whiting, non-DTS trawl with trawl endorsement" ="#e37e00",
#                   "Groundfish fixed gear with trawl endorsement" = "#6e8e84",
# #                   "Groundfish fixed gear with fixed gear endorsement" = "c10534",
#                   "Crab" = "#c10534",
#                   "Other fisheries" = "#938dd2",
# #                   "Alaska" = "#cac27e"
#                   "Shrimp" = "#cac27e")
